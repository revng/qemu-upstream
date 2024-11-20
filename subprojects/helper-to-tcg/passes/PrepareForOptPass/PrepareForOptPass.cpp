//
//  Copyright(c) 2024 rev.ng Labs Srl. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, see <http://www.gnu.org/licenses/>.
//

#include <PrepareForOptPass.h>
#include <Error.h>
#include <FunctionAnnotation.h>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>

#include <queue>
#include <set>

using namespace llvm;

static Expected<Annotation> parseAnnotationStr(StringRef Str,
                                               uint32_t num_function_args)
{
    Annotation Ann;

    Str = Str.trim();

    if (Str.consume_front("helper-to-tcg")) {
        Ann.Kind = AnnotationKind::HelperToTcg;
        // Early return, no additional info to parse from annotation string
        return Ann;
    } else if (Str.consume_front("immediate")) {
        Ann.Kind = AnnotationKind::Immediate;
    } else if (Str.consume_front("ptr-to-offset")) {
        Ann.Kind = AnnotationKind::PtrToOffset;
    } else {
        return mkError("Unknown annotation");
    }

    // Parse comma separated list of argument indices

    if (!Str.consume_front(":")) {
        return mkError("Expected \":\"");
    }

    Str = Str.ltrim(' ');
    do {
        Str = Str.ltrim(' ');
        uint32_t i = 0;
        Str.consumeInteger(10, i);
        if (i >= num_function_args) {
            return mkError("Annotation has out of bounds argument index");
        }
        Ann.ArgIndices.push_back(i);
    } while (Str.consume_front(","));

    return Ann;
}

static void collectAnnotations(Module &M, AnnotationMapTy &ResultAnnotations)
{
    // cast over dyn_cast is being used here to
    // assert that the structure of
    //
    //     llvm.global.annotation
    //
    // is what we expect.

    GlobalVariable *GA = M.getGlobalVariable("llvm.global.annotations");
    if (!GA) {
        return;
    }

    // Get the metadata which is stored in the first op
    auto *CA = cast<ConstantArray>(GA->getOperand(0));
    // Loop over metadata
    for (Value *CAOp : CA->operands()) {
        auto *Struct = cast<ConstantStruct>(CAOp);
        assert(Struct->getNumOperands() >= 2);
        Constant *UseOfF = Struct->getOperand(0);
        if (isa<UndefValue>(UseOfF)) {
            continue;
        }
        auto *F = cast<Function>(UseOfF->getOperand(0));
        auto *AnnVar =
            cast<GlobalVariable>(Struct->getOperand(1)->getOperand(0));
        auto *AnnData = cast<ConstantDataArray>(AnnVar->getOperand(0));

        StringRef AnnStr = AnnData->getAsString();
        AnnStr = AnnStr.substr(0, AnnStr.size() - 1);
        Expected<Annotation> Ann = parseAnnotationStr(AnnStr, F->arg_size());
        if (!Ann) {
            dbgs() << "Failed to parse annotation: \"" << Ann.takeError()
                   << "\" for function " << F->getName() << "\n";
            continue;
        }
        ResultAnnotations[F].push_back(*Ann);
    }
}

inline bool hasValidReturnTy(const Module &M, const Function *F)
{
    Type *RetTy = F->getReturnType();
    return RetTy == Type::getVoidTy(F->getContext()) ||
           RetTy == Type::getInt8Ty(M.getContext()) ||
           RetTy == Type::getInt16Ty(M.getContext()) ||
           RetTy == Type::getInt32Ty(M.getContext()) ||
           RetTy == Type::getInt64Ty(M.getContext());
}

// Functions that should be removed:
//   - No helper-to-tcg annotation (if TranslateAllHelpers == false);
//   - Invalid (non-integer/void) return type
static bool shouldRemoveFunction(const Module &M, const Function &F,
                                 const AnnotationMapTy &AnnotationMap,
                                 bool TranslateAllHelpers)
{
    if (F.isDeclaration()) {
        return false;
    }

    if (!hasValidReturnTy(M, &F)) {
        return true;
    }

    auto hasCorrectAnnotation = [](const Annotation &Ann) {
        return Ann.Kind == AnnotationKind::HelperToTcg;
    };

    std::queue<const Function *> Worklist;
    std::set<const Function *> Visited;
    Worklist.push(&F);
    while (!Worklist.empty()) {
        const Function *F = Worklist.front();
        Worklist.pop();
        if (F->isDeclaration() or Visited.find(F) != Visited.end()) {
            continue;
        }
        Visited.insert(F);

        // Check for llvm-to-tcg annotation
        if (TranslateAllHelpers and F->getName().startswith("helper_")) {
            return false;
        } else {
            auto It = AnnotationMap.find(F);
            if (It != AnnotationMap.end()) {
                const auto &AnnotationVec = It->second;
                auto Res = find_if(AnnotationVec, hasCorrectAnnotation);
                if (Res != AnnotationVec.end()) {
                    return false;
                }
            }
        }

        // Push functions that call F to the worklist, this way we retain
        // functions that are being called by functions with the llvm-to-tcg
        // annotation.
        for (const User *U : F->users()) {
            auto Call = dyn_cast<CallInst>(U);
            if (!Call) {
                continue;
            }
            const Function *ParentF = Call->getParent()->getParent();
            Worklist.push(ParentF);
        }
    }

    return true;
}

static void cullUnusedFunctions(Module &M, AnnotationMapTy &Annotations,
                                bool TranslateAllHelpers)
{
    SmallVector<Function *, 16> FunctionsToRemove;
    for (auto &F : M) {
        if (shouldRemoveFunction(M, F, Annotations, TranslateAllHelpers)) {
            FunctionsToRemove.push_back(&F);
        }
    }

    for (Function *F : FunctionsToRemove) {
        Annotations.erase(F);
        F->setComdat(nullptr);
        F->deleteBody();
    }
}

struct RetAddrReplaceInfo {
    User *Parent;
    unsigned OpIndex;
    Type *Ty;
};

static void replaceRetaddrWithUndef(Module &M)
{
    // Replace uses of llvm.returnaddress arguments to cpu_ld* w. undef,
    // and let optimizations remove it.  Needed as llvm.returnaddress is
    // not reprensentable in TCG.
    SmallVector<RetAddrReplaceInfo, 24> UsesToReplace;
    Function *Retaddr = Intrinsic::getDeclaration(&M, Intrinsic::returnaddress);
    // Loop over all calls to llvm.returnaddress
    for (auto *CallUser : Retaddr->users()) {
        auto *Call = dyn_cast<CallInst>(CallUser);
        if (!Call) {
            continue;
        }
        for (auto *PtrToIntUser : Call->users()) {
            auto *Cast = dyn_cast<PtrToIntInst>(PtrToIntUser);
            if (!Cast) {
                continue;
            }
            for (Use &U : Cast->uses()) {
                auto *Call = dyn_cast<CallInst>(U.getUser());
                Function *F = Call->getCalledFunction();
                StringRef Name = F->getName();
                if (Name.startswith("cpu_ld") or Name.startswith("cpu_st")) {
                    UsesToReplace.push_back({
                        .Parent = U.getUser(),
                        .OpIndex = U.getOperandNo(),
                        .Ty = U->getType(),
                    });
                }
            }
        }
    }

    // Defer replacement to not invalidate iterators
    for (RetAddrReplaceInfo &RI : UsesToReplace) {
        auto *Undef = UndefValue::get(RI.Ty);
        RI.Parent->setOperand(RI.OpIndex, Undef);
    }
}

PreservedAnalyses PrepareForOptPass::run(Module &M, ModuleAnalysisManager &MAM)
{
    collectAnnotations(M, ResultAnnotations);
    cullUnusedFunctions(M, ResultAnnotations, TranslateAllHelpers);
    replaceRetaddrWithUndef(M);
    return PreservedAnalyses::none();
}
