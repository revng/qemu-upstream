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

#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Module.h>

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

PreservedAnalyses PrepareForOptPass::run(Module &M, ModuleAnalysisManager &MAM)
{
    collectAnnotations(M, ResultAnnotations);
    return PreservedAnalyses::none();
}
