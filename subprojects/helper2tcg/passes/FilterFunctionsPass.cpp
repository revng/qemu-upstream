#include "FilterFunctionsPass.h"
#include "MapAnnotationsPass.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Intrinsics.h"

#include <queue>
#include <set>

using namespace llvm;

using AnnotationMapTy = MapAnnotationsPass::AnnotationMapTy;

inline bool hasValidReturnTy(const Module &M, const Function *F) {
  Type *RetTy = F->getReturnType();
  return RetTy == Type::getVoidTy(F->getContext()) ||
         RetTy == Type::getInt8Ty(M.getContext())  ||
         RetTy == Type::getInt16Ty(M.getContext()) ||
         RetTy == Type::getInt32Ty(M.getContext()) ||
         RetTy == Type::getInt64Ty(M.getContext());
}

// Functions that should be removed:
//   - No llvm-to-tcg annotation;
//   - Invalid (non-integer/void) return type
static bool shouldRemoveFunction(const Module &M, const Function &F,
                                 const AnnotationMapTy &AnnotationMap) {
  if (F.isDeclaration()) {
    return false;
  }

  if (!hasValidReturnTy(M, &F)) {
      return true;
  }

  auto hasCorrectAnnotation = [](const Annotation &Ann) {
    return Ann.Kind == AnnLlvmToTcg;
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
    auto It = AnnotationMap.find(F);
    if (It != AnnotationMap.end()) {
      const auto &AnnotationVec = It->second;
      auto Res = find_if(AnnotationVec, hasCorrectAnnotation);
      if (Res != AnnotationVec.end()) {
        return false;
      }
    }

    // Push functions that call F to the worklist, this way we retain functions
    // that are being called by functions with the llvm-to-tcg annotation.
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

struct RetAddrReplaceInfo {
  User *Parent;
  unsigned OpIndex;
  Type *Ty;
};

llvm::PreservedAnalyses
FilterFunctionsPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  auto &AnnotationMap = MAM.getResult<MapAnnotationsPass>(M).Map;

  SmallVector<Function *, 16> FunctionsToRemove;
  for (auto &F : M) {
    if (shouldRemoveFunction(M, F, AnnotationMap)) {
      FunctionsToRemove.push_back(&F);
    }
  }

  for (Function *F : FunctionsToRemove) {
    AnnotationMap.erase(F);
    F->deleteBody();
  }

  // Replace uses of llvm.returnaddress arguments to cpu_ld* w. undef,
  // and let optimizations remove it
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

  return PreservedAnalyses::none();
}
