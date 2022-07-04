#include "RemovePhisPass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

PreservedAnalyses RemovePhisPass::run(Function &F, FunctionAnalysisManager &FAM) {
  if (F.isDeclaration() or F.getName().startswith("llvm"))
    return PreservedAnalyses::all();

  SmallVector<PHINode *, 10> Phis;
  for (auto &BB : F) {
    for (auto &I : BB) {
      if (auto *Phi = dyn_cast<PHINode>(&I)) {
        Phis.push_back(Phi);
      }
    }
  }

  for (auto *Phi : Phis) {
    DemotePHIToStack(Phi);
  }

  return PreservedAnalyses::all();
}
