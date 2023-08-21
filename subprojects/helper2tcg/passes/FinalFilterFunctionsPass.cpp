#include "FinalFilterFunctionsPass.h"
#include "MapAnnotationsPass.h"

#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"

#include <algorithm>
#include <iterator>
#include <queue>

using namespace llvm;

using AnnotationMapTy = MapAnnotationsPass::AnnotationMapTy;

llvm::PreservedAnalyses FinalFilterFunctionsPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  auto &AnnotationMap = MAM.getResult<MapAnnotationsPass>(M).Map;

  auto &FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();

  SmallVector<Function *, 16> FunctionsToRemove;
  for (auto &F : M) {
    if (!F.isDeclaration()) {
      auto &LI = FAM.getResult<LoopAnalysis>(F);
      bool HasLoops = LI.begin() != LI.end();
      if (HasLoops)
        FunctionsToRemove.push_back(&F);
    }
  }

  for (auto *F : FunctionsToRemove) {
    AnnotationMap.erase(F);
    F->deleteBody();
  }

  return PreservedAnalyses::none();
}
