#pragma once

#include "llvm/IR/PassManager.h"

//
// FinalFilterFunctionsPass
//
// Pass that filters functions from the llvm module (post optimization)
// that we have no interest in translating.
//
// We remove:
//   - Functions with loops
//

struct FinalFilterFunctionsPass : llvm::PassInfoMixin<FinalFilterFunctionsPass> {
  FinalFilterFunctionsPass() {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
