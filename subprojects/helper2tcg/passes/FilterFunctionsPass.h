#pragma once

#include "llvm/IR/PassManager.h"

//
// FilterFunctionsPass
//
// Pass that filters functions from the llvm module that we have no
// interest in attempting to translate.
//
// We remove:
//   - Functions with incorrect return type (non void/int32_t/int64_t)
//   - Functions without the llvm-to-tcg attribute that are not called
//     by any other function with the attribute.
//

struct FilterFunctionsPass : llvm::PassInfoMixin<FilterFunctionsPass> {
  FilterFunctionsPass() {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
