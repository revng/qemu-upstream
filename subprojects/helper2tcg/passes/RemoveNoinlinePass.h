#pragma once

#include "llvm/IR/PassManager.h"

// 
// RemoveNoInlinePass
//
// Transform pass that removes the noinline function attribute.
// noinline is an artifact of -O0.
//

struct RemoveNoinlinePass : llvm::PassInfoMixin<RemoveNoinlinePass> {
  RemoveNoinlinePass() {}
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};
