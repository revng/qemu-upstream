#pragma once

#include "llvm/IR/PassManager.h"

//
// RemovePhisPass
//
// Transform pass that removes phi nodes by demoting them to stack arguments.
// Needed since there is no phi-node equivalent in TCG.
//

struct RemovePhisPass : llvm::PassInfoMixin<RemovePhisPass> {
  RemovePhisPass() {}
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};
