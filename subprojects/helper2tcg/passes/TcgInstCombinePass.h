#pragma once

#include "llvm/IR/PassManager.h"

//
// TcgInstCombinePass
//
// Transform pass with the end goal of transforming the input LLVM IR to
// something that maps more directly to TCG.
//
// This pass relies heavily on replaces LLVM expressions with "fake" function
// calls representing custom opcodes.
//

struct TcgInstCombinePass : llvm::PassInfoMixin<TcgInstCombinePass> {
  TcgInstCombinePass() {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
