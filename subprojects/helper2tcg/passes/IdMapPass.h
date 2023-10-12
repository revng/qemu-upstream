#pragma once

#include "llvm/IR/PassManager.h"

//
// IdMapPass
//
// Pass that "standardises" identity mapping of instructions.  When mapping
// LLVM IR -> TCG a few instructions become irrelevant, such as
//
//   zext i8 to i32,
//
// since both the input value of size i8 and the output of size i32 would
// be mapped to a TCGv_i32 in TCG.
//
// We cannot directly remove these instructions as this would break the IR,
// or loose information (if we widen <= 32-bit types), so we instead replace
// them by a call
//
//   call @id.i8.i32(...).
//

struct IdMapPass : llvm::PassInfoMixin<IdMapPass> {
  IdMapPass() {}
  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
