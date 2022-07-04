#pragma once

#include "llvm/IR/PassManager.h"
#include "llvm/ADT/DenseMap.h"

//
// TcgGlobalMapPass
//
// Analysis pass that iterates over a global array of cpu_tcg_mapping structs
// (see include/helper2tcg/tcg_global_mappings.h) to extract information mappings
// between values in CPUArchState and TCGv globals.
//

struct TcgGlobalMapPass : public llvm::AnalysisInfoMixin<TcgGlobalMapPass> {
  struct TcgGlobal {
    llvm::StringRef Code;
    uint64_t Size;
    uint64_t NumElements;
    uint64_t Stride;
  };

  using Result = llvm::DenseMap<uint32_t, TcgGlobal>;

  TcgGlobalMapPass() {}
  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

  static llvm::AnalysisKey Key;
};
