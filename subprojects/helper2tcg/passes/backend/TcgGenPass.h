#pragma once

#include "llvm/IR/PassManager.h"

//
// TcgGenPass
//
// Backend pass responsible for emitting the final TCG code.  Ideally this pass
// should be as simple as possible simply mapping one expression LLVM IR directly
// to another in TCG.
//
// However, we currently still rely on this pass to perform the mapping of constants.
// (mapping of values is handled by the TcgTempAllocationPass.)
//

struct TcgGenPass : llvm::PassInfoMixin<TcgGenPass> {
  llvm::raw_ostream &OutSource;
  llvm::raw_ostream &OutHeader;
  llvm::raw_ostream &OutEnabled;
  llvm::raw_ostream &OutLog;
  llvm::StringRef HeaderPath;

  TcgGenPass(llvm::raw_ostream &OutSource,
             llvm::raw_ostream &OutHeader,
             llvm::raw_ostream &OutEnabled,
             llvm::raw_ostream &OutLog,
             llvm::StringRef HeaderPath)
    : OutSource(OutSource),
      OutHeader(OutHeader),
      OutEnabled(OutEnabled),
      OutLog(OutLog),
      HeaderPath(HeaderPath)
  {}

  llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
