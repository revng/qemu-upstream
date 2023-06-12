#pragma once

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include <set>

#include "backend/TcgType.h"
#include "backend/TcgEmit.h"

//
// TcgTempAllocationPass
//
// Analysis pass that performs basic register allocation to assign identifiers
// representing TCGv's to all values in a given function.
//
// Note: Input code is assumed to be loop free, which drastically simplifies
// the register allocation. This assumption is reasonable as we expect code
// with loops to be either unrolled or vectorized, and we currently don't emit
// for loops in C.
//
// This pass also contains the logic for mapping various LLVM values to a TcgV
// struct, which is necessary in order to figure out what type we need for in
// TCG.
//

extern llvm::cl::opt<uint32_t> TcgTargetPtrSize;

enum ArgumentKind {
  ArgTemp,
  ArgImmediate,
  ArgPtrToOffset,
};

struct Arguments {
  llvm::Optional<llvm::Value *> EnvPtr;
  llvm::DenseMap<llvm::Value *, ArgumentKind> ArgInfoMap;
  llvm::SmallSet<llvm::Value *, 8> Args;
};

struct TempAllocationData {
  // Mapping of LLVM Values to the corresponding TcgV
  llvm::DenseMap<llvm::Value *, TcgV> Map;

  // Keeps track of global varialbes accessed by this function that are
  // not mapped to env.
  std::set<llvm::StringRef> ExternalSymbols;

  // Whether or not the final mov in an instruction can safely
  // be ignored or not.
  bool SkipReturnMov = false;
  llvm::Optional<TcgV> ReturnValue;
  Arguments Args;
};

struct TcgTempAllocationPass : public llvm::AnalysisInfoMixin<TcgTempAllocationPass> {
  using Result = TempAllocationData;

  TcgTempAllocationPass() {}
  Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);

  static llvm::AnalysisKey Key;
};
