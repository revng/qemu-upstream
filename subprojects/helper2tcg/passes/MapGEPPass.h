#pragma once

#include "llvm/IR/PassManager.h"
#include "llvm/ADT/DenseMap.h"
#include <stdint.h>

//
// MapGEPPass
//
// Depends on CollectGEPIndices below
//
// Transform pass that converts getelementptr (GEP) operators to
//   - call @globalArray(OffsetInEnv, Index)
//     if OffsetInEnv is mapped to a global TCGv array.
//
//   - call @globalValue(OffsetInEnv)
//     if OffsetInEnv is mapped to a global TCGv value.
//
//   - call @ptradd.*(OffsetInEnv)
//     otherwise
//

struct MapGEPPass : llvm::PassInfoMixin<MapGEPPass> {
  MapGEPPass() {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

//
// CollectGEPIndices
//
// Analysis pass that iterates of getelementptr (GEP) operators and creates
// an array of GEPIndex structs for each GEP.  GEPIndex keeps track of the
// total offset into the struct.  For instance,
//
//   struct SubS {
//      uint8_t a;
//      uint8_t b;
//      uint8_t c;
//   };
//
//   struct S {
//      uint64_t i;
//      struct SubS sub[3];
//   };
//
//   void f(struct S *s, int idx) {
//      S->sub[idx].a = ...
//      S->sub[idx].b = ...
//      S->sub[idx].c = ...
//   }
//
// would correspond to the following GEPs
//
//   getelementptr %struct.S, %struct.S* %s, i64 0, i32 1, %idx, i32 0
//   getelementptr %struct.S, %struct.S* %s, i64 0, i32 1, %idx, i32 1
//   getelementptr %struct.S, %struct.S* %s, i64 0, i32 1, %idx, i32 2
//
// or the following GEPIndex's
//
//   GEPIndex{Size=0,false}, GEPIndex{Size=8,false}, GEPIndex{Size=4,true}, GEPIndex{Size=0,false}
//   GEPIndex{Size=0,false}, GEPIndex{Size=8,false}, GEPIndex{Size=4,true}, GEPIndex{Size=1,false}
//   GEPIndex{Size=0,false}, GEPIndex{Size=8,false}, GEPIndex{Size=4,true}, GEPIndex{Size=2,false}
//

// TODO(anjo): Rename Size to Offset
// TODO(anjo): I think we can just use a union here instead. I don't believe
//             we ever use the Size argument for Pointer/Array?
//
//             The name Size is confusing, and offset isn't quite right either.
//             It's a size in the context of Pointer/Array, and an offset in the
//             context of Constants.  Confusing.
struct GEPIndex {
  llvm::Value *V;
  uint64_t Size;
  bool IsArrayAccess = false;
};

using GEPIndices = llvm::SmallVector<GEPIndex, 2>;

struct CollectGEPIndices : llvm::AnalysisInfoMixin<CollectGEPIndices> {
  // The result is a map from GEP instructions or store/load
  // instructions with constant expr. GEP pointer operand,
  // to the corresponding GEPIndices that has been collected.
  using Result = llvm::DenseMap<llvm::Value *, GEPIndices>;
  CollectGEPIndices() = default;
  Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);

  static llvm::AnalysisKey Key;
};
