#pragma once

#include "llvm/IR/PassManager.h"
#include "llvm/ADT/DenseMap.h"
#include <unordered_map>

//
// MapAnnotationsPass
//
// Function annotations are stored in the module as a big global array,
// this pass iterates over this array and converts it to a map.
//

enum AnnotationKind {
    AnnLlvmToTcg,
    AnnImmediate,
    AnnPtrToOffset,
};

struct Annotation {
    AnnotationKind Kind;
    llvm::SmallVector<uint32_t, 8> ArgIndices;
};

struct MapAnnotationsPass : llvm::AnalysisInfoMixin<MapAnnotationsPass> {
  static llvm::AnalysisKey Key;

  using AnnotationVector = llvm::SmallVector<Annotation, 3>;
  using AnnotationMapTy = llvm::DenseMap<llvm::Function *, AnnotationVector>;

  struct Result {
    AnnotationMapTy Map;

    bool invalidate(llvm::Module &, const llvm::PreservedAnalyses &,
                    llvm::ModuleAnalysisManager::Invalidator &) {
      return false;
    }
  };

  Result mapAnnotationsToFunctions(const llvm::Module &M);

  MapAnnotationsPass() {}
  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);
};
