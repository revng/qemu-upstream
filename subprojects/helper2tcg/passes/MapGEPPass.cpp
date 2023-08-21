#include "MapGEPPass.h"
#include "TcgGlobalMapPass.h"

#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/Support/Debug.h"

#include "PseudoInst.h"

#include <utils/Common.h>

using OffsetMapTy = TcgGlobalMapPass::Result;
using TcgGlobal = TcgGlobalMapPass::TcgGlobal;

using namespace llvm;

cl::opt<uint32_t> TcgHostPtrSize("tcg-host-ptr-size",
                                 cl::desc("<TCG pointer size>"),
                                 cl::init(64));

static bool mapGEP(Module &M, const OffsetMapTy &OffsetMap,
                   const GEPIndices &Indices, Instruction *ParentInst,
                   GEPOperator *GEP) {
  Value *PtrOp = GEP->getPointerOperand();

  bool PtrOpIsEnv = false;
  {
    auto *PtrTy = cast<PointerType>(PtrOp->getType());
    auto *StructTy = dyn_cast<StructType>(PtrTy->getPointerElementType());
    PtrOpIsEnv = StructTy and StructTy->getName() == "struct.CPUArchState";
  }

  if (PtrOpIsEnv) {
    uint64_t BaseOffset = 0;
    for (const GEPIndex &Index : Indices) {
      if (Index.IsArrayAccess) {
        continue;
      }
      if (auto *Const = dyn_cast<ConstantInt>(Index.V)) {
        BaseOffset += Const->getZExtValue() * Index.Size;
      } else {
        return false;
      }
    }

    if (auto It = OffsetMap.find(BaseOffset); It != OffsetMap.end()) {
      const GEPIndex *ArrayAccess = nullptr;
      for (auto &Index : Indices) {
        if (!Index.IsArrayAccess) {
          continue;
        }
        // We only support a single level of array accesses when mapping to TCG
        // globals
        if (ArrayAccess) {
          return false;
        }
        ArrayAccess = &Index;
      }

      if (ArrayAccess) {
        IRBuilder<> Builder(cast<Instruction>(GEP));
        Type *IndexTy = Type::getIntNTy(M.getContext(), 64);
        Type *ArrayAccessTy = ArrayAccess->V->getType();
        FunctionCallee Fn = pseudoInstFunction(M, AccessGlobalArray, GEP->getType(), {IndexTy, ArrayAccessTy});
        CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(IndexTy, BaseOffset), ArrayAccess->V});
        GEP->replaceAllUsesWith(Call);
        return true;
      } else {
        IRBuilder<> Builder(cast<Instruction>(GEP));
        auto *IndexTy = Type::getIntNTy(M.getContext(), 64);
        FunctionCallee Fn = pseudoInstFunction(M, AccessGlobalValue, GEP->getType(), {IndexTy});
        CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(IndexTy, BaseOffset)});
        GEP->replaceAllUsesWith(Call);
        return true;
      }

      // all remaining accesses have to be array accesses
    } else {
      // other base pointers
      IRBuilder<> Builder(cast<Instruction>(GEP));

      // Sum indices to get the total offset from the base pointer
      // TODO(anjo): This loop is not necessary, we already know BaseOffset
      // contains all constant offsets and that all non-constant offsets are
      // array accesses, and that we have at most 1 array access. Simply add the
      // array access to the constant CONTHERE
      return false;
      Value *PrevV = nullptr;
      for (auto &Index : Indices) {
        Value *Mul = Builder.CreateMul(
            Index.V, ConstantInt::get(Index.V->getType(), Index.Size));
        if (PrevV) {
          uint32_t BitWidthLeft =
              cast<IntegerType>(PrevV->getType())->getIntegerBitWidth();
          uint32_t BitWidthRight =
              cast<IntegerType>(Mul->getType())->getIntegerBitWidth();
          if (BitWidthLeft < BitWidthRight) {
            PrevV = Builder.CreateZExt(PrevV, Mul->getType());
          } else if (BitWidthLeft > BitWidthRight) {
            Mul = Builder.CreateZExt(Mul, PrevV->getType());
          }
          PrevV = Builder.CreateAdd(PrevV, Mul);
        } else {
          PrevV = Mul;
        }
      }
      assert(PrevV);

      auto PtrOpTy = PtrOp->getType();
      auto *IndexTy = Type::getIntNTy(M.getContext(), 64);
      FunctionCallee Fn = pseudoInstFunction(M, PtrAdd, GEP->getType(), {IndexTy, PrevV->getType()});
      CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(IndexTy, BaseOffset), PrevV});
      GEP->replaceAllUsesWith(Call);
      return true;
    }

    return false;
  } else {
    // other base pointers
    IRBuilder<> Builder(ParentInst);

    // Sum indices to get the total offset from the base pointer
    Value *PrevV = nullptr;
    for (auto &Index : Indices) {
      Value *Mul = Builder.CreateMul(
          Index.V, ConstantInt::get(Index.V->getType(), Index.Size));
      if (PrevV) {
        uint32_t BitWidthLeft =
            cast<IntegerType>(PrevV->getType())->getIntegerBitWidth();
        uint32_t BitWidthRight =
            cast<IntegerType>(Mul->getType())->getIntegerBitWidth();
        if (BitWidthLeft < BitWidthRight) {
          PrevV = Builder.CreateZExt(PrevV, Mul->getType());
        } else if (BitWidthLeft > BitWidthRight) {
          Mul = Builder.CreateZExt(Mul, PrevV->getType());
        }
        PrevV = Builder.CreateAdd(PrevV, Mul);
      } else {
        PrevV = Mul;
      }
    }
    assert(PrevV);

    auto PtrOpTy = PtrOp->getType();
    FunctionCallee Fn = pseudoInstFunction(M, PtrAdd, GEP->getType(), {PtrOp->getType(), PrevV->getType()});
    CallInst *Call = Builder.CreateCall(Fn, {PtrOp, PrevV});
    GEP->replaceAllUsesWith(Call);
    return !isa<ConstantExpr>(GEP);
  }
}

PreservedAnalyses MapGEPPass::run(Module &M, ModuleAnalysisManager &MAM) {
  auto &FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  const auto &OffsetMap = MAM.getResult<TcgGlobalMapPass>(M);

  for (auto &F : M) {

    const auto &GEPIndexMap = FAM.getResult<CollectGEPIndices>(F);
    SmallSet<Instruction *, 8> InstToErase;

    for (auto &BB : F) {
      for (auto &I : BB) {
        auto *GEP = dyn_cast<GEPOperator>(&I);
        if (!GEP) {
          if (auto *Load = dyn_cast<LoadInst>(&I)) {
            if (auto *ConstExpr =
                    dyn_cast<ConstantExpr>(Load->getPointerOperand())) {
              GEP = dyn_cast<GEPOperator>(ConstExpr);
            }
          } else if (auto *Store = dyn_cast<StoreInst>(&I)) {
            if (auto *ConstExpr =
                    dyn_cast<ConstantExpr>(Store->getPointerOperand())) {
              GEP = dyn_cast<GEPOperator>(ConstExpr);
            }
          }
        }
        if (!GEP) {
          continue;
        }

        auto It = GEPIndexMap.find(cast<Value>(&I));
        if (It == GEPIndexMap.end()) {
          continue;
        }
        bool ShouldEraseGEP = mapGEP(M, OffsetMap, It->second, &I, GEP);
        // TODO(anjo): Report error
        if (ShouldEraseGEP) {
          InstToErase.insert(&I);
        }
      }
    }

    for (auto I : InstToErase) {
      I->eraseFromParent();
    }
  }

  return PreservedAnalyses::none();
}

static Expected<GEPIndices> collectIndices(const DataLayout &DL,
                                           GEPOperator *GEP) {
  Type *PtrOpTy = GEP->getPointerOperandType();
  if (!PtrOpTy->isPointerTy())
    return mkError("GEPs on vectors are not handled!");
  Type *InternalTy = Type::getIntNTy(GEP->getContext(), TcgHostPtrSize);
  auto *One = ConstantInt::get(InternalTy, 1u);

  GEPIndices Result;

  // NOTE: LLVM <= 11 doesn't have GEP->indices()
  Type *CurrentTy = PtrOpTy;
  for (auto Argument = GEP->idx_begin(); Argument != GEP->idx_end();
       ++Argument) {
    if (auto *Pointer = dyn_cast<PointerType>(CurrentTy)) {
      CurrentTy = Pointer->getPointerElementType();
      uint64_t FixedSize = DL.getTypeAllocSize(CurrentTy).getFixedSize();

      Result.push_back(GEPIndex{Argument->get(), FixedSize});
    } else if (auto *Array = dyn_cast<ArrayType>(CurrentTy)) {
      CurrentTy = Array->getElementType();
      uint64_t FixedSize = DL.getTypeAllocSize(CurrentTy).getFixedSize();

      Result.push_back(GEPIndex{Argument->get(), FixedSize, true});
    } else if (auto *Struct = dyn_cast<StructType>(CurrentTy)) {
      auto *Constant = dyn_cast<ConstantInt>(Argument->get());
      if (!Constant) {
        return mkError("GEPs to struct must have constant indices!");
      }
      if (Constant->getBitWidth() > DL.getPointerSizeInBits()) {
        return mkError("GEP to struct with unsupported index bit width!");
      }
      uint64_t ConstantValue = Constant->getZExtValue();
      uint64_t ElementOffset =
          DL.getStructLayout(Struct)->getElementOffset(ConstantValue);
      CurrentTy = Struct->getTypeAtIndex(ConstantValue);

      Result.push_back(GEPIndex{One, ElementOffset});
    } else {
      return mkError("GEP unsupported index type: ", Argument->get());
    }
  }

  return Result;
}

CollectGEPIndices::Result CollectGEPIndices::run(Function &F,
                                                 FunctionAnalysisManager &FAM) {
  CollectGEPIndices::Result Result;

  const Module *M = F.getParent();
  const DataLayout &DL = M->getDataLayout();

  // Loop over and getelementptr instructions and inline getelementptr
  // constant expressions that sometimes occur inline with loads/stores
  for (auto &BB : F) {
    for (auto &I : BB) {
      auto *GEP = dyn_cast<GEPOperator>(&I);
      if (!GEP) {
        if (auto *Load = dyn_cast<LoadInst>(&I)) {
          if (auto *ConstExpr =
                  dyn_cast<ConstantExpr>(Load->getPointerOperand())) {
            GEP = dyn_cast<GEPOperator>(ConstExpr);
          }
        } else if (auto *Store = dyn_cast<StoreInst>(&I)) {
          if (auto *ConstExpr =
                  dyn_cast<ConstantExpr>(Store->getPointerOperand())) {
            GEP = dyn_cast<GEPOperator>(ConstExpr);
          }
        }
      }
      if (!GEP) {
        continue;
      }

      Expected<GEPIndices> Indices = collectIndices(DL, GEP);
      if (!Indices) {
        dbgs() << "Failed collecting GEP indices for:\n\t" << I << "\n";
        dbgs() << "Reason: " << Indices.takeError();
        break;
      }

      Result.insert(
          std::make_pair(static_cast<Value *>(&I), std::move(Indices.get())));
    }
  }

  return Result;
}

llvm::AnalysisKey CollectGEPIndices::Key{};
