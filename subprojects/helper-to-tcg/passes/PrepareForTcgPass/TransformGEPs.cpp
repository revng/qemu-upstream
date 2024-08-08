//
//  Copyright(c) 2024 rev.ng Labs Srl. All Rights Reserved.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, see <http://www.gnu.org/licenses/>.
//

#include "TransformGEPs.h"
#include <Error.h>
#include <PseudoInst.h>

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Value.h>

using namespace llvm;

// collectIndices will, given a getelementptr (GEP) instruction, construct an
// array of GepIndex structs keeping track of the total offset into the struct
// along with some access information.  For instance,
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
// or the following GepIndex's
//
//   GepIndex{Size=0,false}, GepIndex{Size=8,false}, GepIndex{Size=4,true},
//   GepIndex{Size=0,false} GepIndex{Size=0,false}, GepIndex{Size=8,false},
//   GepIndex{Size=4,true}, GepIndex{Size=1,false} GepIndex{Size=0,false},
//   GepIndex{Size=8,false}, GepIndex{Size=4,true}, GepIndex{Size=2,false}
//

struct GepIndex {
    Value *V;
    uint64_t Size;
    bool IsArrayAccess = false;
};

using GepIndices = SmallVector<GepIndex, 2>;

static Expected<GepIndices> collectIndices(const DataLayout &DL,
                                           GEPOperator *Gep)
{
    Type *PtrOpTy = Gep->getPointerOperandType();
    if (!PtrOpTy->isPointerTy()) {
        return mkError("GEPs on vectors are not handled!");
    }
    Type *InternalTy = Type::getIntNTy(Gep->getContext(), 64);
    auto *One = ConstantInt::get(InternalTy, 1u);

    GepIndices Result;

    // NOTE: LLVM <= 11 doesn't have Gep->indices()
    Type *CurrentTy = PtrOpTy;
    for (auto &Arg : make_range(Gep->idx_begin(), Gep->idx_end())) {
        switch (CurrentTy->getTypeID()) {
        case Type::PointerTyID: {
            CurrentTy = cast<PointerType>(CurrentTy)->getPointerElementType();
            uint64_t FixedSize = DL.getTypeAllocSize(CurrentTy).getFixedSize();
            Result.push_back(GepIndex{Arg.get(), FixedSize});
        } break;
        case Type::ArrayTyID: {
            CurrentTy = cast<ArrayType>(CurrentTy)->getElementType();
            uint64_t FixedSize = DL.getTypeAllocSize(CurrentTy).getFixedSize();
            Result.push_back(
                GepIndex{Arg.get(), FixedSize, /* IsArrayAccess= */ true});
        } break;
        case Type::StructTyID: {
            auto *StructTy = cast<StructType>(CurrentTy);
            auto *Constant = dyn_cast<ConstantInt>(Arg.get());
            if (Constant->getBitWidth() > DL.getPointerSizeInBits()) {
                return mkError(
                    "GEP to struct with unsupported index bit width!");
            }
            uint64_t ConstantValue = Constant->getZExtValue();
            uint64_t ElementOffset =
                DL.getStructLayout(StructTy)->getElementOffset(ConstantValue);
            CurrentTy = StructTy->getTypeAtIndex(ConstantValue);
            Result.push_back(GepIndex{One, ElementOffset});
        } break;
        default:
            return mkError("GEP unsupported index type: ");
        }
    }

    return Result;
}

// Takes indices associated with a getelementpointer instruction and expands
// it into pointer math.
static void replaceGEPWithPointerMath(Module &M, Instruction *ParentInst,
                                      GEPOperator *Gep,
                                      const GepIndices &Indices)
{
    assert(Indices.size() > 0);
    IRBuilder<> Builder(ParentInst);
    Value *PtrOp = Gep->getPointerOperand();

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

    FunctionCallee Fn = pseudoInstFunction(
        M, PtrAdd, Gep->getType(), {PtrOp->getType(), PrevV->getType()});
    CallInst *Call = Builder.CreateCall(Fn, {PtrOp, PrevV});
    Gep->replaceAllUsesWith(Call);
}

// Takes indices associated with a getelementpointer instruction and expands
// it into pointer math.
static void replaceGEPWithGlobalAccess(Module &M, Instruction *ParentInst,
                                       GEPOperator *Gep, uint64_t BaseOffset,
                                       Value *ArrayIndex)
{
    IRBuilder<> Builder(ParentInst);
    Type *IndexTy = Type::getIntNTy(M.getContext(), 64);
    auto *ConstBaseOffset = ConstantInt::get(IndexTy, BaseOffset);
    if (ArrayIndex) {
        Type *ArrayAccessTy = ArrayIndex->getType();
        FunctionCallee Fn = pseudoInstFunction(
            M, AccessGlobalArray, Gep->getType(), {IndexTy, ArrayAccessTy});
        CallInst *Call = Builder.CreateCall(Fn, {ConstBaseOffset, ArrayIndex});
        Gep->replaceAllUsesWith(Call);
    } else {
        FunctionCallee Fn =
            pseudoInstFunction(M, AccessGlobalValue, Gep->getType(), {IndexTy});
        CallInst *Call = Builder.CreateCall(Fn, {ConstBaseOffset});
        Gep->replaceAllUsesWith(Call);
    }
}

static bool transformGEP(Module &M, const TcgGlobalMap &TcgGlobals,
                         const GepIndices &Indices, Instruction *ParentInst,
                         GEPOperator *Gep)
{
    Value *PtrOp = Gep->getPointerOperand();

    bool PtrOpIsEnv = false;
    {
        auto *PtrTy = cast<PointerType>(PtrOp->getType());
        auto *StructTy = dyn_cast<StructType>(PtrTy->getPointerElementType());
        // NOTE: We are identifying the CPU state via matching the typename to
        // CPUArchState. This is fragile to QEMU name changes, and does not
        // play nicely with non-env structs.
        PtrOpIsEnv = StructTy and StructTy->getName() == "struct.CPUArchState";
    }

    uint64_t BaseOffset = 0;
    uint32_t NumArrayAccesses = 0;
    Value *LastArrayAccess = nullptr;
    for (const GepIndex &Index : Indices) {
        if (Index.IsArrayAccess) {
            LastArrayAccess = Index.V;
            ++NumArrayAccesses;
        } else {
            auto *Const = dyn_cast<ConstantInt>(Index.V);
            if (Const) {
                BaseOffset += Const->getZExtValue() * Index.Size;
            }
        }
    }

    if (PtrOpIsEnv) {
        auto It = TcgGlobals.find(BaseOffset);
        if (It != TcgGlobals.end()) {
            if (LastArrayAccess && NumArrayAccesses > 1) {
                return false;
            }
            replaceGEPWithGlobalAccess(M, ParentInst, Gep, BaseOffset,
                                       LastArrayAccess);
            return !isa<ConstantExpr>(Gep);
        }
    }

    replaceGEPWithPointerMath(M, ParentInst, Gep, Indices);
    return !isa<ConstantExpr>(Gep);
}

static GEPOperator *getGEPOperator(Instruction *I)
{
    // If the instructions is directly a GEP, simply return it.
    auto *GEP = dyn_cast<GEPOperator>(I);
    if (GEP) {
        return GEP;
    }

    // Hard-code handling of GEPs that appear as an inline operand to loads
    // and stores.
    if (isa<LoadInst>(I)) {
        auto *Load = cast<LoadInst>(I);
        auto *ConstExpr = dyn_cast<ConstantExpr>(Load->getPointerOperand());
        if (ConstExpr) {
            return dyn_cast<GEPOperator>(ConstExpr);
        }
    } else if (isa<StoreInst>(I)) {
        auto *Store = dyn_cast<StoreInst>(I);
        auto *ConstExpr = dyn_cast<ConstantExpr>(Store->getPointerOperand());
        if (ConstExpr) {
            return dyn_cast<GEPOperator>(ConstExpr);
        }
    }

    return nullptr;
}

void transformGEPs(Module &M, Function &F, const TcgGlobalMap &TcgGlobals)
{
    SmallSet<Instruction *, 8> InstToErase;

    for (auto &I : instructions(F)) {
        GEPOperator *GEP = getGEPOperator(&I);
        if (!GEP) {
            continue;
        }

        Expected<GepIndices> Indices = collectIndices(M.getDataLayout(), GEP);
        if (!Indices) {
            dbgs() << "Failed collecting GEP indices for:\n\t" << I << "\n";
            dbgs() << "Reason: " << Indices.takeError();
            abort();
        }

        bool ShouldErase = transformGEP(M, TcgGlobals, Indices.get(), &I, GEP);
        if (ShouldErase) {
            InstToErase.insert(&I);
        }
    }

    for (auto *I : InstToErase) {
        I->eraseFromParent();
    }
}
