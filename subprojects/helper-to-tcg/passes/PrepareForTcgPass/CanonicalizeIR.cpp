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

#include "CanonicalizeIR.h"
#include <PseudoInst.h>
#include <llvm-compat.h>

#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/VectorUtils.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PatternMatch.h>
#include <llvm/Support/Casting.h>

using namespace llvm;
using namespace PatternMatch;

// Needed to track and remove instructions not handled by a subsequent dead code
// elimination, this applies to calls to pseudo instructions in particular.
//
// TODO: Can we instead make pseudo instructions side effect free via
// attributes?
using EraseInstVec = SmallVector<Instruction *, 16>;
using UsageCountMap = DenseMap<Value *, uint16_t>;

// Helper function to remove an instruction only if all uses have been removed.
// This way we can keep track instruction uses without having to modify the IR,
// or without having to iterate over all uses everytime we wish to remove an
// instruction.
static void addToEraseVectorIfUnused(EraseInstVec &InstToErase,
                                     UsageCountMap &UsageMap, Value *V)
{
    auto *I = dyn_cast<Instruction>(V);
    if (!I) {
        return;
    }

    // Add V to map if not there
    if (UsageMap.count(V) == 0) {
        UsageMap[V] = V->getNumUses();
    }

    // Erase if count reaches zero
    if (--UsageMap[V] == 0) {
        InstToErase.push_back(I);
        UsageMap.erase(V);
    }
}

// Forward declarations of IR transformations used in canonicalizing the IR
static void upcastAshr(Instruction *I);
static void convertInsertShuffleToSplat(Module &M, Instruction *I);

static void simplifyVecBinOpWithSplat(EraseInstVec &InstToErase,
                                      UsageCountMap &UsageMap, Module &M,
                                      BinaryOperator *BinOp);

static void convertSelectICmp(Module &M, SelectInst *Select, ICmpInst *ICmp);

static void convertQemuLoadStoreToPseudoInst(Module &M, CallInst *Call);
static void convertExceptionCallsToPseudoInst(Module &M, CallInst *Call);
static void convertVecStoreBitcastToPseudoInst(EraseInstVec &InstToErase,
                                               Module &M, StoreInst *Store);
static void convertICmpBrToPseudInst(LLVMContext &Context,
                                     EraseInstVec &InstToErase, Module &M,
                                     Instruction *I, BasicBlock *NextBb);

void canonicalizeIR(Module &M)
{
    for (Function &F : M) {
        if (F.isDeclaration()) {
            continue;
        }

        EraseInstVec InstToErase;
        UsageCountMap UsageMap;
        LLVMContext &Context = F.getContext();

        // Perform a first pass over all instructions in the function and apply
        // IR transformations sequentially.  NOTE: order matters here.
        for (Instruction &I : instructions(F)) {
            if (I.isArithmeticShift()) {
                upcastAshr(&I);
            }

            convertInsertShuffleToSplat(M, &I);

            // Depends on convertInsertShuffleToSplat for @VecSplat instructions
            if (auto *BinOp = dyn_cast<BinaryOperator>(&I)) {
                simplifyVecBinOpWithSplat(InstToErase, UsageMap, M, BinOp);
            }

            // Independent of above
            if (auto *ICmp = dyn_cast<ICmpInst>(&I)) {
                for (auto *U : ICmp->users()) {
                    auto *Select = dyn_cast<SelectInst>(U);
                    if (Select and Select->getCondition() == ICmp) {
                        convertSelectICmp(M, Select, ICmp);
                    }
                }
            }

            // Independent of above, can run at any point
            if (auto *Call = dyn_cast<CallInst>(&I)) {
                convertQemuLoadStoreToPseudoInst(M, Call);
                convertExceptionCallsToPseudoInst(M, Call);
            }

            // Depends on other vector conversions performed above, needs to
            // run last
            if (auto *Store = dyn_cast<StoreInst>(&I)) {
                convertVecStoreBitcastToPseudoInst(InstToErase, M, Store);
            }
        }

        // Perform a second pass over the instructions. Can be combined with the
        // above by using a worklist and making sure we have access to the
        // BasicBlock.
        //
        // Depends on icmp,select -> @movcond
        ReversePostOrderTraversal<Function *> RPOT(&F);
        for (auto BbIt = RPOT.begin(); BbIt != RPOT.end(); ++BbIt) {
            BasicBlock &BB = **BbIt;

            auto NextIt = BbIt;
            BasicBlock *NextBb = &**(++NextIt);

            for (Instruction &I : BB) {
                convertICmpBrToPseudInst(Context, InstToErase, M, &I, NextBb);
            }
        }

        // Finally clean up instructions we need to remove manually
        for (Instruction *I : InstToErase) {
            I->eraseFromParent();
        }
    }
}

static Value *upcastInt(IRBuilder<> &Builder, IntegerType *FinalIntTy, Value *V)
{
    if (auto *ConstInt = dyn_cast<ConstantInt>(V)) {
        return ConstantInt::get(FinalIntTy, ConstInt->getZExtValue());
    } else {
        return Builder.CreateSExt(V, FinalIntTy);
    }
}

// Convert
//
//   %2 = ashr i[8|16] %1, %0
//
// to
//
//   %2 = zext i[8|16] %1 to i32
//   %3 = zext i[8|16] %2 to i32
//   %2 = ashr i32 %2, %3
//
static void upcastAshr(Instruction *I)
{
    // Only care about scalar shifts < on less than 32-bit integers
    auto *IntTy = dyn_cast<IntegerType>(I->getType());
    if (!IntTy or IntTy->getBitWidth() >= 32) {
        return;
    }

    IRBuilder<> Builder(I);

    Value *Op1 = I->getOperand(0);
    Value *Op2 = I->getOperand(1);
    auto *UpcastIntTy = Builder.getInt32Ty();
    Op1 = upcastInt(Builder, UpcastIntTy, Op1);
    Op2 = upcastInt(Builder, UpcastIntTy, Op2);

    auto *AShr = Builder.CreateAShr(Op1, Op2);
    auto *Trunc = Builder.CreateTrunc(AShr, I->getType());
    I->replaceAllUsesWith(Trunc);
}

// Convert vector intrinsics
//
//   %0 = insertelement ...
//   %1 = shuffle ...
//
// to
//
//   %0 = call @VecSplat.*
//
static void convertInsertShuffleToSplat(Module &M, Instruction *I)
{
    Value *SplatV;
    if (match(I, compat_m_Shuffle(compat_m_InsertElt(m_Value(), m_Value(SplatV),
                                                     m_ZeroInt()),
                                  m_Value(), compat_m_ZeroMask()))) {

        auto *VecTy = cast<VectorType>(I->getType());

        IRBuilder<> Builder(I);
        FunctionCallee Fn =
            pseudoInstFunction(M, VecSplat, VecTy, {SplatV->getType()});
        CallInst *Call = Builder.CreateCall(Fn, {SplatV});
        I->replaceAllUsesWith(Call);
    }
}

// Convert
//
//   %1 = @VecSplat(%0)
//   %2 = <NxM> ... op <NxM> %1
//
// to
//
//   %2 = call @Vec[op]Scalar(..., %0)
//
// which more closely matches TCG gvec operations.
static void simplifyVecBinOpWithSplat(EraseInstVec &InstToErase,
                                      UsageCountMap &UsageMap, Module &M,
                                      BinaryOperator *BinOp)
{
    Value *Lhs = BinOp->getOperand(0);
    Value *Rhs = BinOp->getOperand(1);
    if (!Lhs->getType()->isVectorTy() or !Rhs->getType()->isVectorTy()) {
        return;
    }

    // Get splat value from constant or @VecSplat call
    Value *SplatValue = nullptr;
    if (auto *Const = dyn_cast<Constant>(Rhs)) {
        SplatValue = Const->getSplatValue();
    } else if (auto *Call = dyn_cast<CallInst>(Rhs)) {
        if (getPseudoInstFromCall(Call) == VecSplat) {
            SplatValue = Call->getOperand(0);
        }
    }

    if (SplatValue == nullptr) {
        return;
    }

    auto *VecTy = cast<VectorType>(Lhs->getType());
    auto *ConstInt = dyn_cast<ConstantInt>(SplatValue);
    bool ConstIsNegOne = ConstInt and ConstInt->getSExtValue() == -1;
    bool IsNot = BinOp->getOpcode() == Instruction::Xor and ConstIsNegOne;
    if (IsNot) {
        FunctionCallee Fn = pseudoInstFunction(M, VecNot, VecTy, {VecTy});
        IRBuilder<> Builder(BinOp);
        CallInst *Call = Builder.CreateCall(Fn, {Lhs});
        BinOp->replaceAllUsesWith(Call);
    } else {
        PseudoInst Inst;
        switch (BinOp->getOpcode()) {
        case Instruction::Add:
            Inst = VecAddScalar;
            break;
        case Instruction::Sub:
            Inst = VecSubScalar;
            break;
        case Instruction::Mul:
            Inst = VecMulScalar;
            break;
        case Instruction::Xor:
            Inst = VecXorScalar;
            break;
        case Instruction::Or:
            Inst = VecOrScalar;
            break;
        case Instruction::And:
            Inst = VecAndScalar;
            break;
        case Instruction::Shl:
            Inst = VecShlScalar;
            break;
        case Instruction::LShr:
            Inst = VecLShrScalar;
            break;
        case Instruction::AShr:
            Inst = VecAShrScalar;
            break;
        default:
            abort();
        }

        IRBuilder<> Builder(BinOp);
        // Scalar gvec shift operations uses 32-bit scalars, whereas arithmetic
        // operations uses 64-bit scalars.
        uint32_t SplatSize = SplatValue->getType()->getIntegerBitWidth();
        if (BinOp->isShift()) {
            if (SplatSize > 32) {
                SplatValue =
                    Builder.CreateTrunc(SplatValue, Builder.getInt32Ty());
            }
        } else {
            if (SplatSize < 64) {
                SplatValue =
                    Builder.CreateZExt(SplatValue, Builder.getInt64Ty());
            }
        }
        FunctionCallee Fn =
            pseudoInstFunction(M, Inst, VecTy, {VecTy, SplatValue->getType()});
        CallInst *Call = Builder.CreateCall(Fn, {Lhs, SplatValue});
        BinOp->replaceAllUsesWith(Call);
    }

    InstToErase.push_back(BinOp);
    addToEraseVectorIfUnused(InstToErase, UsageMap, Rhs);
}

// Convert
//
//   %2 = icmp [sgt|ugt|slt|ult] %0, %1
//   %5 = select %2, %3, %4
//
// to
//
//   %5 = [s|u][max|min] %0, %1
//
// if possible.  Results in cleaner IR, particularly useful for vector
// instructions.
static bool convertSelectICmpToMinMax(Module &M, SelectInst *Select,
                                      ICmpInst *ICmp, ICmpInst::Predicate &Pred,
                                      Value *ICmpOp0, Value *ICmpOp1,
                                      Value *SelectOp0, Value *SelectOp1)
{
#if LLVM_VERSION_MAJOR > 11
    if (ICmpOp0 != SelectOp0 or ICmpOp1 != SelectOp1) {
        return false;
    }

    Intrinsic::ID Intrin;
    switch (Pred) {
    case ICmpInst::ICMP_SGT:
        Intrin = Intrinsic::smax;
        break;
    case ICmpInst::ICMP_UGT:
        Intrin = Intrinsic::umax;
        break;
    case ICmpInst::ICMP_SLT:
        Intrin = Intrinsic::smin;
        break;
    case ICmpInst::ICMP_ULT:
        Intrin = Intrinsic::umin;
        break;
    default:
        return false;
    }

    auto Ty = Select->getType();
    auto MaxMinF = Intrinsic::getDeclaration(&M, Intrin, {Ty});

    IRBuilder<> Builder(Select);
    auto Call = Builder.CreateCall(MaxMinF, {ICmpOp0, ICmpOp1});
    Select->replaceAllUsesWith(Call);

    return true;
#else
    return false;
#endif
}

// In LLVM, icmp on vectors returns a vector on i1s whereas TCGs gvec_cmp
// returns a vector of the element type of its operands.  This can result in
// some subtle bugs.  Convert
//
//   icmp -> call @VecCompare
//   select -> call @VecWideCondBitsel
//
static bool convertSelectICmpToVecBitsel(Module &M, SelectInst *Select,
                                         ICmpInst *ICmp,
                                         ICmpInst::Predicate &Pred,
                                         Value *ICmpOp0, Value *ICmpOp1,
                                         Value *SelectOp0, Value *SelectOp1)
{
    auto *ICmpVecTy = dyn_cast<VectorType>(ICmpOp0->getType());
    auto *SelectVecTy = dyn_cast<VectorType>(Select->getType());
    if (!ICmpVecTy or !SelectVecTy) {
        return false;
    }

    Instruction *Cmp = ICmp;
    {
        IRBuilder<> Builder(Cmp);
        FunctionCallee Fn =
            pseudoInstFunction(M, VecCompare, ICmpVecTy,
                               {Builder.getInt32Ty(), ICmpVecTy, ICmpVecTy});
        ICmpInst::Predicate Pred = ICmp->getPredicate();
        CallInst *Call = Builder.CreateCall(
            Fn,
            {ConstantInt::get(Builder.getInt32Ty(), Pred), ICmpOp0, ICmpOp1});
        Cmp = Call;
    }

    unsigned SrcWidth = ICmpVecTy->getElementType()->getIntegerBitWidth();
    unsigned DstWidth = SelectVecTy->getElementType()->getIntegerBitWidth();

    if (SrcWidth < DstWidth) {
        IRBuilder<> Builder(Select);
        Value *ZExt = Builder.CreateSExt(Cmp, SelectVecTy);
        FunctionCallee Fn =
            pseudoInstFunction(M, VecWideCondBitsel, SelectVecTy,
                               {SelectVecTy, SelectVecTy, SelectVecTy});
        CallInst *Call = Builder.CreateCall(Fn, {ZExt, SelectOp0, SelectOp1});
        Select->replaceAllUsesWith(Call);
    } else if (SrcWidth > DstWidth) {
        IRBuilder<> Builder(Select);
        Value *ZExt = Builder.CreateTrunc(Cmp, SelectVecTy);
        FunctionCallee Fn =
            pseudoInstFunction(M, VecWideCondBitsel, SelectVecTy,
                               {SelectVecTy, SelectVecTy, SelectVecTy});
        CallInst *Call = Builder.CreateCall(Fn, {ZExt, SelectOp0, SelectOp1});
        Select->replaceAllUsesWith(Call);
    } else {
        IRBuilder<> Builder(Select);
        FunctionCallee Fn =
            pseudoInstFunction(M, VecWideCondBitsel, SelectVecTy,
                               {SelectVecTy, SelectVecTy, SelectVecTy});
        CallInst *Call = Builder.CreateCall(Fn, {Cmp, SelectOp0, SelectOp1});
        Select->replaceAllUsesWith(Call);
    }

    return true;
}

// Convert
//
//   %2 = icmp [sgt|ugt|slt|ult] %0, %1
//   %5 = select %2, %3, %4
//
// to
//
//   5 = call @Movcond.[cond].*(%1, %0, %3, %4)
//
// to more closely match TCG semantics.
static bool convertSelectICmpToMovcond(Module &M, SelectInst *Select,
                                       ICmpInst *ICmp,
                                       ICmpInst::Predicate &Pred,
                                       Value *ICmpOp0, Value *ICmpOp1,
                                       Value *SelectOp0, Value *SelectOp1)
{
    // We only handle integers, we have no movcond equivalent in gvec
    auto *IntTy = dyn_cast<IntegerType>(Select->getType());
    if (!IntTy) {
        return false;
    }

    // If the type of the comparison does not match the return type of the
    // select statement, we cannot do anything so skip
    if (ICmpOp0->getType() != IntTy) {
        return false;
    }

    IRBuilder<> Builder(Select);
    if (cast<IntegerType>(ICmpOp0->getType())->getBitWidth() <
        IntTy->getBitWidth()) {
        if (ICmp->isSigned(Pred)) {
            ICmpOp0 = Builder.CreateSExt(ICmpOp0, IntTy);
            ICmpOp1 = Builder.CreateSExt(ICmpOp1, IntTy);
        } else {
            ICmpOp0 = Builder.CreateZExt(ICmpOp0, IntTy);
            ICmpOp1 = Builder.CreateZExt(ICmpOp1, IntTy);
        }
    }

    // Create @Movcond.[slt|...].* function
    FunctionCallee Fn = pseudoInstFunction(M, Movcond, IntTy,
                                           {IntTy, IntTy, IntTy, IntTy, IntTy});
    CallInst *Call =
        Builder.CreateCall(Fn, {ConstantInt::get(IntTy, Pred), ICmpOp0, ICmpOp1,
                                SelectOp0, SelectOp1});
    Select->replaceAllUsesWith(Call);

    return true;
}

// Specialize
//
//   %2 = icmp [sgt|ugt|slt|ult] %0, %1
//   %5 = select %2, %3, %4
//
// to either maximum/minimum, vector operations matching TCG, or a conditional
// move that also matches TCG in sematics.
static void convertSelectICmp(Module &M, SelectInst *Select, ICmpInst *ICmp)
{
    // Given
    //   %2 = icmp [sgt|ugt|slt|ult] %0, %1
    //   %5 = select %2, %3, %4
    assert(Select->getCondition() == ICmp);
    Value *ICmpOp0 = ICmp->getOperand(0);
    Value *ICmpOp1 = ICmp->getOperand(1);
    Value *SelectOp0 = Select->getTrueValue();
    Value *SelectOp1 = Select->getFalseValue();
    ICmpInst::Predicate Pred = ICmp->getPredicate();

    // First try to convert to min/max
    //   %5 = [s|u][max|min] %0, %1
    if (convertSelectICmpToMinMax(M, Select, ICmp, Pred, ICmpOp0, ICmpOp1,
                                  SelectOp0, SelectOp1)) {
        return;
    }

    // Secondly try convert icmp -> @VecCompare, select -> @VecWideCondBitsel
    if (convertSelectICmpToVecBitsel(M, Select, ICmp, Pred, ICmpOp0, ICmpOp1,
                                     SelectOp0, SelectOp1)) {
        return;
    }

    // If min/max and vector conversion failed we fallback to a movcond
    //   %5 = call @Movcond.[cond].*(%1, %0, %3, %4)
    convertSelectICmpToMovcond(M, Select, ICmp, Pred, ICmpOp0, ICmpOp1,
                               SelectOp0, SelectOp1);
}

// Convert QEMU guest loads/stores represented by calls such as
//
//   call cpu_ldub*(),
//   call cpu_stb*(),
//
// and friends, to pseudo instructions
//
//   %5 = call @GuestLoad.*(%addr, %sign, %size, %endian);
//   %5 = call @GuestStore.*(%addr, %value, %size, %endian);
//
// Makes the backend agnostic to what instructions or calls are used to
// represent loads and stores.
static void convertQemuLoadStoreToPseudoInst(Module &M, CallInst *Call)
{
    Function *F = Call->getCalledFunction();
    StringRef Name = F->getName();
    if (Name.consume_front("cpu_")) {
        bool IsLoad = Name.consume_front("ld");
        bool IsStore = !IsLoad and Name.consume_front("st");
        if (IsLoad or IsStore) {
            bool Signed = !Name.consume_front("u");
            uint8_t Size = 0;
            switch (Name[0]) {
            case 'b':
                Size = 1;
                break;
            case 'w':
                Size = 2;
                break;
            case 'l':
                Size = 4;
                break;
            case 'q':
                Size = 8;
                break;
            default:
                abort();
            }

            uint8_t Endianness = 0; // unknown
            if (Size > 1) {
                Name = Name.drop_front(2);
                switch (Name[0]) {
                case 'l':
                    Endianness = 1;
                    break;
                case 'b':
                    Endianness = 2;
                    break;
                default:
                    abort();
                }
            }

            IRBuilder<> Builder(Call);
            Value *AddrOp = Call->getArgOperand(1);
            IntegerType *AddrTy = cast<IntegerType>(AddrOp->getType());
            IntegerType *FlagTy = Builder.getInt8Ty();
            Value *SizeOp = ConstantInt::get(FlagTy, Size);
            Value *EndianOp = ConstantInt::get(FlagTy, Endianness);
            CallInst *NewCall;
            if (IsLoad) {
                Value *SignOp = ConstantInt::get(FlagTy, Signed);
                IntegerType *RetTy = cast<IntegerType>(Call->getType());
                FunctionCallee Fn = pseudoInstFunction(
                    M, GuestLoad, RetTy, {AddrTy, FlagTy, FlagTy, FlagTy});
                NewCall =
                    Builder.CreateCall(Fn, {AddrOp, SignOp, SizeOp, EndianOp});
            } else {
                Value *ValueOp = Call->getArgOperand(2);
                IntegerType *ValueTy = cast<IntegerType>(ValueOp->getType());
                FunctionCallee Fn =
                    pseudoInstFunction(M, GuestStore, Builder.getVoidTy(),
                                       {AddrTy, ValueTy, FlagTy, FlagTy});
                NewCall =
                    Builder.CreateCall(Fn, {AddrOp, ValueOp, SizeOp, EndianOp});
            }
            Call->replaceAllUsesWith(NewCall);
        }
    }
}

// Convert QEMU exception calls
//
//   call raise_exception_ra(...),
//   ...
//
// to a pseudo instruction
//
//   %5 = call @Exception.*(...);
//
// Makes the backend agnostic to what instructions or calls are used to
// represent exceptions, and the list of sources can be expanded here.
static void convertExceptionCallsToPseudoInst(Module &M, CallInst *Call)
{
    Function *F = Call->getCalledFunction();
    StringRef Name = F->getName();
    // NOTE: expand as needed
    if (Name == "raise_exception_ra") {
        IRBuilder<> Builder(Call);
        Value *Op0 = Call->getArgOperand(0);
        Value *Op1 = Call->getArgOperand(1);
        FunctionCallee Fn =
            pseudoInstFunction(M, Exception, Builder.getVoidTy(),
                               {Op0->getType(), Op1->getType()});
        CallInst *NewCall = Builder.CreateCall(Fn, {Op0, Op1});
        Call->replaceAllUsesWith(NewCall);
    }
}

//
// Following functions help with converting between different types of
// instructions to pseudo instructions, particularly ones that write
// to a pointer, aka the Vec*Store pseudo instructions
//

static PseudoInst instructionToStorePseudoInst(unsigned Opcode)
{
    switch (Opcode) {
    case Instruction::Trunc:
        return VecTruncStore;
    case Instruction::ZExt:
        return VecZExtStore;
    case Instruction::SExt:
        return VecSExtStore;
    case Instruction::Select:
        return VecSelectStore;
    case Instruction::Add:
        return VecAddStore;
    case Instruction::Sub:
        return VecSubStore;
    case Instruction::Mul:
        return VecMulStore;
    case Instruction::Xor:
        return VecXorStore;
    case Instruction::Or:
        return VecOrStore;
    case Instruction::And:
        return VecAndStore;
    case Instruction::Shl:
        return VecShlStore;
    case Instruction::LShr:
        return VecLShrStore;
    case Instruction::AShr:
        return VecAShrStore;
    default:
        abort();
    }
}

static PseudoInst pseudoInstToStorePseudoInst(PseudoInst Inst)
{
    switch (Inst) {
    case VecNot:
        return VecNotStore;
    case VecAddScalar:
        return VecAddScalarStore;
    case VecSubScalar:
        return VecSubScalarStore;
    case VecMulScalar:
        return VecMulScalarStore;
    case VecXorScalar:
        return VecXorScalarStore;
    case VecOrScalar:
        return VecOrScalarStore;
    case VecAndScalar:
        return VecAndScalarStore;
    case VecShlScalar:
        return VecShlScalarStore;
    case VecLShrScalar:
        return VecLShrScalarStore;
    case VecAShrScalar:
        return VecAShrScalarStore;
    case VecWideCondBitsel:
        return VecWideCondBitselStore;
    default:
        abort();
    }
}

static PseudoInst intrinsicToStorePseudoInst(unsigned IntrinsicID)
{
    switch (IntrinsicID) {
    case Intrinsic::sadd_sat:
        return VecSignedSatAddStore;
    case Intrinsic::ssub_sat:
        return VecSignedSatSubStore;
    case Intrinsic::fshr:
        return VecFunnelShrStore;
#if LLVM_VERSION_MAJOR > 11
    case Intrinsic::abs:
        return VecAbsStore;
    case Intrinsic::smax:
        return VecSignedMaxStore;
    case Intrinsic::umax:
        return VecUnsignedMaxStore;
    case Intrinsic::smin:
        return VecSignedMinStore;
    case Intrinsic::umin:
        return VecUnsignedMinStore;
#endif
    case Intrinsic::ctlz:
        return VecCtlzStore;
    case Intrinsic::cttz:
        return VecCttzStore;
    case Intrinsic::ctpop:
        return VecCtpopStore;
    default:
        abort();
    }
}

// For binary/unary ops on vectors where the result is stored to a
// pointer
//
//   %3 = <NxM> %1 [op] <NxM> %2
//   %4 = bitcast i8* %0 to <NxM>*
//   store <NxM> %3, <NxM>* %4
//
// to
//
//   call @Vec[Op]Store.*(%0, %1, %2)
//
// This deals with the duality of pointers and vectors, and
// simplifies the backend.  We previously kept a map on the
// side to propagate "vector"-ness from %3 to %4 via the store,
// no longer!
static void convertVecStoreBitcastToPseudoInst(EraseInstVec &InstToErase,
                                               Module &M, StoreInst *Store)
{
    Value *ValueOp = Store->getValueOperand();
    Type *ValueTy = ValueOp->getType();
    if (!ValueTy->isVectorTy()) {
        return;
    }
    auto *Bitcast = cast<BitCastInst>(Store->getPointerOperand());
    Type *PtrTy = Bitcast->getType();
    // Ensure store and binary op. are in the same basic
    // block since the op. is moved to the store.
    bool InSameBB =
        cast<Instruction>(ValueOp)->getParent() == Store->getParent();
    if (!InSameBB) {
        return;
    }

    SmallVector<Type *, 3> Types;
    SmallVector<Value *, 3> Args;
    Value *PtrOp = Store->getPointerOperand();
    if (auto *BinOp = dyn_cast<BinaryOperator>(ValueOp)) {
        Instruction *Inst = cast<Instruction>(ValueOp);
        PseudoInst NewInst = instructionToStorePseudoInst(BinOp->getOpcode());
        IRBuilder<> Builder(Store);
        const unsigned ArgCount = pseudoInstArgCount(NewInst);
        // Add one to account for extra store pointer
        // argument of Vec*Store pseudo instructions.
        assert(ArgCount > 0 and ArgCount - 1 <= Inst->getNumOperands());
        Types.push_back(PtrTy);
        Args.push_back(PtrOp);
        for (unsigned I = 0; I < ArgCount - 1; ++I) {
            Value *Op = Inst->getOperand(I);
            Types.push_back(Op->getType());
            Args.push_back(Op);
        }
        FunctionCallee Fn =
            pseudoInstFunction(M, NewInst, Builder.getVoidTy(), Types);
        Builder.CreateCall(Fn, Args);
    } else if (auto *Call = dyn_cast<CallInst>(ValueOp)) {
        Function *F = Call->getCalledFunction();
        PseudoInst OldInst = getPseudoInstFromCall(Call);
        if (OldInst != InvalidPseudoInst) {
            // Map scalar vector pseudo instructions to
            // store variants
            PseudoInst NewInst = pseudoInstToStorePseudoInst(OldInst);
            IRBuilder<> Builder(Store);
            Types.push_back(PtrTy);
            Args.push_back(PtrOp);
            for (Value *Op : Call->args()) {
                Types.push_back(Op->getType());
                Args.push_back(Op);
            }
            FunctionCallee Fn =
                pseudoInstFunction(M, NewInst, Builder.getVoidTy(), Types);
            Builder.CreateCall(Fn, Args);
        } else if (F->isIntrinsic()) {
            Instruction *Inst = cast<Instruction>(ValueOp);
            PseudoInst NewInst =
                intrinsicToStorePseudoInst(F->getIntrinsicID());
            const unsigned ArgCount = pseudoInstArgCount(NewInst);
            // Add one to account for extra store pointer
            // argument of Vec*Store pseudo instructions.
            assert(ArgCount > 0 and ArgCount - 1 <= Inst->getNumOperands());
            IRBuilder<> Builder(Store);
            SmallVector<Type *, 8> ArgTys;
            SmallVector<Value *, 8> Args;
            ArgTys.push_back(PtrTy);
            Args.push_back(PtrOp);
            for (unsigned I = 0; I < ArgCount - 1; ++I) {
                Value *Op = Inst->getOperand(I);
                ArgTys.push_back(Op->getType());
                Args.push_back(Op);
            }
            FunctionCallee Fn =
                pseudoInstFunction(M, NewInst, Builder.getVoidTy(), ArgTys);
            Builder.CreateCall(Fn, Args);
        } else {
            dbgs() << "Uhandled vector + bitcast + store op. " << *ValueOp
                   << "\n";
            abort();
        }
    } else {
        Instruction *Inst = cast<Instruction>(ValueOp);
        PseudoInst NewInst = instructionToStorePseudoInst(Inst->getOpcode());
        const unsigned ArgCount = pseudoInstArgCount(NewInst);
        // Add one to account for extra store pointer
        // argument of Vec*Store pseudo instructions.
        assert(ArgCount > 0 and ArgCount - 1 <= Inst->getNumOperands());
        IRBuilder<> Builder(Store);
        SmallVector<Type *, 8> ArgTys;
        SmallVector<Value *, 8> Args;
        ArgTys.push_back(PtrTy);
        Args.push_back(PtrOp);
        for (unsigned I = 0; I < ArgCount - 1; ++I) {
            Value *Op = Inst->getOperand(I);
            ArgTys.push_back(Op->getType());
            Args.push_back(Op);
        }
        FunctionCallee Fn =
            pseudoInstFunction(M, NewInst, Builder.getVoidTy(), ArgTys);
        Builder.CreateCall(Fn, Args);
    }

    // Remove store instruction, this ensures DCE
    // can cleanup the rest, we also remove ValueOp
    // here since it's a call and won't get cleaned
    // by DCE.
    InstToErase.push_back(cast<Instruction>(ValueOp));
    InstToErase.push_back(Store);
}

//
// Convert
//
//   %cond = icmp [cond] i32 %0, i32 %1
//   br i1 %cond, label %true, label %false
//
// to
//
//   call void @brcond.[cond].i32(i32 %0, i32 %1, label %true.exit,
//   label %false) br i1 %cond, label %true, label %false !dead-branch
//
// note the old branch still remains as @brcond.* is not an actual
// branch instruction. Removing the old branch would result in broken
// IR.
//
// Additionally if the %false basic block immediatly succeeds the
// current one, we can ignore the false branch and fallthrough, this is
// indicated via !fallthrough metadata on the call.
//
// TODO: Consider using a ConstantInt i1 arguments instead. Metadata is
// fragile and does not survive optimization. We do not run any more
// optimization passes, but this could be a source of future headache.
static void convertICmpBrToPseudInst(LLVMContext &Context,
                                     EraseInstVec &InstToErase, Module &M,
                                     Instruction *I, BasicBlock *NextBb)
{
    auto *ICmp = dyn_cast<ICmpInst>(I);
    if (!ICmp) {
        return;
    }

    // Since we want to remove the icmp instruction we ensure that
    // all uses are branch instructions that can be converted into
    // @brcond.* calls.
    for (User *U : ICmp->users()) {
        if (!isa<BranchInst>(U)) {
            return;
        }
    }

    Value *Op0 = ICmp->getOperand(0);
    Value *Op1 = ICmp->getOperand(1);
    auto *CmpIntTy = dyn_cast<IntegerType>(Op0->getType());
    if (!CmpIntTy) {
        return;
    }
    for (User *U : ICmp->users()) {
        auto *Br = cast<BranchInst>(U);

        BasicBlock *True = Br->getSuccessor(0);
        BasicBlock *False = Br->getSuccessor(1);

        bool TrueUnreachable =
            True->getTerminator()->getOpcode() == Instruction::Unreachable and
            False->getTerminator()->getOpcode() != Instruction::Unreachable;

        // If the next basic block is either of our true/false
        // branches, we can fallthrough instead of branching.
        bool Fallthrough = (NextBb == True or NextBb == False);

        // If the succeeding basic block is the true branch we
        // invert the condition so we can !fallthrough instead.
        ICmpInst::Predicate Predicate;
        if (NextBb == True or (TrueUnreachable and NextBb == False)) {
            std::swap(True, False);
            Predicate = ICmp->getInversePredicate();
        } else {
            Predicate = ICmp->getPredicate();
        }

        IRBuilder<> Builder(Br);
        FunctionCallee Fn = pseudoInstFunction(
            M, Brcond, Builder.getVoidTy(),
            {CmpIntTy, CmpIntTy, CmpIntTy, True->getType(), False->getType()});
        CallInst *Call = Builder.CreateCall(
            Fn, {ConstantInt::get(CmpIntTy, Predicate), Op0, Op1, True, False});

        if (Fallthrough) {
            MDTuple *N = MDNode::get(Context, MDString::get(Context, ""));
            Call->setMetadata("fallthrough", N);
        }

        //
        // We need to keep the BB of the true branch alive
        // so that we can iterate over the CFG as usual
        // using LLVM. Or custom "opcode" @brcond is not an
        // actual branch, so LLVM does not understand that
        // we can branch to the true branch.
        //
        // For this reason we emit an extra dead branch
        // to the true branch, and tag it as dead using
        // metadata. The backend can later check that if
        // this metadata is present and ignore the branch.
        //
        // Another idea:
        //    What we could do instead is to
        //    linearize the CFG before this point, i.e.
        //    establish the order we want to emit all BBs
        //    in, in say an array. We can then iterate
        //    over this array instead, note this can only
        //    happen in the later stages of the pipeline
        //    where we don't rely on LLVM for any extra work.
        //
        //    Keeping our own linear array would also allow
        //    us to optimize brconds for fallthroughs, e.g.
        //    check if any of the basic blocks we branch to
        //    is the next basic block, and if so we can adjust
        //    the condition accordingly.
        //    (We do this currently, but this assumes the
        //    iteration order here is the same as in the
        //    backend.)
        //
        // Note also: LLVM expectects the BB to end in a single
        // branch.
        //
        BranchInst *DeadBranch =
            Builder.CreateCondBr(ConstantInt::getFalse(Context), True, False);
        {
            MDTuple *N = MDNode::get(Context, MDString::get(Context, ""));
            DeadBranch->setMetadata("dead-branch", N);
        }

        InstToErase.push_back(Br);
    }
    InstToErase.push_back(ICmp);
}
