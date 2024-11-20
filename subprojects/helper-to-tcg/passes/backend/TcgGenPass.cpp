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

#include "TcgGenPass.h"
#include "CmdLineOptions.h"
#include "Error.h"
#include "FunctionAnnotation.h"
#include "PseudoInst.h"
#include "TcgEmit.h"
#include "TcgTempAllocationPass.h"
#include "TcgType.h"
#include "llvm-compat.h"

#include <llvm/ADT/Optional.h>
#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallBitVector.h>
#include <llvm/Analysis/CallGraph.h>
#include <llvm/Demangle/Demangle.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

// For std::swap
#include <algorithm>

using namespace llvm;

// Wrapper class around a TcgV to cast it to/from 32-/64-bit
class TcgSizeAdapter
{
    raw_ostream &Out;
    const TcgV Orig;
    Optional<TcgV> Adapted;

  public:
    TcgSizeAdapter(raw_ostream &Out, const TcgV Orig) : Out(Out), Orig(Orig) {}

    const TcgV get(uint32_t Size)
    {
        if (Orig.Kind == IrImmediate or (Orig.TcgSize == Size)) {
            return Orig;
        } else if (!Adapted.hasValue()) {
            initAdapted(Size);
        }
        return *Adapted;
    }

  private:
    void initAdapted(uint32_t Size)
    {
        assert(!Adapted.hasValue());
        assert((Size == 32 and Orig.TcgSize == 64) or
               (Size == 64 and Orig.TcgSize == 32));

        Adapted = TcgV::makeTemp(Size, Orig.LlvmSize, Orig.Kind);
        tcg::defineNewTemp(Out, *Adapted);
        if (Size == 32) {
            tcg::genExtrlI64I32(Out, *Adapted, Orig);
        } else {
            tcg::genExtuI32I64(Out, *Adapted, Orig);
        }
    }
};

class Mapper
{
    raw_ostream &Out;
    llvm::DenseMap<const Value *, TcgV> Map;
    llvm::DenseMap<const BasicBlock *, TcgV> Labels;

    // Keep track of whether a TcgV has been defined already, or not
    SmallBitVector HasBeenDefined;

    const TempAllocationData &TAD;

  public:
    Mapper(raw_ostream &Out, const TcgGlobalMap &TcgGlobals, const Module &M,
           const TempAllocationData &TAD)
        : Out(Out), TAD(TAD)
    {
        // Default to size of previously mapped TcgVs
        HasBeenDefined.resize(TAD.Map.size());
    }

    Expected<TcgV> getMapped(const Value *V)
    {
        auto It = Map.find(V);
        if (It != Map.end()) {
            return It->second;
        }
        return mkError("Value not mapped");
    }

    TcgV mapBbAndEmit(BasicBlock *BB)
    {
        auto Find = Labels.find(BB);
        if (Find == Labels.end()) {
            TcgV Label = TcgV::makeLabel();
            tcg::defineNewTemp(Out, Label);
            return Labels.try_emplace(BB, Label).first->second;
        }
        return Find->second;
    }

    void mapExplicitly(Value *Val, const TcgV &TcgVal)
    {
        assert(Map.find(Val) == Map.end());
        Map.try_emplace(Val, TcgVal);
    }

    void mapClear(Value *Val)
    {
        auto It = Map.find(Val);
        assert(It != Map.end());
        Map.erase(It);
    }

    Expected<TcgV> mapAndEmit(const Value *V)
    {
        auto Mapped = getMapped(V);
        if (Mapped) {
            return Mapped.get();
        }

        auto It = TAD.Map.find(V);
        if (It == TAD.Map.end()) {
            return mkError("Unable to map value: ", V);
        }

        const TcgV Tcg = It->second;

        bool IsArg = TAD.Args.ArgInfoMap.find(V) != TAD.Args.ArgInfoMap.end();

        if (Tcg.Id >= HasBeenDefined.size()) {
            HasBeenDefined.resize(Tcg.Id + 1);
        }

        if (!IsArg and !HasBeenDefined[Tcg.Id] and
            (!TAD.ReturnValue.hasValue() or Tcg != *TAD.ReturnValue) and
            Tcg.Kind != IrImmediate and Tcg.Kind != IrConst) {
            HasBeenDefined.set(Tcg.Id);
            tcg::defineNewTemp(Out, Tcg);
        }

        // Logic for emitted TCG corresponding to constant LLVM vectors, two
        // cases are handled, splatted values
        //
        //   <NxiM> <iM 1, iM 1, ..., iM 1>
        //
        // and vectors where elements differ
        //
        //   <NxiM> <iM 1, iM 2, ..., iM 16>
        //
        // For the latter case, attemt to emit it as a constant splatted
        // vector with a larger size by combining adjacent elements. This
        // is an optimization as initialzing a constant vector with different
        // elements is expensive compared to splatting.
        auto ConstV = dyn_cast<Constant>(V);
        if (ConstV and V->getType()->isVectorTy()) {
            Constant *Splat = ConstV->getSplatValue();
            if (Splat) {
                // Constant splatted vector
                auto It = TAD.Map.find(Splat);
                assert(It != TAD.Map.end());
                auto Size = TcgV::makeImmediate(
                    Twine(vectorSizeInBytes(Tcg)).str(), 64, 64);
                tcg::genVecMemset(Out, Tcg, It->second, Size);
            } else {
                // Constant non-splatted vector, attempt to combine elements
                // to make it splattable.
                SmallVector<uint64_t, 16> Ints;

                // Copy over elements to a vector
                for (unsigned I = 0; I < Tcg.VectorElementCount; ++I) {
                    Constant *Element = ConstV->getAggregateElement(I);
                    uint64_t Value = Element->getUniqueInteger().getZExtValue();
                    Ints.push_back(Value);
                }

                // When combining adjacent elements, the maximum size supported
                // by TCG is 64-bit.  MaxNumElements is the maximum amount of
                // elements to attempt to merge
                size_t PatternLen = 0;
                unsigned MaxNumElements = 8 * sizeof(uint64_t) / Tcg.LlvmSize;
                for (unsigned N = MaxNumElements; N > 1; N /= 2) {
                    // Attempt to combine N elements by checking if the first
                    // N elements tile the vector.
                    bool Match = true;
                    for (unsigned J = 0; J < Tcg.VectorElementCount; ++J) {
                        if (Ints[J % N] != Ints[J]) {
                            Match = false;
                            break;
                        }
                    }
                    // If tiling succeeded, break out
                    if (Match) {
                        PatternLen = N;
                        break;
                    }
                }

                if (PatternLen > 0) {
                    // Managed to tile vector with splattable element, compute
                    // final splattable value
                    uint64_t Value = 0;
                    for (unsigned I = 0; I < PatternLen; ++I) {
                        Value |= Ints[I] << I * Tcg.LlvmSize;
                    }
                    auto Splat =
                        TcgV::makeImmediate(Twine(Value).str(), 64, 64);
                    auto Size = TcgV::makeImmediate(
                        Twine(vectorSizeInBytes(Tcg)).str(), 64, 64);
                    tcg::genVecMemset(Out, Tcg, Splat, Size);
                } else {
                    // Tiling failed, fall back to emitting an array copy from
                    // C to a gvec vector.
                    SmallVector<TcgV, 16> Arr;
                    for (unsigned I = 0; I < Tcg.VectorElementCount; ++I) {
                        Constant *Element = ConstV->getAggregateElement(I);
                        auto It = TAD.Map.find(Element);
                        assert(It != TAD.Map.end());
                        Arr.push_back(It->second);
                    }
                    tcg::genVecArrSplat(Out, Tcg, Arr);
                }
            }
        }

        return Map.try_emplace(V, It->second).first->second;
    }

    Expected<TcgV> mapCondAndEmit(Value *V, uint32_t TcgSize, uint32_t LlvmSize)
    {
        auto Mapped = getMapped(V);
        if (Mapped) {
            assert(Mapped.get().LlvmSize == 1);
            return Mapped.get();
        }

        auto It = TAD.Map.find(const_cast<Value *>(V));
        if (It == TAD.Map.end()) {
            return mkError("Unable to map cond: ", V);
        }

        const TcgV Tcg = It->second;
        if (Tcg.Id >= HasBeenDefined.size()) {
            HasBeenDefined.resize(Tcg.Id + 1);
        }
        if (!HasBeenDefined[Tcg.Id] and
            (!TAD.ReturnValue.hasValue() or Tcg != *TAD.ReturnValue)) {
            HasBeenDefined.set(Tcg.Id);
            tcg::defineNewTemp(Out, Tcg);
        }
        return Map.try_emplace(V, It->second).first->second;
    }
};

struct TranslatedFunction {
    std::string Name;
    std::string Decl;
    std::string Code;
    std::string DispatchCode;
    bool IsHelper;
};

static void ensureSignBitIsSet(raw_ostream &Out, const TcgV &V)
{
    if (V.LlvmSize == V.TcgSize or V.Kind != IrValue) {
        return;
    }
    tcg::genExtract(Out, true, V, V,
                    TcgV::makeImmediate("0", V.TcgSize, V.LlvmSize),
                    TcgV::makeImmediate(Twine((int)V.LlvmSize).str(), V.TcgSize,
                                        V.LlvmSize));
}

static Expected<TcgV> mapCallReturnValue(Mapper &Mapper, CallInst *Call)
{
    // Only map return value if it has > 0 uses.  Destination values of call
    // instructions are the only ones which LLVM will not remove if unused.
    if (Call->getType()->isVoidTy() or Call->getNumUses() == 0) {
        return mkError("Invalid return type", Call);
    }
    return Mapper.mapAndEmit(Call);
}

static Instruction::BinaryOps mapPseudoInstToOpcode(PseudoInst Inst)
{
    switch (Inst) {
    case VecAddScalar:
    case VecAddStore:
    case VecAddScalarStore:
        return Instruction::Add;
    case VecSubScalar:
    case VecSubStore:
    case VecSubScalarStore:
        return Instruction::Sub;
    case VecMulScalar:
    case VecMulStore:
    case VecMulScalarStore:
        return Instruction::Mul;
    case VecXorScalar:
    case VecXorStore:
    case VecXorScalarStore:
        return Instruction::Xor;
    case VecOrScalar:
    case VecOrStore:
    case VecOrScalarStore:
        return Instruction::Or;
    case VecAndScalar:
    case VecAndStore:
    case VecAndScalarStore:
        return Instruction::And;
    case VecShlScalar:
    case VecShlStore:
    case VecShlScalarStore:
        return Instruction::Shl;
    case VecLShrScalar:
    case VecLShrStore:
    case VecLShrScalarStore:
        return Instruction::LShr;
    case VecAShrScalar:
    case VecAShrStore:
    case VecAShrScalarStore:
        return Instruction::AShr;
    default:
        abort();
    }
}

static bool translatePseudoInstCall(raw_ostream &Out, CallInst *Call,
                                    PseudoInst PInst,
                                    const SmallVector<TcgV, 4> &Args,
                                    Mapper &Mapper,
                                    const TcgGlobalMap &TcgGlobals)
{
    switch (PInst) {
    case IdentityMap: {
        Mapper.mapExplicitly(Call, Args[0]);
    } break;
    case PtrAdd: {
        if (Args[0].Kind == IrPtr or Args[0].Kind == IrEnv) {
            Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
            if (!MaybeRes) {
                return false;
            }
            tcg::genAddPtr(Out, *MaybeRes, Args[0], Args[1]);
        } else if ((Args[0].Kind == IrImmediate or Args[0].Kind == IrConst) and
                   (Args[1].Kind == IrConst or Args[1].Kind == IrImmediate)) {
            Mapper.mapExplicitly(Call, c::ptrAdd(Args[0], Args[1]));
        } else if (Args[0].Kind == IrPtrToOffset and
                   (Args[1].Kind == IrConst or Args[1].Kind == IrImmediate)) {
            Mapper.mapExplicitly(Call, c::ptrAdd(Args[0], Args[1]));
        } else {
            // ptradd on vector types requires immediate offset
            return false;
        }
    } break;
    case AccessGlobalArray: {
        auto Offset = cast<ConstantInt>(Call->getArgOperand(0))->getZExtValue();
        auto It = TcgGlobals.find(Offset);
        assert(It != TcgGlobals.end());
        TcgGlobal Global = It->second;
        uint32_t LlvmSize = Global.Size;
        uint32_t TcgSize = llvmToTcgSize(LlvmSize);
        if (Args[1].Kind != IrImmediate) {
            // globalArray access with non-immediate index
            return false;
        }
        auto Code = Global.Code.str() + "[" + tcg::getName(Args[1]) + "]";
        auto Tcg =
            TcgV::makeConstantExpression(Code, TcgSize, LlvmSize, IrValue);
        Mapper.mapExplicitly(Call, Tcg);
    } break;
    case AccessGlobalValue: {
        auto Offset = cast<ConstantInt>(Call->getArgOperand(0))->getZExtValue();
        auto It = TcgGlobals.find(Offset);
        assert(It != TcgGlobals.end());
        TcgGlobal Global = It->second;
        auto LlvmSize = Global.Size;
        auto TcgSize = llvmToTcgSize(LlvmSize);
        auto Tcg = TcgV::makeConstantExpression(Global.Code.str(), TcgSize,
                                                LlvmSize, IrValue);
        Mapper.mapExplicitly(Call, Tcg);
    } break;
    case Brcond: {
        auto LlvmPred = static_cast<ICmpInst::Predicate>(
            cast<ConstantInt>(Call->getOperand(0))->getZExtValue());
        tcg::genBrcond(Out, LlvmPred, Args[1], Args[2], Args[3]);
        if (!Call->hasMetadata("fallthrough")) {
            tcg::genBr(Out, Args[4]);
        }
    } break;
    case Movcond: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        auto LlvmPred = static_cast<ICmpInst::Predicate>(
            cast<ConstantInt>(Call->getOperand(0))->getZExtValue());
        if (CmpInst::isSigned(LlvmPred)) {
            ensureSignBitIsSet(Out, Args[1]);
            ensureSignBitIsSet(Out, Args[2]);
        }
        tcg::genMovcond(Out, LlvmPred, *MaybeRes, Args[1], Args[2], Args[3],
                        Args[4]);
    } break;
    case VecSplat: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecSplat(Out, *MaybeRes, Args[0]);
    } break;
    case VecNot: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecNot(Out, *MaybeRes, Args[0]);
    } break;
    case VecNotStore: {
        tcg::genVecNot(Out, Args[0], Args[1]);
    } break;
    case VecAddScalar:
    case VecSubScalar:
    case VecMulScalar:
    case VecXorScalar:
    case VecOrScalar:
    case VecAndScalar:
    case VecShlScalar:
    case VecLShrScalar:
    case VecAShrScalar: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        auto Opcode = mapPseudoInstToOpcode(PInst);
        tcg::genVecBinOp(Out, Opcode, *MaybeRes, Args[0], Args[1]);
    } break;
    case VecAddStore:
    case VecSubStore:
    case VecMulStore:
    case VecXorStore:
    case VecOrStore:
    case VecAndStore:
    case VecShlStore:
    case VecLShrStore:
    case VecAShrStore:
    case VecAddScalarStore:
    case VecSubScalarStore:
    case VecMulScalarStore:
    case VecXorScalarStore:
    case VecOrScalarStore:
    case VecAndScalarStore:
    case VecShlScalarStore:
    case VecLShrScalarStore:
    case VecAShrScalarStore: {
        auto Opcode = mapPseudoInstToOpcode(PInst);
        tcg::genVecBinOp(Out, Opcode, Args[0], Args[1], Args[2]);
    } break;
    case VecSignedSatAddStore: {
        tcg::genVecSignedSatAdd(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecSignedSatSubStore: {
        tcg::genVecSignedSatSub(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecSelectStore: {
        tcg::genVecBitsel(Out, Args[0], Args[1], Args[2], Args[3]);
    } break;
    case VecAbsStore: {
        tcg::genAbs(Out, Args[0], Args[1]);
    } break;
    case VecSignedMaxStore: {
        tcg::genVecSignedMax(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecUnsignedMaxStore: {
        tcg::genVecUnsignedMax(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecSignedMinStore: {
        tcg::genVecSignedMin(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecUnsignedMinStore: {
        tcg::genVecUnsignedMin(Out, Args[0], Args[1], Args[2]);
    } break;
    case VecTruncStore: {
        tcg::genVecTrunc(Out, Args[0], Args[1]);
    } break;
    case VecCompare: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        auto LlvmPred = static_cast<ICmpInst::Predicate>(
            cast<ConstantInt>(Call->getOperand(0))->getZExtValue());
        tcg::genVecCmp(Out, MaybeRes.get(), LlvmPred, Args[1], Args[2]);
    } break;
    case VecWideCondBitsel: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecBitsel(Out, MaybeRes.get(), Args[0], Args[1], Args[2]);
        break;
    } break;
    case VecWideCondBitselStore: {
        tcg::genVecBitsel(Out, Args[0], Args[1], Args[2], Args[3]);
        break;
    } break;
    case GuestLoad: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        uint8_t Sign = cast<ConstantInt>(Call->getOperand(1))->getZExtValue();
        uint8_t Size = cast<ConstantInt>(Call->getOperand(2))->getZExtValue();
        uint8_t Endianness =
            cast<ConstantInt>(Call->getOperand(3))->getZExtValue();
        std::string MemOpStr = "MO_";
        raw_string_ostream MemOpStream(MemOpStr);
        switch (Endianness) {
        case 0:
            break; // do nothing
        case 1:
            MemOpStream << "LE";
            break;
        case 2:
            MemOpStream << "BE";
            break;
        default:
            abort();
        }
        switch (Sign) {
        case 0:
            MemOpStream << "U";
            break;
        case 1:
            MemOpStream << "S";
            break;
        default:
            abort();
        }
        switch (Size) {
        case 1:
            MemOpStream << "B";
            break;
        case 2:
            MemOpStream << "W";
            break;
        case 4:
            MemOpStream << "L";
            break;
        case 8:
            MemOpStream << "Q";
            break;
        default:
            abort();
        }
        tcg::genQemuLoad(Out, *MaybeRes, Args[0], MemOpStream.str().c_str());
    } break;
    case GuestStore: {
        uint8_t Size = cast<ConstantInt>(Call->getOperand(2))->getZExtValue();
        uint8_t Endianness =
            cast<ConstantInt>(Call->getOperand(3))->getZExtValue();
        std::string MemOpStr = "MO_";
        raw_string_ostream MemOpStream(MemOpStr);
        switch (Endianness) {
        case 0:
            break; // do nothing
        case 1:
            MemOpStream << "LE";
            break;
        case 2:
            MemOpStream << "BE";
            break;
        default:
            abort();
        }
        // Always unsigned for stores
        MemOpStream << "U";
        switch (Size) {
        case 1:
            MemOpStream << "B";
            break;
        case 2:
            MemOpStream << "W";
            break;
        case 4:
            MemOpStream << "L";
            break;
        case 8:
            MemOpStream << "Q";
            break;
        default:
            abort();
        }
        tcg::genQemuStore(Out, Args[0], Args[1], MemOpStream.str().c_str());
    } break;
    case Exception: {
        // Map and adapt arguments to the call
        SmallVector<TcgV, 8> IArgs;
        for (auto Arg : Args) {
            IArgs.push_back(tcg::materialize(Arg));
        }
        tcg::genCallHelper(Out, "helper_raise_exception", IArgs.begin(),
                           IArgs.end());
    } break;
    default:
        // unmapped pseudo inst
        return false;
    }
    return true;
}

static bool translateIntrinsicCall(raw_ostream &Out, CallInst *Call,
                                   Function *F,
                                   const SmallVector<TcgV, 4> &Args,
                                   Mapper &Mapper)
{
    switch (F->getIntrinsicID()) {
#if LLVM_VERSION_MAJOR > 11
    case Intrinsic::abs: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genAbs(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::smax: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecSignedMax(Out, *MaybeRes, Args[0], Args[1]);
    } break;
    case Intrinsic::smin: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecSignedMin(Out, *MaybeRes, Args[0], Args[1]);
    } break;
    case Intrinsic::umax: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecUnsignedMax(Out, *MaybeRes, Args[0], Args[1]);
    } break;
    case Intrinsic::umin: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecUnsignedMin(Out, *MaybeRes, Args[0], Args[1]);
    } break;
#endif
    case Intrinsic::sadd_sat: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecSignedSatAdd(Out, *MaybeRes, Args[0], Args[1]);
    } break;
    case Intrinsic::ssub_sat: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genVecSignedSatSub(Out, *MaybeRes, Args[0], Args[1]);
    } break;
    case Intrinsic::ctlz: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        if (Args[0].Kind == IrPtrToOffset) {
            // no gvec equivalent to clzi
            return false;
        }
        tcg::genCountLeadingZeros(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::cttz: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        if (Args[0].Kind == IrPtrToOffset) {
            // no gvec equivalent to ctti
            return false;
        }
        tcg::genCountTrailingZeros(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::ctpop: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        if (Args[0].Kind == IrPtrToOffset) {
            // no gvec equivalent to ctpop
            return false;
        }
        tcg::genCountOnes(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::bswap: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genByteswap(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::fshl: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genFunnelShl(Out, *MaybeRes, Args[0], Args[1], Args[2]);
    } break;
    case Intrinsic::bitreverse: {
        Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
        if (!MaybeRes) {
            return false;
        }
        tcg::genBitreverse(Out, *MaybeRes, Args[0]);
    } break;
    case Intrinsic::memcpy: {
        tcg::genVecMemcpy(Out, Args[0], Args[1], Args[2]);
    } break;
    case Intrinsic::memset: {
        tcg::genVecMemset(Out, Args[0], Args[1], Args[2]);
    } break;
    default:
        // Unhandled LLVM intrinsic
        return false;
    }
    return true;
}

static Expected<TranslatedFunction>
translateFunction(const Function *F, const TcgGlobalMap &TcgGlobals,
                  const AnnotationMapTy &Annotations,
                  const SmallPtrSet<Function *, 16> HasTranslatedFunction)
{
    TranslatedFunction TF = {
        .Name = F->getName().str(),
    };

    // Run TcgV register allocation
    Expected<TempAllocationData> MaybeTAD =
        allocateTemporaries(*F, Annotations);
    if (!MaybeTAD) {
        return MaybeTAD.takeError();
    }
    const TempAllocationData TAD = MaybeTAD.get();

    {
        StringRef NameRef(TF.Name);
        std::string DemangledFuncName = demangle(TF.Name);
        if (TF.Name != DemangledFuncName) {
            // If the function name changed when trying to demangle the name,
            // the name was mangled.  The resulting demangled name might look
            // something like
            //
            //   namespace::subnamespace::function(...)
            //
            // extract the function name, this assumes 0 name collisions in
            // the output.
            size_t Index = 0;
            NameRef = DemangledFuncName;
            // Remove namespaces
            Index = NameRef.find_last_of(':');
            if (Index != StringRef::npos) {
                NameRef = NameRef.substr(Index + 1);
            }
            // Remove arguments
            Index = NameRef.find_first_of('(');
            if (Index != StringRef::npos) {
                NameRef = NameRef.substr(0, Index);
            }
        }

        // Remove prefix for helper functions to get cleaner emitted names
        TF.IsHelper = NameRef.consume_front("helper_");
        TF.Name = NameRef.str();
    }

    raw_string_ostream Out(TF.Code);
    raw_string_ostream HeaderWriter(TF.Decl);

    raw_string_ostream DispatchWriter(TF.DispatchCode);
    std::string DispatchCall;
    raw_string_ostream DispatchCallWriter(DispatchCall);
    int dispatch_arg_count = 0;
    bool IsVectorInst = false;

    // Functions that should be ignored are convereted
    // to declarations, see FilterFunctionsPass.
    if (F->isDeclaration()) {
        return mkError("Function is not translated");
    }

    Mapper Mapper(Out, TcgGlobals, *F->getParent(), TAD);
    Optional<TcgV> RetVal = None;
    Out << "// " << *F->getReturnType() << ' ' << F->getName() << '\n';
    HeaderWriter << "void " << "emit_" << TF.Name << '(';
    SmallVector<TcgV, 4> CArgs;

    if (!F->getReturnType()->isVoidTy()) {
        assert(TAD.ReturnValue.hasValue());
        IsVectorInst = (*TAD.ReturnValue).Kind == IrPtrToOffset;
        CArgs.push_back(*TAD.ReturnValue);
    }

    for (const Value *Arg : TAD.Args.Args) {
        Expected<TcgV> MaybeMapped = Mapper.mapAndEmit(Arg);
        if (!MaybeMapped) {
            return mkError("failed mapping arg");
        }
        IsVectorInst |= (MaybeMapped.get().Kind == IrPtrToOffset);
        CArgs.push_back(MaybeMapped.get());
    }

    auto CArgIt = CArgs.begin();
    if (CArgIt != CArgs.end()) {
        HeaderWriter << tcg::getType(*CArgIt) << ' ' << tcg::getName(*CArgIt);
        ++CArgIt;
    }
    while (CArgIt != CArgs.end()) {
        HeaderWriter << ", " << tcg::getType(*CArgIt) << ' '
                     << tcg::getName(*CArgIt);
        ++CArgIt;
    }

    if (!IsVectorInst) {
        DispatchCallWriter << "emit_" << TF.Name << "(";
        auto CArgIt = CArgs.begin();
        if (CArgIt != CArgs.end()) {
            DispatchWriter << tcg::getType(*CArgIt) << ' '
                           << tcg::getName(*CArgIt) << " = ";
            if (TAD.ReturnValue and CArgIt->Id == (*TAD.ReturnValue).Id) {
                assert(CArgIt->Kind == IrValue);
                DispatchWriter << "temp_tcgv_i" << CArgIt->TcgSize
                               << "(ret_temp);\n";
            } else {
                switch (CArgIt->Kind) {
                case IrPtr:
                case IrEnv:
                    DispatchWriter << "temp_tcgv_ptr(args["
                                   << dispatch_arg_count++ << "]);\n";
                    break;
                case IrValue:
                    DispatchWriter << "temp_tcgv_i" << CArgIt->TcgSize
                                   << "(args[" << dispatch_arg_count++
                                   << "]);\n";
                    break;
                case IrImmediate:
                    DispatchWriter << "args[" << dispatch_arg_count++
                                   << "]->val;\n";
                    break;
                case IrPtrToOffset:
                    DispatchWriter << "args[" << dispatch_arg_count++
                                   << "]->val;\n";
                    break;
                default:
                    abort();
                };
            }
            DispatchCallWriter << tcg::getName(*CArgIt);
            ++CArgIt;
        }
        while (CArgIt != CArgs.end()) {
            DispatchWriter << tcg::getType(*CArgIt) << ' '
                           << tcg::getName(*CArgIt) << " = ";
            switch (CArgIt->Kind) {
            case IrPtr:
            case IrEnv:
                DispatchWriter << "temp_tcgv_ptr(args[" << dispatch_arg_count++
                               << "]);\n";
                break;
            case IrValue:
                DispatchWriter << "temp_tcgv_i" << CArgIt->TcgSize << "(args["
                               << dispatch_arg_count++ << "]);\n";
                break;
            case IrImmediate:
                DispatchWriter << "args[" << dispatch_arg_count++
                               << "]->val;\n";
                break;
            case IrPtrToOffset:
                DispatchWriter << "args[" << dispatch_arg_count++
                               << "]->val;\n";
                break;
            default:
                abort();
            };
            DispatchCallWriter << ", " << tcg::getName(*CArgIt);
            ++CArgIt;
        }
        DispatchCallWriter << ");\n";
        DispatchWriter << DispatchCallWriter.str();
    }

    // Copy over function declaration from header to source file
    HeaderWriter << ')';
    Out << HeaderWriter.str();
    Out << " {\n";
    HeaderWriter << ';';

    ReversePostOrderTraversal<Function *> RPOT((Function *)F);
    for (auto BBI = RPOT.begin(); BBI != RPOT.end(); ++BBI) {
        BasicBlock &BB = **BBI;

        // Set label if not first BB
        if (&BB != &F->getEntryBlock()) {
            TcgV Label = Mapper.mapBbAndEmit(&BB);
            tcg::genSetLabel(Out, Label);
        }

        // Emit TCG generators for the current BB
        for (Instruction &I : BB) {
            switch (I.getOpcode()) {
            case Instruction::Alloca: {
                auto Alloca = cast<AllocaInst>(&I);
                Expected<TcgV> Res = Mapper.mapAndEmit(Alloca);
                if (!Res) {
                    return Res.takeError();
                }
            } break;
            case Instruction::Br: {
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
                if (I.hasMetadata("dead-branch")) {
                    break;
                }

                auto Branch = cast<BranchInst>(&I);
                if (Branch->isConditional()) {
                    assert(Branch->getNumSuccessors() == 2);
                    Expected<TcgV> Condition =
                        Mapper.mapCondAndEmit(Branch->getCondition(), 32, 1);
                    if (!Condition)
                        return mkError("couldn't map brcond condition ",
                                       Branch->getCondition());
                    const TcgV CCondition = tcg::materialize(Condition.get());
                    const TcgV True =
                        Mapper.mapBbAndEmit(Branch->getSuccessor(0));
                    const TcgV False =
                        Mapper.mapBbAndEmit(Branch->getSuccessor(1));

                    // Jump if condition is != 0
                    auto Zero = TcgV::makeImmediate("0", CCondition.TcgSize, 1);
                    tcg::genBrcond(Out, CmpInst::Predicate::ICMP_NE, CCondition,
                                   Zero, True);
                    tcg::genBr(Out, False);
                } else {
                    const TcgV Label =
                        Mapper.mapBbAndEmit(Branch->getSuccessor(0));
                    tcg::genBr(Out, Label);
                }
            } break;
            case Instruction::SExt: {
                auto SExt = cast<SExtInst>(&I);

                Expected<TcgV> SrcVal = Mapper.mapAndEmit(SExt->getOperand(0));
                if (!SrcVal) {
                    return mkError("Couldn't map value ", SExt->getOperand(0));
                }
                if (SrcVal.get().Kind == IrImmediate) {
                    auto ResLlvmSize = SExt->getDestTy()->getIntegerBitWidth();
                    Mapper.mapExplicitly(&I,
                                         c::sext(SrcVal.get(), ResLlvmSize,
                                                 llvmToTcgSize(ResLlvmSize)));
                } else if (SrcVal.get().Kind == IrPtrToOffset) {
                    Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                    if (!Res) {
                        return Res.takeError();
                    }
                    tcg::genVecSext(Out, Res.get(), SrcVal.get());
                } else {
                    Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                    if (!Res) {
                        return Res.takeError();
                    }
                    if (Res.get().LlvmSize < 32) {
                        return mkError("sext to unsupported size: ", &I);
                    }
                    if (SrcVal.get().Kind == IrPtrToOffset) {
                        return mkError("sext on vector type not supported: ",
                                       &I);
                    }
                    if (SrcVal.get().LlvmSize > 1 and
                        SrcVal.get().LlvmSize < 32) {
                        // TODO: Here we are using the fact that we
                        // support (16,64), (8,64). Also, move to TcgEmit
                        auto FuncStr =
                            Twine("tcg_gen_ext")
                                .concat(std::to_string(SrcVal.get().LlvmSize))
                                .concat("s_i")
                                .concat(std::to_string(Res.get().TcgSize))
                                .str();
                        auto ASrcVal = TcgSizeAdapter(Out, SrcVal.get());
                        tcg::emitCallTcg(
                            Out, FuncStr,
                            {Res.get(), ASrcVal.get(Res.get().TcgSize)});
                    } else if (SrcVal.get().LlvmSize == 1 and
                               Res.get().TcgSize == 32) {
                        tcg::genMov(Out, Res.get(), SrcVal.get());
                    } else {
                        tcg::genExtI32I64(Out, Res.get(), SrcVal.get());
                    }
                }
            } break;
            case Instruction::ZExt: {
                auto ZExt = cast<ZExtInst>(&I);

                Expected<TcgV> SrcVal = Mapper.mapAndEmit(ZExt->getOperand(0));
                if (!SrcVal)
                    return mkError("Couldn't map value ", ZExt->getOperand(0));

                if (SrcVal.get().Kind == IrImmediate) {
                    auto ResLlvmSize = ZExt->getDestTy()->getIntegerBitWidth();
                    if (ResLlvmSize > 64) {
                        return mkError("128-bit integers not supported: ", &I);
                    }
                    Mapper.mapExplicitly(&I,
                                         c::zext(SrcVal.get(), ResLlvmSize,
                                                 llvmToTcgSize(ResLlvmSize)));
                    break;
                }

                auto *DestTy = ZExt->getDestTy();
                if (DestTy->isIntegerTy()) {
                    const uint32_t ResLlvmSize =
                        cast<IntegerType>(DestTy)->getIntegerBitWidth();
                    const uint32_t ResTcgSize = llvmToTcgSize(ResLlvmSize);
                    if (ResLlvmSize > 64) {
                        return mkError("Invalid size: ", &I);
                    }
                    const uint32_t SrcLlvmSize = SrcVal.get().LlvmSize;
                    const uint32_t SrcTcgSize = SrcVal.get().TcgSize;

                    Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                    if (!Res) {
                        return Res.takeError();
                    }
                    if (SrcTcgSize == ResTcgSize) {
                        tcg::genMov(Out, Res.get(), SrcVal.get());
                    } else if (SrcTcgSize > Res.get().TcgSize and
                               SrcLlvmSize == 1) {
                        // Paradoxically we may need to emit an extract
                        // instruction for when a zero extension is requested.
                        // This is to account for the fact that "booleans" in
                        // tcg can be both 64- and 32-bit. So for instance zext
                        // i1 -> i32, here i1 may actually be 64-bit.
                        tcg::genExtrlI64I32(Out, Res.get(), SrcVal.get());
                    } else {
                        tcg::genExtuI32I64(Out, Res.get(), SrcVal.get());
                    }
                } else if (DestTy->isVectorTy()) {
                    Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                    if (!Res) {
                        return Res.takeError();
                    }
                    tcg::genVecZext(Out, Res.get(), SrcVal.get());
                } else {
                    return mkError("Invalid TcgSize!");
                }
            } break;
            case Instruction::Trunc: {
                auto Trunc = cast<TruncInst>(&I);

                Expected<TcgV> SrcVal = Mapper.mapAndEmit(Trunc->getOperand(0));
                if (!SrcVal) {
                    return mkError("Couldn't map value ", Trunc->getOperand(0));
                }
                if (SrcVal.get().Kind == IrImmediate) {
                    Mapper.mapExplicitly(&I, SrcVal.get());
                    break;
                }

                Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                if (!Res) {
                    return Res.takeError();
                }
                if (Res.get().Kind == IrValue) {
                    if (SrcVal.get().TcgSize == 64) {
                        if (Res.get().LlvmSize == 32) {
                            // 64 -> 32
                            tcg::genExtrlI64I32(Out, Res.get(), SrcVal.get());
                        } else {
                            // 64 -> 16,8,1
                            TcgV MRes = Res.get();
                            TcgV MSrc = SrcVal.get();
                            auto Offset = TcgV::makeImmediate("0", MRes.TcgSize,
                                                              MRes.LlvmSize);
                            auto Size = TcgV::makeImmediate(
                                Twine((int)MRes.LlvmSize).str(), MRes.TcgSize,
                                MRes.LlvmSize);
                            auto Temp = TcgV::makeTemp(64, 64, IrValue);
                            tcg::defineNewTemp(Out, Temp);
                            tcg::genExtract(Out, false, Temp, MSrc, Offset,
                                            Size);
                            tcg::genExtrlI64I32(Out, MRes, Temp);
                        }
                    } else if (SrcVal.get().TcgSize == 32) {
                        // 32 -> 16,8,1
                        // 16 -> 8,1
                        //  8 -> 1
                        TcgV MRes = Res.get();
                        TcgV MSrc = SrcVal.get();
                        auto Offset = TcgV::makeImmediate("0", MRes.TcgSize,
                                                          MRes.LlvmSize);
                        auto Size =
                            TcgV::makeImmediate(Twine((int)MRes.LlvmSize).str(),
                                                MRes.TcgSize, MRes.LlvmSize);
                        tcg::genExtract(Out, false, MRes, MSrc, Offset, Size);
                    } else {
                        return mkError("Invalid TcgSize!");
                    }
                } else if (Res.get().Kind == IrPtrToOffset) {
                    tcg::genVecTrunc(Out, Res.get(), SrcVal.get());
                } else {
                    return mkError("Invalid TcgSize!");
                }
            } break;
            case Instruction::Add:
            case Instruction::And:
            case Instruction::AShr:
            case Instruction::LShr:
            case Instruction::Mul:
            case Instruction::UDiv:
            case Instruction::SDiv:
            case Instruction::Or:
            case Instruction::Shl:
            case Instruction::Sub:
            case Instruction::Xor: {
                auto Bin = cast<BinaryOperator>(&I);
                // Check we are working on integers
                Expected<TcgV> MaybeOp1 = Mapper.mapAndEmit(Bin->getOperand(0));
                if (!MaybeOp1) {
                    return MaybeOp1.takeError();
                }
                Expected<TcgV> MaybeOp2 = Mapper.mapAndEmit(Bin->getOperand(1));
                if (!MaybeOp2) {
                    return MaybeOp2.takeError();
                }
                TcgV Op1 = MaybeOp1.get();
                TcgV Op2 = MaybeOp2.get();

                // Swap operands if the first op. is an immediate
                // and the operator is commutative
                if (Op1.Kind == IrImmediate and Op2.Kind != IrImmediate and
                    Bin->isCommutative()) {
                    std::swap(Op1, Op2);
                }

                if (isa<IntegerType>(Bin->getType())) {
                    if (Op1.Kind == IrImmediate and Op2.Kind == IrImmediate) {
                        Mapper.mapExplicitly(
                            Bin, c::binop(Bin->getOpcode(), Op1, Op2));
                    } else {
                        Expected<TcgV> Res = Mapper.mapAndEmit(Bin);
                        if (!Res) {
                            return mkError("couldn't map binary op res", &I);
                        }

                        // Adapt sizes to account for boolean values, with
                        // LlvmSize == 1 and TcgSize == 32 or 64.  Materialize
                        // first op. to deal with non-commutative ops.
                        TcgSizeAdapter AOp1(Out, tcg::materialize(Op1));
                        TcgSizeAdapter AOp2(Out, Op2);

                        const uint32_t ResSize = Res.get().TcgSize;
                        tcg::genBinOp(Out, Res.get(), Bin->getOpcode(),
                                      AOp1.get(ResSize), AOp2.get(ResSize));
                    }
                } else if (isa<VectorType>(Bin->getType())) {
                    Expected<TcgV> Res = Mapper.mapAndEmit(Bin);
                    if (!Res) {
                        return Res.takeError();
                    }
                    assert(Res.get().Kind == IrPtrToOffset);
                    tcg::genVecBinOp(Out, Bin->getOpcode(), Res.get(), Op1,
                                     Op2);
                }
            } break;
            case Instruction::Call: {
                auto Call = cast<CallInst>(&I);
                Function *F = Call->getCalledFunction();
                if (!F) {
                    return mkError("Indirect function calls not handled: ", &I);
                }
                assert(F->hasName());
                StringRef Name = F->getName();

                // These are the calls we currently no-op/ignore
                if (Name == "__assert_fail" or
                    Name == "g_assertion_message_expr" or
                    isa<DbgValueInst>(I) or isa<DbgLabelInst>(I)) {
                    break;
                }

                SmallVector<TcgV, 4> Args;
                for (uint32_t i = 0; i < Call->arg_size(); ++i) {
                    if (auto Bb =
                            dyn_cast<BasicBlock>(Call->getArgOperand(i))) {
                        Args.push_back(Mapper.mapBbAndEmit(Bb));
                    } else {
                        Expected<TcgV> Mapped =
                            Mapper.mapAndEmit(Call->getArgOperand(i));
                        if (!Mapped) {
                            return Mapped.takeError();
                        }
                        Args.push_back(Mapped.get());
                    }
                }

                // Function names sometimes contain embedded type information to
                // handle polymorphic arguments, for instance
                //
                //   llvm.memcpy.p0i8.p0i8.i64
                //
                // specifying the source and desination pointer types as i8* and
                // the size argument as an i64.
                //
                // Find the index for the first '.' before the types are
                // specified
                //
                //   llvm.memcpy.p0i8.p0i8.i64
                //              ^- index of this '.'
                size_t IndexBeforeTypes = StringRef::npos;
                for (size_t i = Name.size() - 1; i > 0; --i) {
                    const char c = Name[i];
                    bool ValidType = (c >= '0' and c <= '9') or c == 'i' or
                                     c == 'p' or c == 'a' or c == 'v' or
                                     c == 'x';
                    if (c == '.') {
                        IndexBeforeTypes = i;
                    } else if (!ValidType) {
                        break;
                    }
                }
                StringRef StrippedName = Name.substr(0, IndexBeforeTypes);

                PseudoInst PInst = getPseudoInstFromCall(Call);

                if (F->isIntrinsic()) {
                    if (!translateIntrinsicCall(Out, Call, F, Args, Mapper)) {
                        return mkError("Unable to map intrinsic: ", Call);
                    }
                } else if (PInst != InvalidPseudoInst) {
                    if (!translatePseudoInstCall(Out, Call, PInst, Args, Mapper,
                                                 TcgGlobals)) {
                        return mkError("Unable to map pseudo inst: ", Call);
                    }
                } else if (StrippedName == "extract32") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genExtract(Out, false, *MaybeRes, Args[0], Args[1],
                                    Args[2]);
                } else if (StrippedName == "extract64") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genExtract(Out, false, *MaybeRes, Args[0], Args[1],
                                    Args[2]);
                } else if (StrippedName == "sextract32") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genExtract(Out, true, *MaybeRes, Args[0], Args[1],
                                    Args[2]);
                } else if (StrippedName == "sextract64") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genExtract(Out, true, *MaybeRes, Args[0], Args[1],
                                    Args[2]);
                } else if (StrippedName == "deposit32") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genDeposit(Out, *MaybeRes, Args[0], Args[1], Args[2],
                                    Args[3]);
                } else if (StrippedName == "deposit64") {
                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);
                    if (!MaybeRes) {
                        return MaybeRes.takeError();
                    }
                    tcg::genDeposit(Out, *MaybeRes, Args[0], Args[1], Args[2],
                                    Args[3]);
                } else if (Name.startswith("helper")) {
                    // Map and adapt arguments to the call
                    SmallVector<TcgV, 8> IArgs;
                    for (auto Arg : Args) {
                        IArgs.push_back(tcg::materialize(Arg));
                    }
                    tcg::genCallHelper(Out, Name, IArgs.begin(), IArgs.end());
                } else {
                    if (F->isDeclaration()) {
                        return mkError("call to declaration: ", Call);
                    }
                    if (HasTranslatedFunction.find(F) ==
                        HasTranslatedFunction.end()) {
                        return mkError(
                            "call to function which failed to translate: ",
                            Call);
                    }

                    // Map and adapt arguments to the call

                    Expected<TcgV> MaybeRes = mapCallReturnValue(Mapper, Call);

                    StringRef Name = F->getName();
                    Name.consume_front("helper_");
                    Out << "emit_" << Name << "(";

                    if (MaybeRes) {
                        Out << tcg::getName(MaybeRes.get());
                        if (!Args.empty()) {
                            Out << ", ";
                        }
                    }

                    for (unsigned i = 0; i < Args.size(); ++i) {
                        Out << tcg::getName(tcg::materialize(Args[i]));
                        if (i < Args.size() - 1) {
                            Out << ", ";
                        }
                    }
                    Out << ");\n";
                }

            } break;
            case Instruction::ICmp: {
                auto *ICmp = cast<ICmpInst>(&I);
                Expected<TcgV> Op1 = Mapper.mapAndEmit(I.getOperand(0));
                if (!Op1) {
                    return mkError("Couldn't map first op: ", ICmp);
                }
                Expected<TcgV> Op2 = Mapper.mapAndEmit(I.getOperand(1));
                if (!Op2) {
                    return mkError("Couldn't map first op: ", ICmp);
                }
                // If both operands are immediates (constant expressions, we can
                // perform the operation as a constant expression.
                if (Op1.get().Kind == IrImmediate and
                    Op2.get().Kind == IrImmediate) {
                    Mapper.mapExplicitly(
                        ICmp,
                        c::compare(ICmp->getPredicate(), Op1.get(), Op2.get()));
                    break;
                }

                ICmpInst::Predicate LlvmPred = ICmp->getPredicate();

                if (Op1.get().Kind == IrPtrToOffset) {
                    Expected<TcgV> Res = Mapper.mapCondAndEmit(
                        &I, Op1.get().TcgSize, Op1.get().LlvmSize);
                    if (!Res) {
                        return mkError("couldn't map icmp result", &I);
                    }
                    tcg::genVecCmp(Out, Res.get(), LlvmPred, Op1.get(),
                                   Op2.get());
                } else {
                    Expected<TcgV> Res =
                        Mapper.mapCondAndEmit(&I, Op1.get().TcgSize, 1);
                    if (!Res) {
                        return mkError("couldn't map icmp result", &I);
                    }
                    auto IOp1 = tcg::materialize(Op1.get());
                    if (ICmp->isSigned()) {
                        ensureSignBitIsSet(Out, IOp1);
                        ensureSignBitIsSet(Out, Op2.get());
                    }
                    if (Op2.get().Kind == IrImmediate) {
                        tcg::genSetcondI(Out, LlvmPred, Res.get(), IOp1,
                                         Op2.get());
                    } else {
                        tcg::genSetcond(Out, LlvmPred, Res.get(), IOp1,
                                        Op2.get());
                    }
                }

            } break;
            case Instruction::Select: {
                auto Select = cast<SelectInst>(&I);
                Expected<TcgV> Res = Mapper.mapAndEmit(&I);
                if (!Res) {
                    return mkError("Couldn't map select result", &I);
                }
                if (Res.get().Kind == IrPtr) {
                    return mkError(
                        "Select statements for pointer types not supported: ",
                        Select);
                }
                Expected<TcgV> Cond = Mapper.mapAndEmit(Select->getCondition());
                if (!Cond) {
                    return mkError("Error mapping select cond");
                }
                Expected<TcgV> True = Mapper.mapAndEmit(Select->getTrueValue());
                if (!True) {
                    return mkError("Couldn't map True for select instruction: ",
                                   Select);
                }
                Expected<TcgV> False =
                    Mapper.mapAndEmit(Select->getFalseValue());
                if (!False) {
                    return mkError(
                        "Couldn't map False for select instruction: ", Select);
                }

                if (Res.get().Kind == IrPtrToOffset) {
                    tcg::genVecBitsel(Out, Res.get(), Cond.get(), True.get(),
                                      False.get());
                } else if (Cond.get().Kind == IrImmediate) {
                    assert(Res.get().Kind != IrImmediate);
                    const TcgV MTrue = tcg::materialize(True.get());
                    const TcgV MFalse = tcg::materialize(False.get());
                    tcg::genMov(Out, Res.get(),
                                c::ternary(Cond.get(), MTrue, MFalse));
                } else {
                    TcgV Zero = TcgV::makeImmediate("0", Res.get().TcgSize, 1);
                    TcgSizeAdapter ACond(Out, Cond.get());
                    TcgSizeAdapter ATrue(Out, True.get());
                    TcgSizeAdapter AFalse(Out, False.get());
                    if (True.get().Kind == IrImmediate or
                        False.get().Kind == IrImmediate) {
                        auto CTrue =
                            tcg::materialize(ATrue.get(Res.get().TcgSize));
                        auto CFalse =
                            tcg::materialize(AFalse.get(Res.get().TcgSize));

                        tcg::genMovcond(Out, CmpInst::Predicate::ICMP_NE,
                                        Res.get(), ACond.get(CTrue.TcgSize),
                                        Zero, CTrue, CFalse);
                    } else {
                        tcg::genMovcond(Out, CmpInst::Predicate::ICMP_NE,
                                        Res.get(),
                                        ACond.get(True.get().TcgSize), Zero,
                                        ATrue.get(Res.get().TcgSize),
                                        AFalse.get(Res.get().TcgSize));
                    }
                }
            } break;
            case Instruction::Ret: {
                auto Ret = cast<ReturnInst>(&I);
                if (Ret->getNumOperands() == 0)
                    break;

                assert(TAD.ReturnValue.hasValue());
                Expected<TcgV> Tcg = Mapper.mapAndEmit(Ret->getReturnValue());
                if (!Tcg) {
                    return Tcg.takeError();
                }
                if (Tcg.get().Kind == IrImmediate) {
                    tcg::genMovI(Out, *TAD.ReturnValue, Tcg.get());
                } else if (!TAD.SkipReturnMov) {
                    tcg::genMov(Out, *TAD.ReturnValue, Tcg.get());
                }
            } break;
            case Instruction::BitCast: {
                // We currently identity-map `BitCast`s
                //
                // If the bitcast has a larger lifetime than the source
                // variable, we need to allocate a new variable so we
                // don't accidentally free too soon.
                auto Bitcast = cast<BitCastInst>(&I);
                Expected<TcgV> SrcVal =
                    Mapper.mapAndEmit(Bitcast->getOperand(0));
                if (!SrcVal) {
                    return SrcVal.takeError();
                }
                auto *DstTy = Bitcast->getType();
                if (SrcVal.get().Kind == IrPtrToOffset) {
                    auto *PtrTy = cast<PointerType>(DstTy);
                    auto *VecTy =
                        dyn_cast<VectorType>(PtrTy->getPointerElementType());
                    if (!VecTy) {
                        return mkError("bitcast to unsuppored type: ", Bitcast);
                    }
                    auto *IntTy = cast<IntegerType>(VecTy->getElementType());
                    uint32_t LlvmSize = IntTy->getBitWidth();
                    uint32_t VectorElements =
                        compat::getVectorElementCount(VecTy);
                    uint32_t VectorSize = LlvmSize * VectorElements;
                    TcgV Tcg = SrcVal.get();
                    uint32_t TcgVectorSize = llvmToTcgSize(VectorSize);
                    Tcg.TcgSize = TcgVectorSize;
                    Tcg.LlvmSize = LlvmSize;
                    Tcg.VectorElementCount = VectorElements;
                    Tcg.Kind = IrPtrToOffset;
                    Mapper.mapExplicitly(Bitcast, Tcg);
                } else if (DstTy->isPointerTy()) {
                    auto *ElmTy = DstTy->getPointerElementType();
                    if (ElmTy->isIntegerTy()) {
                        auto *IntTy = cast<IntegerType>(ElmTy);
                        const uint32_t TcgSize =
                            llvmToTcgSize(IntTy->getBitWidth());
                        if (TcgSize == SrcVal.get().TcgSize) {
                            Mapper.mapExplicitly(Bitcast, SrcVal.get());
                        } else {
                            return mkError("Invalid bitcast changes tcg size: ",
                                           &I);
                        }
                    } else if (ElmTy->isArrayTy()) {
                        return mkError("Bitcast to unsupported type: ", &I);
                    } else {
                        Mapper.mapExplicitly(Bitcast, SrcVal.get());
                    }
                } else if (DstTy->isVectorTy()) {
                    auto *VecTy = cast<VectorType>(DstTy);
                    auto *IntTy = cast<IntegerType>(VecTy->getElementType());
                    uint32_t LlvmSize = IntTy->getBitWidth();
                    uint32_t VectorElements =
                        compat::getVectorElementCount(VecTy);
                    uint32_t VectorSize = LlvmSize * VectorElements;
                    uint32_t TcgVectorSize = llvmToTcgSize(VectorSize);
                    TcgV Tcg = SrcVal.get();
                    Tcg.TcgSize = TcgVectorSize;
                    Tcg.LlvmSize = LlvmSize;
                    Tcg.VectorElementCount = VectorElements;
                    Tcg.Kind = IrPtrToOffset;
                    Mapper.mapExplicitly(Bitcast, Tcg);
                } else {
                    return mkError("Unhandled bitcast type: ", Bitcast);
                }
            } break;
            case Instruction::Load: {
                auto *Load = cast<LoadInst>(&I);
                auto *LlvmPtr = Load->getPointerOperand();

                Expected<TcgV> Mapped = Mapper.mapAndEmit(LlvmPtr);
                if (!Mapped) {
                    return Mapped.takeError();
                }
                switch (Mapped.get().Kind) {
                case IrPtr: {
                    Expected<TcgV> Res = Mapper.mapAndEmit(Load);
                    if (!Res) {
                        return Res.takeError();
                    }
                    tcg::genLd(Out, Res.get(), Mapped.get(), 0);
                } break;
                case IrImmediate: {
                    Expected<TcgV> Res = Mapper.mapAndEmit(Load);
                    if (!Res) {
                        return Res.takeError();
                    }
                    // Add pointer dereference to immediate address
                    tcg::genMovI(Out, Res.get(),
                                 c::deref(Mapped.get(), Res.get().LlvmSize,
                                          Res.get().TcgSize));
                } break;
                case IrValue: {
                    Expected<TcgV> Res = Mapper.mapAndEmit(Load);
                    if (!Res) {
                        return Res.takeError();
                    }
                    tcg::genMov(Out, Res.get(), Mapped.get());
                } break;
                case IrPtrToOffset: {
                    // Loads from IrPtrToOffset are identity mapped, they are an
                    // artifact of IrPtrToOffset arguments being pointers.
                    // Stores to results are instead taken care of by whatever
                    // instruction generated the result.
                    if (isa<VectorType>(Load->getType())) {
                        Mapper.mapExplicitly(Load, Mapped.get());
                    }
                } break;
                default:
                    return mkError("Load from unsupported TcgV type");
                };

            } break;
            case Instruction::Store: {
                auto *Store = cast<StoreInst>(&I);
                Expected<TcgV> Val =
                    Mapper.mapAndEmit(Store->getValueOperand());
                if (!Val) {
                    return Val.takeError();
                }
                auto *LlvmPtr = Store->getPointerOperand();
                Expected<TcgV> Mapped = Mapper.mapAndEmit(LlvmPtr);
                if (!Mapped) {
                    return Mapped.takeError();
                }
                if (Mapped.get().Kind == IrValue) {
                    switch (Val.get().Kind) {
                    case IrImmediate: {
                        tcg::genMovI(Out, Mapped.get(), Val.get());
                    } break;
                    case IrValue: {
                        tcg::genMov(Out, Mapped.get(), Val.get());
                    } break;
                    default:
                        return mkError("Store from unsupported TcgV type");
                    };
                } else if (Mapped.get().Kind == IrPtr) {
                    tcg::genSt(Out, Mapped.get(), tcg::materialize(Val.get()),
                               0);
                } else if (Mapped.get().Kind == IrPtrToOffset) {
                    // Stores to IrPtrToOffset are ignored, they are an artifact
                    // of IrPtrToOffset arguments being pointers. Stores to
                    // results are instead taken care of by whatever instruction
                    // generated the result.
                } else {
                    return mkError("Store to unsupported TcgV kind: ", Store);
                }
            } break;
            case Instruction::Unreachable: {
                Out << "/* unreachable */\n";
            } break;
            case Instruction::Switch: {
                auto Switch = cast<SwitchInst>(&I);
                // Operands to switch instructions alternate between
                // case values and the corresponding label:
                //   Operands: { Cond, DefaultLabel, Case0, Label0, Case1,
                //   Label1, ... }
                Expected<TcgV> Val = Mapper.mapAndEmit(Switch->getOperand(0));
                if (!Val) {
                    return Val.takeError();
                }
                const TcgV DefaultLabel = Mapper.mapBbAndEmit(
                    cast<BasicBlock>(Switch->getOperand(1)));
                for (uint32_t i = 2; i < Switch->getNumOperands(); i += 2) {
                    Expected<TcgV> BranchVal =
                        Mapper.mapAndEmit(Switch->getOperand(i));
                    if (!BranchVal) {
                        return BranchVal.takeError();
                    }
                    const TcgV BranchLabel = Mapper.mapBbAndEmit(
                        cast<BasicBlock>(Switch->getOperand(i + 1)));
                    tcg::genBrcond(Out, CmpInst::Predicate::ICMP_EQ, Val.get(),
                                   BranchVal.get(), BranchLabel);
                }
                tcg::genBr(Out, DefaultLabel);
            } break;
            case Instruction::Freeze: {
            } break;
            default: {
                return mkError("Instruction not yet implemented", &I);
            }
            }
        }
    }

    Out << "}\n";

    Out.flush();
    HeaderWriter.flush();
    DispatchWriter.flush();
    DispatchCallWriter.flush();

    return TF;
}

PreservedAnalyses TcgGenPass::run(Module &M, ModuleAnalysisManager &MAM)
{
    auto &CG = MAM.getResult<CallGraphAnalysis>(M);

    // Vector of translation results
    SmallVector<TranslatedFunction, 16> TranslatedFunctions;
    // Two sets used for quickly looking up whether or not a function has
    // already been translated, or the translation failed.
    SmallPtrSet<Function *, 16> FailedToTranslateFunction;
    SmallPtrSet<Function *, 16> HasTranslatedFunction;
    for (Function &F : M) {
        if (F.isDeclaration()) {
            continue;
        }

        // Depth first traversal of call graph.  Needed to ensure called
        // functions are translated before the current function.
        CallGraphNode *Node = CG[&F];
        for (auto *N : make_range(po_begin(Node), po_end(Node))) {
            Function *F = N->getFunction();

            // If F in the call graph has already been translated and failed,
            // abort translation of the current function. (NOTE: use of .find()
            // over .contains() is to appease LLVM 10.)
            bool FailedTranslation = FailedToTranslateFunction.find(F) !=
                                     FailedToTranslateFunction.end();
            if (FailedTranslation) {
                break;
            }

            // Skip translation of invalid functions or functions that have
            // already been translated. (NOTE: use of .find() over .contains()
            // is to appease LLVM 10.)
            bool AlreadyTranslated =
                HasTranslatedFunction.find(F) != HasTranslatedFunction.end();
            if (!F or F->isDeclaration() or AlreadyTranslated) {
                continue;
            }

            tcg::resetNameIndices();

            auto Translated = translateFunction(F, TcgGlobals, Annotations,
                                                HasTranslatedFunction);
            if (!Translated) {
                FailedToTranslateFunction.insert(F);
                OutLog << F->getName() << ": " << Translated.takeError()
                       << "\n";
                if (ErrorOnTranslationFailure) {
                    return PreservedAnalyses::all();
                } else {
                    break;
                }
            }

            TranslatedFunctions.push_back(*Translated);
            HasTranslatedFunction.insert(F);
            OutLog << F->getName() << ": OK\n";
        }
    }

    // Preamble
    OutSource << "#include \"qemu/osdep.h\"\n";
    OutSource << "#include \"qemu/log.h\"\n";
    OutSource << "#include \"cpu.h\"\n";
    OutSource << "#include \"tcg/tcg-op.h\"\n";
    OutSource << "#include \"tcg/tcg-op-gvec.h\"\n";
    OutSource << "#include \"tcg/tcg.h\"\n";
    OutSource << "#include \"tcg/tcg-global-mappings.h\"\n";
    OutSource << "#include \"exec/exec-all.h\"\n";
    OutSource << "#include \"exec/helper-gen.h\"\n";
    OutSource << '\n';

    OutSource << "#include \""
              << HeaderPath.substr(HeaderPath.find_last_of('/') + 1) << "\"\n";
    OutSource << '\n';

    // Emit extern definitions for all global TCGv_* that are mapped
    // to the CPUState.
    for (auto &P : TcgGlobals) {
        const TcgGlobal &Global = P.second;
        const uint32_t Size = llvmToTcgSize(Global.Size);
        OutSource << "extern " << "TCGv_i" << Size << " " << Global.Code;
        if (Global.NumElements > 1) {
            OutSource << "[" << Global.NumElements << "]";
        }
        OutSource << ";\n";
    }

    c::emitVectorPreamble(OutSource);

    // Emit translated functions
    for (auto &TF : TranslatedFunctions) {
        OutSource << TF.Code << '\n';
        OutHeader << TF.Decl << '\n';
        OutEnabled << TF.Name << '\n';
    }

    // Emit a dispatched to go from helper function address to our
    // emitted code, if we succeeded.
    OutHeader << "int helper_to_tcg_dispatcher(void *func, TCGTemp *ret_temp, "
                 "int nargs, TCGTemp **args);\n";

    OutSource << "\n";
    OutSource << "#include \"exec/helper-proto.h\"\n";
    OutSource << "int helper_to_tcg_dispatcher(void *func, TCGTemp *ret_temp, "
                 "int nargs, TCGTemp **args) {\n";
    for (auto &TF : TranslatedFunctions) {
        if (!TF.IsHelper or TF.DispatchCode.empty()) {
            continue;
        }
        OutSource << "    if ((uintptr_t) func == (uintptr_t) helper_"
                  << TF.Name << ") {\n";
        OutSource << TF.DispatchCode;
        OutSource << "        return 1;\n";
        OutSource << "    }\n";
    }
    OutSource << "    return 0;\n";
    OutSource << "}\n";

    return PreservedAnalyses::all();
}
