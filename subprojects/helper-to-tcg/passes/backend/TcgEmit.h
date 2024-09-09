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

#pragma once

#include "TcgType.h"

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/IR/InstrTypes.h> // for CmpInst::Predicate
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

#include <string>

using llvm::CmpInst;
using llvm::Instruction;
using llvm::raw_ostream;
using llvm::SmallVector;
using llvm::StringRef;
using llvm::Twine;
using llvm::Value;

namespace tcg
{
inline std::string getName(const TcgV &V)
{
    if (V.ConstantExpression or V.Kind == IrImmediate or V.Kind == IrConst) {
        return V.Name;
    } else {
        switch (V.Kind) {
        case IrValue:
            return Twine("temp").concat(Twine(V.Id)).str();
        case IrEnv:
            return "env";
        case IrPtr:
            return Twine("ptr").concat(Twine(V.Id)).str();
        case IrPtrToOffset:
            return Twine("vec").concat(Twine(V.Id)).str();
        case IrLabel:
            return Twine("label").concat(Twine(V.Id)).str();
        default:
            abort();
        };
    }
}
} // namespace tcg

inline raw_ostream &operator<<(raw_ostream &Out, const TcgV &V)
{
    Out << tcg::getName(V);
    return Out;
}

namespace tcg
{

// TODO: The names we give temporaries depend on the function we're in,
// maybe we can put this name/index stuff somewhere more relevant?
void resetNameIndices();
const std::string mkName(const std::string Suffix);

// String representation of types
const std::string getType(const TcgV &Value);

inline const TcgV materialize(const TcgV &Value)
{
    if (Value.Kind != IrImmediate) {
        return Value;
    }
    TcgV M = Value;
    M.Name = Twine("tcg_constant_i")
                 .concat(Twine((int)Value.TcgSize))
                 .concat("(")
                 .concat(tcg::getName(Value))
                 .concat(")")
                 .str();
    M.Kind = IrConst;
    return M;
}

template <typename I>
void emitArgListTcg(raw_ostream &Out, const I Beg, const I End)
{
    auto It = Beg;
    if (It != End) {
        Out << *It;
        ++It;
    }
    while (It != End) {
        Out << ", " << *It;
        ++It;
    }
}

template <typename I>
void emitCall(raw_ostream &Out, const StringRef &S, const I Beg, const I End)
{
    Out << S << '(';
    auto It = Beg;
    if (It != End) {
        Out << *It;
        ++It;
    }
    while (It != End) {
        Out << ", " << *It;
        ++It;
    }
    Out << ");\n";
}

template <typename Iterator>
void emitCallTcg(raw_ostream &Out, const StringRef S, Iterator Begin,
                 Iterator End)
{
    assert(Begin != End);
    Out << S << '(';
    Out << *Begin;
    ++Begin;
    while (Begin != End) {
        Out << ", " << *Begin;
        ++Begin;
    }
    Out << ");\n";
}

inline void emitArgListTcg(raw_ostream &Out,
                           const std::initializer_list<TcgV> Args)
{
    emitArgListTcg(Out, Args.begin(), Args.end());
}

inline void emitCall(raw_ostream &Out, const StringRef &S,
                     const std::initializer_list<StringRef> Args)
{
    emitCall(Out, S, Args.begin(), Args.end());
}

inline void emitCallTcg(raw_ostream &Out, const StringRef &S,
                        std::initializer_list<TcgV> Args)
{
    emitCallTcg(Out, S, Args.begin(), Args.end());
}

inline void genCallHelper(raw_ostream &Out, const StringRef &Helper,
                          const std::initializer_list<TcgV> Args)
{
    auto Func = Twine("gen_").concat(Helper).str();
    emitCallTcg(Out, Func, Args);
}

template <typename I>
void genCallHelper(raw_ostream &Out, const StringRef &Helper, I Beg, I End)
{
    auto Func = Twine("gen_").concat(Helper).str();
    emitCallTcg(Out, Func, Beg, End);
}

void tempNew(raw_ostream &Out, const TcgV &Value);
void tempNewPtr(raw_ostream &Out);
void tempNewVec(raw_ostream &Out);

void genNewLabel(raw_ostream &Out);
void genSetLabel(raw_ostream &Out, const TcgV &L);

void defineNewTemp(raw_ostream &Out, const TcgV &Tcg);

void genBr(raw_ostream &Out, const TcgV &L);

void genTempInit(raw_ostream &Out, const TcgV &Arg1, const StringRef Str);
void genTempInit(raw_ostream &Out, const TcgV &Arg1, uint64_t Value);
void genTempInit(raw_ostream &Out, const TcgV &Arg1, const TcgV &Arg2);
void genAssignConst(raw_ostream &Out, const TcgV &Arg1, const StringRef Str);
void genAssignConst(raw_ostream &Out, const TcgV &Arg1, uint64_t Value);
void genAssignConst(raw_ostream &Out, const TcgV &Arg1, const TcgV &Arg2);

void genExtI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genExtrlI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genExtuI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genExtrhI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genExtract(raw_ostream &Out, bool Sign, const TcgV &Dst, const TcgV &Src,
                const TcgV &Offset, const TcgV &Length);
void genDeposit(raw_ostream &Out, const TcgV &Dst, const TcgV &Into,
                const TcgV &From, const TcgV &Offset, const TcgV &Length);

void genTruncPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);

void genConcat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src1,
               const TcgV &Src2);
void genMov(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genMovPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genAddPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr,
               const TcgV &Offset);
void genBinOp(raw_ostream &Out, const TcgV &Dst,
              const Instruction::BinaryOps Opcode, const TcgV &Src0,
              const TcgV &Src1);
void genMovI(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);

void genMovcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
                const TcgV &Ret, const TcgV &C1, const TcgV &C2, const TcgV &V1,
                const TcgV &V2);
void genSetcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
                const TcgV &Dst, const TcgV &Op1, const TcgV &Op2);
void genSetcondI(raw_ostream &Out, const CmpInst::Predicate &Pred,
                 const TcgV &Dst, const TcgV &Op1, const TcgV &Op2);
void genBrcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
               const TcgV &Arg1, const TcgV &Arg2, const TcgV &Label);

void genQemuLoad(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr,
                 const char *MemOpStr);
void genQemuStore(raw_ostream &Out, const TcgV &Ptr, const TcgV &Src,
                  const char *MemOpStr);

void genLd(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr, uint64_t Offset);
void genSt(raw_ostream &Out, const TcgV &Ptr, const TcgV &Src, uint64_t Offset);

void genFunnelShl(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                  const TcgV &Src1, const TcgV &Shift);
void genBitreverse(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genAbs(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genCountLeadingZeros(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genCountTrailingZeros(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genCountOnes(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genByteswap(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);

// Vector ops.
void genVecBinOp(raw_ostream &Out, const Instruction::BinaryOps Opcode,
                 const TcgV &Dst, const TcgV &Src0, const TcgV &Src1);
void genVecSignedSatAdd(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                        const TcgV &Src1);
void genVecSignedSatSub(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                        const TcgV &Src1);
void genVecSignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                     const TcgV &Src1);
void genVecUnsignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                       const TcgV &Src1);
void genVecSignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                     const TcgV &Src1);
void genVecUnsignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                       const TcgV &Src1);
void genVecMemcpy(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                  const TcgV &Size);
void genVecMemset(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                  const TcgV &Size);
void genVecSplat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genVecArrSplat(raw_ostream &Out, const TcgV &Dst,
                    SmallVector<TcgV, 16> &Arr);
void genVecBitsel(raw_ostream &Out, const TcgV &Dst, const TcgV &Cond,
                  const TcgV &Src0, const TcgV &Src1);
void genVecCmp(raw_ostream &Out, const TcgV &Dst,
               const CmpInst::Predicate &Pred, const TcgV &Src0,
               const TcgV &Src1);
void genVecNot(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genVecTrunc(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genVecSext(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);
void genVecZext(raw_ostream &Out, const TcgV &Dst, const TcgV &Src);

} // namespace tcg

namespace c
{

TcgV ptrAdd(const TcgV &Ptr, const TcgV &Offset);
TcgV ternary(const TcgV &Cond, const TcgV &True, const TcgV &False);
TcgV deref(const TcgV &Ptr, uint32_t LlvmSize, uint32_t TcgSize);
TcgV compare(const CmpInst::Predicate &Pred, const TcgV &Src0,
             const TcgV &Src1);
TcgV zext(const TcgV &V, uint32_t LlvmSize, uint32_t TcgSize);
TcgV sext(const TcgV &V, uint32_t LlvmSize, uint32_t TcgSize);
TcgV binop(Instruction::BinaryOps Opcode, const TcgV &Src0, const TcgV &Src1);

void emitVectorPreamble(raw_ostream &Out);
void emitVectorMemVar(raw_ostream &Out);

} // namespace c
