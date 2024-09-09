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

#include "TcgEmit.h"
#include "CmdLineOptions.h"
#include "backend/TcgType.h"

using namespace llvm;

// Counters used for prettier numbers in names
uint32_t VarIndex = 0;
bool EmittedVectorMem = false;

namespace tcg
{

// Constant used to represent the MMU INDEX for all memory operations.
// get_tb_mmu_index is a function assumed to be defined by the target.
static const TcgV MmuIndex =
    TcgV::makeImmediate(MmuIndexFunction + "(tcg_ctx->gen_tb->flags)", 32, 32);

void resetNameIndices()
{
    VarIndex = 0;
    EmittedVectorMem = false;
}

// TODO: do we still have manual makename calls?
const std::string mkName(const std::string Suffix)
{
    return Twine("v")
        .concat(Suffix)
        .concat("_")
        .concat(Twine(VarIndex++))
        .str();
}

const std::string getType(const TcgV &Value)
{
    switch (Value.Kind) {
    case IrValue:
    case IrConst:
        return Twine("TCGv_i").concat(Twine(Value.TcgSize)).str();
    case IrEnv:
        return "TCGv_env";
    case IrImmediate:
        if (Value.LlvmSize == 1) {
            return "bool";
        } else {
            return Twine("int")
                .concat(Twine((int)Value.LlvmSize))
                .concat("_t")
                .str();
        }
    case IrPtr:
        return "TCGv_ptr";
    case IrPtrToOffset:
        return "intptr_t";
    case IrLabel:
        return "TCGLabel *";
    default:
        abort();
    }
}

inline StringRef mapPredicate(const CmpInst::Predicate &Pred)
{
    switch (Pred) {
    case CmpInst::ICMP_EQ:
        return "TCG_COND_EQ";
    case CmpInst::ICMP_NE:
        return "TCG_COND_NE";
    case CmpInst::ICMP_UGT:
        return "TCG_COND_GTU";
    case CmpInst::ICMP_UGE:
        return "TCG_COND_GEU";
    case CmpInst::ICMP_ULT:
        return "TCG_COND_LTU";
    case CmpInst::ICMP_ULE:
        return "TCG_COND_LEU";
    case CmpInst::ICMP_SGT:
        return "TCG_COND_GT";
    case CmpInst::ICMP_SGE:
        return "TCG_COND_GE";
    case CmpInst::ICMP_SLT:
        return "TCG_COND_LT";
    case CmpInst::ICMP_SLE:
        return "TCG_COND_LE";
    default:
        abort();
    }
}

static std::string mapBinOp(const Instruction::BinaryOps &Opcode,
                            const TcgV &Src0, const TcgV &Src1)
{
    const bool IsImmediate =
        (Src0.Kind == IrImmediate or Src1.Kind == IrImmediate);
    const bool IsPtr = (Opcode == Instruction::Add and
                        (Src0.Kind == IrPtr or Src1.Kind == IrPtr));
    assert(IsImmediate or Src0.TcgSize == Src1.TcgSize);
    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);

    // Check for valid boolean operations if operating on a boolean
    if (Src0.LlvmSize == 1) {
        assert(Src1.LlvmSize == 1);
        assert(Src0.TcgSize == 32 or Src0.TcgSize == 64);
        assert(Src1.TcgSize == 32 or Src1.TcgSize == 64);
        switch (Opcode) {
        case Instruction::And:
        case Instruction::Or:
        case Instruction::Xor:
            break;
        default:
            abort();
        }
    }

    bool NeedSafe = false;
    switch (Opcode) {
    case Instruction::Add:
        ExprStream << "tcg_gen_add";
        break;
    case Instruction::Sub:
        ExprStream << "tcg_gen_sub";
        break;
    case Instruction::And:
        ExprStream << "tcg_gen_and";
        break;
    case Instruction::Or:
        ExprStream << "tcg_gen_or";
        break;
    case Instruction::Xor:
        ExprStream << "tcg_gen_xor";
        break;
    case Instruction::Mul:
        ExprStream << "tcg_gen_mul";
        break;
    case Instruction::UDiv:
        ExprStream << "tcg_gen_divu";
        break;
    case Instruction::SDiv:
        ExprStream << "tcg_gen_div";
        break;
    case Instruction::AShr:
        ExprStream << "tcg_gen_sar";
        NeedSafe = true;
        break;
    case Instruction::LShr:
        ExprStream << "tcg_gen_shr";
        NeedSafe = true;
        break;
    case Instruction::Shl:
        ExprStream << "tcg_gen_shl";
        NeedSafe = true;
        break;
    default:
        abort();
    }
    NeedSafe = false;

    if (IsImmediate) {
        ExprStream << "i";
    }

    if (IsPtr) {
        ExprStream << "_ptr";
    } else {
        ExprStream << "_i" << (int)Src0.TcgSize;
    }

    if (IsImmediate and NeedSafe) {
        ExprStream << "_safe" << (int)Src0.TcgSize;
    }

    ExprStream.flush();

    return Expr;
}

static std::string mapVecBinOp(const Instruction::BinaryOps &Opcode,
                               const TcgV &Src0, const TcgV &Src1)
{
    const bool IsShift = Opcode == Instruction::Shl or
                         Opcode == Instruction::LShr or
                         Opcode == Instruction::AShr;

    std::string Suffix;
    switch (Src1.Kind) {
    case IrPtrToOffset:
        Suffix = (IsShift) ? "v" : "";
        break;
    case IrConst:
    case IrValue:
        Suffix = "s";
        break;
    case IrImmediate:
        Suffix = "i";
        break;
    default:
        abort();
    }

    switch (Opcode) {
    case Instruction::Add:
        return "add" + Suffix;
        break;
    case Instruction::Sub:
        return "sub" + Suffix;
        break;
    case Instruction::Mul:
        return "mul" + Suffix;
        break;
    case Instruction::And:
        return "and" + Suffix;
        break;
    case Instruction::Or:
        return "or" + Suffix;
        break;
    case Instruction::Xor:
        return "xor" + Suffix;
        break;
    case Instruction::Shl:
        return "shl" + Suffix;
        break;
    case Instruction::LShr:
        return "shr" + Suffix;
        break;
    case Instruction::AShr:
        return "sar" + Suffix;
        break;
    default:
        abort();
    }
}

void tempNew(raw_ostream &Out, const TcgV &Value)
{
    if (Value.Kind == IrValue) {
        Out << "tcg_temp_new_i" << (int)Value.TcgSize << "();\n";
    }
}

void tempNewPtr(raw_ostream &Out) { Out << "tcg_temp_new_ptr();\n"; }

void tempNewVec(raw_ostream &Out, uint32_t Size)
{
    Out << "temp_new_gvec(&mem, " << Size << ");\n";
}

void genNewLabel(raw_ostream &Out) { Out << "gen_new_label();\n"; }

void genSetLabel(raw_ostream &Out, const TcgV &L)
{
    assert(L.Kind == IrLabel);
    Out << "gen_set_label(" << L << ");\n";
}

void defineNewTemp(raw_ostream &Out, const TcgV &Tcg)
{
    assert(!Tcg.ConstantExpression);
    if (Tcg.Kind == IrPtrToOffset and !EmittedVectorMem) {
        EmittedVectorMem = true;
        c::emitVectorMemVar(Out);
    }
    Out << tcg::getType(Tcg) << " " << Tcg << " = ";
    switch (Tcg.Kind) {
    case IrValue:
        tcg::tempNew(Out, Tcg);
        break;
    case IrPtr:
        tcg::tempNewPtr(Out);
        break;
    case IrPtrToOffset:
        tcg::tempNewVec(Out, vectorSizeInBytes(Tcg));
        break;
    case IrLabel:
        tcg::genNewLabel(Out);
        break;
    default:
        abort();
    }
}

void genBr(raw_ostream &Out, const TcgV &L)
{
    assert(L.Kind == IrLabel);
    Out << "tcg_gen_br(" << L << ");\n";
}

void genTempInit(raw_ostream &Out, const TcgV &Arg1, const StringRef Str)
{
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_const_i"
        << (int)Arg1.TcgSize << "(" << Str << ");\n";
}

void genTempInit(raw_ostream &Out, const TcgV &Arg1, uint64_t Value)
{
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_const_i"
        << (int)Arg1.TcgSize << "((uint64_t)" << Value << "ULL);\n";
}

void genTempInit(raw_ostream &Out, const TcgV &Arg1, const TcgV &Arg2)
{
    assert(Arg2.Kind == IrImmediate);
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_const_i"
        << (int)Arg1.TcgSize << "(" << Arg2 << ");\n";
}

void genAssignConst(raw_ostream &Out, const TcgV &Arg1, const StringRef Str)
{
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_constant_i"
        << (int)Arg1.TcgSize << "(" << Str << ");\n";
}

void genAssignConst(raw_ostream &Out, const TcgV &Arg1, uint64_t Value)
{
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_constant_i"
        << (int)Arg1.TcgSize << "((uint64_t)" << Value << "ULL);\n";
}

void genAssignConst(raw_ostream &Out, const TcgV &Arg1, const TcgV &Arg2)
{
    assert(Arg2.Kind == IrImmediate);
    Out << getType(Arg1) << ' ' << Arg1 << " = " << "tcg_constant_i"
        << (int)Arg1.TcgSize << "(" << Arg2 << ");\n";
}

void genExtI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == 64);
    assert(Src.TcgSize == 32);
    emitCallTcg(Out, "tcg_gen_ext_i32_i64", {Dst, Src});
}

void genExtrlI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == 32);
    assert(Src.TcgSize == 64);
    emitCallTcg(Out, "tcg_gen_extrl_i64_i32", {Dst, Src});
}

void genExtuI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == 64);
    assert(Src.TcgSize == 32);
    emitCallTcg(Out, "tcg_gen_extu_i32_i64", {Dst, Src});
}

void genExtrhI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == 32);
    assert(Src.TcgSize == 64);
    emitCallTcg(Out, "tcg_gen_extrh_i64_i32", {Dst, Src});
}

void genExtract(raw_ostream &Out, bool Sign, const TcgV &Dst, const TcgV &Src,
                const TcgV &Offset, const TcgV &Length)
{
    assert(Dst.TcgSize == Src.TcgSize);
    const char *SignStr = (Sign) ? "s" : "";
    const TcgV &MSrc = materialize(Src);
    Out << "tcg_gen_" << SignStr << "extract_i" << (int)Dst.TcgSize << "(";
    emitArgListTcg(Out, {Dst, MSrc, Offset, Length});
    Out << ");\n";
}

void genDeposit(raw_ostream &Out, const TcgV &Dst, const TcgV &Into,
                const TcgV &From, const TcgV &Offset, const TcgV &Length)
{
    assert(Dst.TcgSize == Into.TcgSize);
    assert(Dst.TcgSize == From.TcgSize or From.Kind == IrImmediate);
    Out << "tcg_gen_deposit_i" << (int)Dst.TcgSize << "(";
    const TcgV MInto = materialize(Into);
    const TcgV MLength = materialize(Length);
    emitArgListTcg(Out, {Dst, MInto, MLength, From, Offset});
    Out << ");\n";
}

void genTruncPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    auto FuncStr = Twine("tcg_gen_trunc_i")
                       .concat(std::to_string(Src.TcgSize))
                       .concat("_ptr")
                       .str();
    emitCallTcg(Out, FuncStr, {Dst, Src});
}

void genConcat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src1,
               const TcgV &Src2)
{
    assert(Dst.TcgSize == 64);
    assert(Src1.TcgSize == 32);
    assert(Src2.TcgSize == 32);
    emitCallTcg(Out, "tcg_gen_concat_i32_i64", {Dst, Src1, Src2});
}

void genMov(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    Out << "tcg_gen_mov_i" << (int)Dst.TcgSize << "(" << Dst << ", " << Src
        << ");\n";
}

void genMovPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    assert(Dst.Kind == IrPtr);
    assert(Src.Kind == IrPtr);
    Out << "tcg_gen_mov_ptr(" << Dst << ", " << Src << ");\n";
}

void genAddPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr,
               const TcgV &Offset)
{
    assert(Ptr.Kind == IrPtr or Ptr.Kind == IrEnv);
    switch (Offset.Kind) {
    case IrConst:
    case IrImmediate: {
        emitCallTcg(Out, "tcg_gen_addi_ptr", {Dst, Ptr, Offset});
    } break;
    case IrValue: {
        uint32_t TcgTargetPtrSize = 64;
        auto OffsetPtr =
            TcgV::makeTemp(TcgTargetPtrSize, TcgTargetPtrSize, IrPtr);
        tcg::defineNewTemp(Out, OffsetPtr);
        tcg::genTruncPtr(Out, OffsetPtr, Offset);

        emitCallTcg(Out, "tcg_gen_add_ptr", {Dst, Ptr, OffsetPtr});
    } break;
    default:
        abort();
    }
}

void genBinOp(raw_ostream &Out, const TcgV &Dst,
              const Instruction::BinaryOps Opcode, const TcgV &Src0,
              const TcgV &Src1)
{
    auto OpStr = mapBinOp(Opcode, Src0, Src1);
    emitCallTcg(Out, OpStr, {Dst, Src0, Src1});
}

void genMovI(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Src.Kind == IrImmediate);
    Out << "tcg_gen_movi_i" << (int)Dst.TcgSize << "(" << Dst << ", " << Src
        << ");\n";
}

void genMovcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
                const TcgV &Ret, const TcgV &C1, const TcgV &C2, const TcgV &V1,
                const TcgV &V2)
{
    assert(Ret.TcgSize == C1.TcgSize);
    assert(Ret.TcgSize == C2.TcgSize);
    assert(Ret.TcgSize == V1.TcgSize);
    assert(Ret.TcgSize == V2.TcgSize);
    const TcgV mC1 = materialize(C1);
    const TcgV mC2 = materialize(C2);
    const TcgV mV1 = materialize(V1);
    const TcgV mV2 = materialize(V2);
    Out << "tcg_gen_movcond_i" << (int)Ret.TcgSize << '(' << mapPredicate(Pred)
        << ", ";
    emitArgListTcg(Out, {Ret, mC1, mC2, mV1, mV2});
    Out << ");\n";
}

void genSetcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
                const TcgV &Dst, const TcgV &Op1, const TcgV &Op2)
{
    assert(Op1.TcgSize == Op2.TcgSize);
    assert(Op1.TcgSize == Dst.TcgSize);
    assert(Op1.TcgSize == 32 or Op1.TcgSize == 64);
    Out << "tcg_gen_setcond_i" << (int)Dst.TcgSize << "(" << mapPredicate(Pred)
        << ", " << Dst << ", " << Op1 << ", " << Op2 << ");\n";
}

void genSetcondI(raw_ostream &Out, const CmpInst::Predicate &Pred,
                 const TcgV &Dst, const TcgV &Op1, const TcgV &Op2)
{
    assert(Op1.TcgSize == Dst.TcgSize);
    assert(Op1.TcgSize == 32 or Op1.TcgSize == 64);
    assert(Dst.Kind != IrImmediate && Op1.Kind != IrImmediate &&
           Op2.Kind == IrImmediate);
    Out << "tcg_gen_setcondi_i" << (int)Dst.TcgSize << "(" << mapPredicate(Pred)
        << ", " << Dst << ", " << Op1 << ", " << Op2 << ");\n";
}

void genBrcond(raw_ostream &Out, const CmpInst::Predicate &Pred,
               const TcgV &Arg1, const TcgV &Arg2, const TcgV &Label)
{
    assert(Arg1.TcgSize == Arg2.TcgSize);
    assert(Arg1.TcgSize == 32 || Arg1.TcgSize == 64);
    assert(Label.Kind == IrLabel);
    if (Arg2.Kind == IrImmediate) {
        Out << "tcg_gen_brcondi_i" << (int)Arg1.TcgSize;
    } else {
        Out << "tcg_gen_brcond_i" << (int)Arg1.TcgSize;
    }
    Out << "(" << mapPredicate(Pred) << ", " << materialize(Arg1) << ", "
        << Arg2 << ", " << Label << ");\n";
}

void genQemuLoad(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr,
                 const char *MemOpStr)
{
    assert(Dst.Kind == IrValue);
    assert(Ptr.Kind != IrImmediate);
    const auto MPtr = materialize(Ptr);
    Out << "tcg_gen_qemu_ld_i" << (int)Dst.TcgSize << "(";
    emitArgListTcg(Out, {Dst, MPtr, MmuIndex});
    Out << ", " << MemOpStr << ");\n";
}

void genQemuStore(raw_ostream &Out, const TcgV &Ptr, const TcgV &Src,
                  const char *MemOpStr)
{
    assert(Src.Kind == IrValue);
    assert(Ptr.Kind != IrImmediate);
    const auto MPtr = materialize(Ptr);
    Out << "tcg_gen_qemu_st_i" << (int)Src.TcgSize << "(";
    emitArgListTcg(Out, {Src, MPtr, MmuIndex});
    Out << ", " << MemOpStr << ");\n";
}

void genLd(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr, uint64_t Offset)
{
    assert(Ptr.Kind == IrPtr);
    // First output the correct tcg function for the widths of Dst
    if (Dst.LlvmSize < Dst.TcgSize) {
        Out << "tcg_gen_ld" << (int)Dst.LlvmSize << "u_i" << (int)Dst.TcgSize;
    } else {
        Out << "tcg_gen_ld_i" << (int)Dst.TcgSize;
    }
    // Then emit params
    Out << "(" << Dst << ", " << Ptr << ", " << Offset << ");\n";
}

void genSt(raw_ostream &Out, const TcgV &Ptr, const TcgV &Src, uint64_t Offset)
{
    assert(Ptr.Kind == IrPtr);
    // First output the correct tcg function for the widths of Dst
    if (Src.LlvmSize < Src.TcgSize) {
        Out << "tcg_gen_st" << (int)Src.LlvmSize << "_i" << (int)Src.TcgSize;
    } else {
        Out << "tcg_gen_st_i" << (int)Src.TcgSize;
    }
    // Then emit params
    Out << "(" << Src << ", " << Ptr << ", " << Offset << ");\n";
}
void genFunnelShl(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                  const TcgV &Src1, const TcgV &Shift)
{
    assert(Src0.TcgSize == Dst.TcgSize);
    assert(Src1.TcgSize == Dst.TcgSize);
    assert(Shift.TcgSize == Dst.TcgSize);

    if (Dst.TcgSize == 32) {
        auto Temp = TcgV::makeTemp(64, 64, IrValue);
        defineNewTemp(Out, Temp);
        genConcat(Out, Temp, Src1, Src0);

        if (Shift.Kind == IrImmediate) {
            genBinOp(Out, Temp, Instruction::Shl, Temp, Shift);
        } else {
            auto Ext = TcgV::makeTemp(64, 64, IrValue);
            defineNewTemp(Out, Ext);
            genExtuI32I64(Out, Ext, Shift);
            genBinOp(Out, Temp, Instruction::Shl, Temp, Ext);
        }

        tcg::genExtrhI64I32(Out, Dst, Temp);
    } else if (Dst.TcgSize == 64) {
        const TcgV ASrc0 = materialize(Src0);
        const TcgV ASrc1 = materialize(Src1);
        const TcgV AShift = materialize(Shift);
        genCallHelper(Out, "helper_fshl_i64", {Dst, ASrc0, ASrc1, AShift});
    }
}

void genBitreverse(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    auto FuncName = Twine("helper_bitreverse")
                        .concat(Twine((int)Dst.LlvmSize))
                        .concat("_i")
                        .concat(Twine((int)Src.TcgSize))
                        .str();
    genCallHelper(Out, FuncName, {Dst, Src});
}

void genCountLeadingZeros(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    Out << "tcg_gen_clzi_i" << (int)Dst.TcgSize << "(" << Dst << ", " << Src
        << ", " << (int)Src.TcgSize << ");\n";
}

void genCountTrailingZeros(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    Out << "tcg_gen_ctzi_i" << (int)Dst.TcgSize << "(" << Dst << ", " << Src
        << ", " << (int)Src.TcgSize << ");\n";
}

void genCountOnes(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    Out << "tcg_gen_ctpop_i" << (int)Dst.TcgSize << "(" << Dst << ", " << Src
        << ");\n";
}

void genByteswap(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.TcgSize == Src.TcgSize);
    Out << "tcg_gen_bswap" << (int)Dst.TcgSize << "_i" << (int)Src.TcgSize
        << "(" << Dst << ", " << Src << ");\n";
}

static void genVecBinOpStr(raw_ostream &Out, StringRef Op, const TcgV &Dst,
                           const TcgV &Src0, const TcgV &Src1)
{
    const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
    Out << "tcg_gen_gvec_";
    Out << Op;
    Out << "(MO_" << (int)Dst.LlvmSize << ", " << Dst << ", " << Src0 << ", "
        << Src1 << ", " << VectorSizeInBytes << ", " << VectorSizeInBytes
        << ");\n";
}

void genVecBinOp(raw_ostream &Out, const Instruction::BinaryOps Opcode,
                 const TcgV &Dst, const TcgV &Src0, const TcgV &Src1)
{
    genVecBinOpStr(Out, mapVecBinOp(Opcode, Src0, Src1), Dst, Src0, Src1);
}

void genVecSignedSatAdd(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                        const TcgV &Src1)
{
    assert(Dst.Kind == IrPtrToOffset);
    genVecBinOpStr(Out, "ssadd", Dst, Src0, Src1);
}

void genVecSignedSatSub(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                        const TcgV &Src1)
{
    assert(Dst.Kind == IrPtrToOffset);
    genVecBinOpStr(Out, "sssub", Dst, Src0, Src1);
}

void genVecSignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                     const TcgV &Src1)
{
    switch (Dst.Kind) {
    case IrValue: {
        const TcgV MSrc0 = materialize(Src0);
        const TcgV MSrc1 = materialize(Src1);
        Out << "tcg_gen_smax_i" << (int)Dst.TcgSize << "(" << Dst << ", "
            << MSrc0 << ", " << MSrc1 << ");\n";
    } break;
    case IrPtrToOffset: {
        genVecBinOpStr(Out, "smax", Dst, Src0, Src1);
    } break;
    default:
        abort();
    }
}

void genVecUnsignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                       const TcgV &Src1)
{
    switch (Dst.Kind) {
    case IrValue: {
        const TcgV MSrc0 = materialize(Src0);
        const TcgV MSrc1 = materialize(Src1);
        Out << "tcg_gen_umax_i" << (int)Dst.TcgSize << "(" << Dst << ", "
            << MSrc0 << ", " << MSrc1 << ");\n";
    } break;
    case IrPtrToOffset: {
        genVecBinOpStr(Out, "umax", Dst, Src0, Src1);
    } break;
    default:
        abort();
    }
}

void genVecSignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                     const TcgV &Src1)
{
    switch (Dst.Kind) {
    case IrValue: {
        const TcgV MSrc0 = materialize(Src0);
        const TcgV MSrc1 = materialize(Src1);
        Out << "tcg_gen_smin_i" << (int)Dst.TcgSize << "(" << Dst << ", "
            << MSrc0 << ", " << MSrc1 << ");\n";
    } break;
    case IrPtrToOffset: {
        genVecBinOpStr(Out, "smin", Dst, Src0, Src1);
    } break;
    default:
        abort();
    }
}

void genVecUnsignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                       const TcgV &Src1)
{
    switch (Dst.Kind) {
    case IrValue: {
        const TcgV MSrc0 = materialize(Src0);
        const TcgV MSrc1 = materialize(Src1);
        Out << "tcg_gen_umin_i" << (int)Dst.TcgSize << "(" << Dst << ", "
            << MSrc0 << ", " << MSrc1 << ");\n";
    } break;
    case IrPtrToOffset: {
        genVecBinOpStr(Out, "umin", Dst, Src0, Src1);
    } break;
    default:
        assert(false);
    }
}

void genVecMemcpy(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                  const TcgV &Size)
{
    Out << "tcg_gen_gvec_mov(MO_8" << ", " << Dst << ", " << Src << ", " << Size
        << ", " << Size << ");\n";
}

void genVecMemset(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                  const TcgV &Size)
{
    switch (Src.Kind) {
    case IrValue:
    case IrConst:
        Out << "tcg_gen_gvec_dup_i" << (int)Src.TcgSize << "(MO_"
            << (int)Src.LlvmSize << ", " << Dst << ", " << Size << ", " << Size
            << ", " << Src << ");\n";
        break;
    case IrImmediate:
        Out << "tcg_gen_gvec_dup_imm" << "(MO_" << (int)Src.LlvmSize << ", "
            << Dst << ", " << Size << ", " << Size << ", " << Src << ");\n";
        break;
    default:
        abort();
    }
}

void genVecSplat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
    const auto Size =
        TcgV::makeImmediate(Twine(VectorSizeInBytes).str(), 64, 64);
    genVecMemset(Out, Dst, Src, Size);
}

void genVecArrSplat(raw_ostream &Out, const TcgV &Dst,
                    SmallVector<TcgV, 16> &Arr)
{
    const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
    const std::string TmpName = mkName("varr");
    Out << "uint" << (int)Dst.LlvmSize << "_t " << TmpName << "[] = {";
    emitArgListTcg(Out, Arr.begin(), Arr.end());
    Out << "};\n";
    // TODO: We are using global tcg_env here as not all functions that might
    // emit constants take env.
    Out << "tcg_gen_gvec_constant(MO_" << (int)Dst.LlvmSize << ", tcg_env"
        << ", " << Dst << ", " << TmpName << ", " << VectorSizeInBytes
        << ");\n";
}

void genVecBitsel(raw_ostream &Out, const TcgV &Dst, const TcgV &Cond,
                  const TcgV &Src0, const TcgV &Src1)
{
    const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
    Out << "tcg_gen_gvec_bitsel(" << "MO_" << (int)Dst.LlvmSize << ", " << Dst
        << ", " << Cond << ", " << Src0 << ", " << Src1 << ", "
        << VectorSizeInBytes << ", " << VectorSizeInBytes << ");\n";
}

void genVecCmp(raw_ostream &Out, const TcgV &Dst,
               const CmpInst::Predicate &Pred, const TcgV &Src0,
               const TcgV &Src1)
{
    // TODO: Return type of llvm vector compare is actually 128 x i1, currently
    // we keep the same element size.  Requires trunc.
    const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
    Out << "tcg_gen_gvec_cmp(" << mapPredicate(Pred) << ", " << "MO_"
        << (int)Dst.LlvmSize << ", " << Dst << ", " << Src0 << ", " << Src1
        << ", " << VectorSizeInBytes << ", " << VectorSizeInBytes << ");\n";
}

void genAbs(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    assert(Dst.Kind == Src.Kind);
    assert(Dst.TcgSize == Src.TcgSize);
    switch (Dst.Kind) {
    case IrValue: {
        const auto FuncStr =
            Twine("tcg_gen_abs_i").concat(Twine(Src.TcgSize)).str();
        emitCallTcg(Out, FuncStr, {Dst, Src});
    } break;
    case IrPtrToOffset: {
        auto VectorSize = Dst.LlvmSize * Dst.VectorElementCount / 8;
        Out << "tcg_gen_gvec_abs(" << "MO_" << (int)Dst.LlvmSize << ", " << Dst
            << ", " << Src << ", " << VectorSize << ", " << VectorSize
            << ");\n";
    } break;
    default:
        abort();
    }
}

void genVecNot(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    const uint32_t VectorSize = Dst.LlvmSize * Dst.VectorElementCount / 8;
    Out << "tcg_gen_gvec_not(MO_" << (int)Src.LlvmSize << ", " << Dst << ", "
        << Src << ", " << VectorSize << ", " << VectorSize << ");\n";
}

static void genVecSizeChange(raw_ostream &Out, StringRef Name, const TcgV &Dst,
                             const TcgV &Src)
{
    auto DstSz = vectorSizeInBytes(Dst);
    auto SrcSz = vectorSizeInBytes(Src);
    Out << "tcg_gen_gvec_" << Name << "(MO_" << (int)Dst.LlvmSize << ", MO_"
        << (int)Src.LlvmSize << ", " << Dst << ", " << Src << ", " << DstSz
        << ", " << SrcSz << ", " << DstSz << ");\n";
}

void genVecTrunc(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    genVecSizeChange(Out, "trunc", Dst, Src);
}

void genVecSext(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    genVecSizeChange(Out, "sext", Dst, Src);
}

void genVecZext(raw_ostream &Out, const TcgV &Dst, const TcgV &Src)
{
    genVecSizeChange(Out, "zext", Dst, Src);
}

} // namespace tcg

namespace c
{

inline StringRef mapCPredicate(const CmpInst::Predicate &Pred)
{
    switch (Pred) {
    case CmpInst::ICMP_EQ:
        return "==";
    case CmpInst::ICMP_NE:
        return "!=";
    case CmpInst::ICMP_UGT:
        return ">";
    case CmpInst::ICMP_UGE:
        return ">=";
    case CmpInst::ICMP_ULT:
        return "<";
    case CmpInst::ICMP_ULE:
        return "<=";
    case CmpInst::ICMP_SGT:
        return ">";
    case CmpInst::ICMP_SGE:
        return ">=";
    case CmpInst::ICMP_SLT:
        return "<";
    case CmpInst::ICMP_SLE:
        return "<=";
    default:
        abort();
    }
}

enum BinOpSrcCast {
    CastNone,
    CastSigned,
    CastUnsigned,
};

static std::string mapBinOp(const Instruction::BinaryOps &Opcode,
                            const TcgV &Src0, const TcgV &Src1)
{
    assert(Src0.Kind == IrImmediate and Src1.Kind == IrImmediate);
    std::string Op;
    BinOpSrcCast CastSrc0 = CastNone;
    BinOpSrcCast CastSrc1 = CastNone;
    switch (Opcode) {
    case Instruction::Add:
        Op = "+";
        break;
    case Instruction::And:
        Op = "&";
        break;
    case Instruction::AShr:
        CastSrc0 = CastUnsigned;
        Op = ">>";
        break;
    case Instruction::LShr:
        CastSrc0 = CastSigned;
        Op = ">>";
        break;
    case Instruction::Shl:
        Op = "<<";
        break;
    case Instruction::Mul:
        Op = "*";
        break;
    case Instruction::UDiv:
        CastSrc0 = CastUnsigned;
        CastSrc1 = CastUnsigned;
        Op = "/";
        break;
    case Instruction::SDiv:
        CastSrc0 = CastSigned;
        CastSrc1 = CastSigned;
        Op = "/";
        break;
    case Instruction::Or:
        Op = "|";
        break;
    case Instruction::Sub:
        Op = "-";
        break;
    case Instruction::Xor:
        Op = "^";
        break;
    default:
        abort();
    }

    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);
    ExprStream << "(";
    if (CastSrc0 != CastNone) {
        auto IntPrefix = (CastSrc0 == CastSigned) ? "int" : "uint";
        ExprStream << "(" << IntPrefix << (int)Src0.LlvmSize << "_t) ";
    }
    ExprStream << Src0 << " " << Op << " ";
    if (CastSrc1 != CastNone) {
        auto IntPrefix = (CastSrc1 == CastSigned) ? "int" : "uint";
        ExprStream << "(" << IntPrefix << (int)Src1.LlvmSize << "_t) ";
    }
    ExprStream << Src1 << ")";
    ExprStream.flush();

    return Expr;
}

TcgV ptrAdd(const TcgV &Ptr, const TcgV &Offset)
{
    assert(Offset.Kind == IrConst or Offset.Kind == IrImmediate);
    switch (Ptr.Kind) {
    case IrConst:
    case IrImmediate: {
        std::string Expr = "";
        llvm::raw_string_ostream ExprStream(Expr);
        ExprStream << "(uint" << (int)Ptr.TcgSize << "_t *) ((uintptr_t) "
                   << Ptr << " + " << Offset << ")";
        ExprStream.flush();
        return TcgV::makeImmediate(Expr, Ptr.TcgSize, Ptr.LlvmSize);
    } break;
    case IrPtrToOffset: {
        std::string Expr = "";
        llvm::raw_string_ostream ExprStream(Expr);
        ExprStream << "(" << Ptr << " + " << Offset << ")";
        ExprStream.flush();
        TcgV Tcg(Expr, Ptr.TcgSize, Ptr.LlvmSize, Ptr.VectorElementCount,
                 IrPtrToOffset);
        Tcg.ConstantExpression = true;
        return Tcg;
    } break;
    default:
        abort();
    }
}

TcgV ternary(const TcgV &Cond, const TcgV &True, const TcgV &False)
{
    assert(Cond.Kind == IrImmediate);
    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);
    ExprStream << "(" << Cond << " ? " << True << " : " << False << ")";
    ExprStream.flush();
    return TcgV::makeImmediate(Expr, True.TcgSize, True.LlvmSize);
}

TcgV deref(const TcgV &Ptr, uint32_t LlvmSize, uint32_t TcgSize)
{
    assert(Ptr.Kind == IrImmediate);
    std::string Expr = Twine("*").concat(tcg::getName(Ptr)).str();
    return TcgV::makeImmediate(Expr, TcgSize, LlvmSize);
}

TcgV compare(const CmpInst::Predicate &Pred, const TcgV &Src0, const TcgV &Src1)
{
    assert(Src0.Kind == IrImmediate and Src1.Kind == IrImmediate);
    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);
    ExprStream << "(" << Src0 << " " << mapCPredicate(Pred) << " " << Src1
               << ")";
    ExprStream.flush();
    return TcgV::makeImmediate(Expr, Src0.TcgSize, 1);
}

TcgV zext(const TcgV &V, uint32_t LlvmSize, uint32_t TcgSize)
{
    assert(V.Kind == IrImmediate);
    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);
    ExprStream << "((uint" << (int)LlvmSize << "_t) (uint" << (int)V.TcgSize
               << "_t) " << V << ")";
    ExprStream.flush();
    return TcgV::makeImmediate(Expr, TcgSize, LlvmSize);
}

TcgV sext(const TcgV &V, uint32_t LlvmSize, uint32_t TcgSize)
{
    assert(V.Kind == IrImmediate);
    std::string Expr = "";
    llvm::raw_string_ostream ExprStream(Expr);
    ExprStream << "((int" << (int)LlvmSize << "_t) (int" << (int)V.TcgSize
               << "_t) " << V << ")";
    ExprStream.flush();
    return TcgV::makeImmediate(Expr, TcgSize, LlvmSize);
}

TcgV binop(Instruction::BinaryOps Opcode, const TcgV &Src0, const TcgV &Src1)
{
    std::string Op = mapBinOp(Opcode, Src0, Src1);
    uint32_t LargestLlvmSize = std::max(Src0.LlvmSize, Src1.LlvmSize);
    uint32_t LargestTcgSize = llvmToTcgSize(LargestLlvmSize);
    return TcgV::makeImmediate(Op, LargestTcgSize, LargestLlvmSize);
}

void emitVectorPreamble(raw_ostream &Out)
{
    Out << "typedef struct VectorMem {\n";
    Out << "    uint32_t allocated;\n";
    Out << "} VectorMem;\n\n";

    Out << "static intptr_t temp_new_gvec(VectorMem *mem, uint32_t size)\n";
    Out << "{\n";
    Out << "    uint32_t off = ROUND_UP(mem->allocated, size);\n";
    Out << "    g_assert(off + size <= STRUCT_ARRAY_SIZE(CPUArchState, "
           "tmp_vmem));\n";
    Out << "    mem->allocated = off + size;\n";
    Out << "    return offsetof(CPUArchState, " << TempVectorBlock
        << ") + off;\n";
    Out << "}\n";
}

void emitVectorMemVar(raw_ostream &Out) { Out << "VectorMem mem = {0};\n"; }

} // namespace c
