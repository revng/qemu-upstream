#pragma once

#include "TcgType.h"

#include "llvm/ADT/Twine.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

#include <string>

extern uint32_t NumTempVectorOffsets;

namespace c {
inline llvm::StringRef uintStr(uint8_t Size) {
  return llvm::Twine("uint")
      .concat(Twine(Size))
      .concat("_t")
      .getSingleStringRef();
}
} // namespace c

namespace tcg {

using namespace llvm;

// TODO(anjo): The names we give temporaries depend on the function we're in,
// maybe we can put this name/index stuff somewhere more relevant?
void resetNameIndices();
const std::string mkName(const Value *V);
const std::string mkName(const std::string Suffix);
const std::string mkLabelName();

// String representation of types
const std::string getType(const TcgV &Value);

using ArgVec = SmallVector<const TcgV *, 4>;

inline ArgKindMask argKindMask(const ArgVec &Args) {
  assert(Args.size() <= 8 * sizeof(ArgKindMask) / ArgKindBits);
  ArgKindMask Mask = 0;
  ArgKindMask Shift = 0;
  for (auto Arg : Args) {
    Mask |= (ArgKindMask)Arg->Kind << Shift;
    Shift += ArgKindBits;
  }
  return Mask;
}

inline ArgKindMask
argKindMask(const std::initializer_list<ArgKindMask> ArgKinds) {
  assert(ArgKinds.size() <= 8 * sizeof(ArgKindMask) / ArgKindBits);
  ArgKindMask Mask = 0;
  ArgKindMask Shift = 0;
  for (auto Kind : ArgKinds) {
    Mask |= Kind << Shift;
    Shift += ArgKindBits;
  }
  return Mask;
}

inline bool verifyArgs(const ArgVec &Args,
                       const std::initializer_list<ArgKindMask> ArgKinds) {
  const ArgKindMask Mask = argKindMask(ArgKinds);
  return (tcg::argKindMask(Args) | Mask) == Mask;
}

inline std::string getName(const TcgV &V) {
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
      assert(false);
    };
  }
}

inline const TcgV materialize(const TcgV &Value) {
  if (Value.Kind != IrImmediate)
    return Value;
  TcgV M = Value;
  M.Name = Twine("tcg_constant_i")
               .concat(Twine(Value.TcgSize))
               .concat("(")
               .concat(tcg::getName(Value))
               .concat(")")
               .str();
  M.Kind = IrConst;
  return M;
}

inline raw_ostream &operator<<(raw_ostream &Out, const TcgV &V) {
  Out << getName(V);
  return Out;
}

template <typename I>
void emitArgListTcg(raw_ostream &Out, const I Beg, const I End) {
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
void emitCall(raw_ostream &Out, const StringRef &S, const I Beg, const I End) {
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
                 Iterator End) {
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
                           const std::initializer_list<TcgV> Args) {
  emitArgListTcg(Out, Args.begin(), Args.end());
}

inline void emitCall(raw_ostream &Out, const StringRef &S,
                     const std::initializer_list<StringRef> Args) {
  emitCall(Out, S, Args.begin(), Args.end());
}

inline void emitCallTcg(raw_ostream &Out, const StringRef &S,
                        std::initializer_list<TcgV> Args) {
  emitCallTcg(Out, S, Args.begin(), Args.end());
}

inline void emitBinOp(raw_ostream &Out, const TcgBinOp Funk,
                      std::initializer_list<TcgV> Args) {
  emitCallTcg(Out, Funk.Code, Args.begin(), Args.end());
}

inline void genCallHelper(raw_ostream &Out, const StringRef &Helper,
                          const std::initializer_list<TcgV> Args) {
  auto Func = Twine("gen_").concat(Helper).str();
  emitCallTcg(Out, Func, Args);
}

template <typename I>
void genCallHelper(raw_ostream &Out, const StringRef &Helper, I Beg, I End) {
  auto Func = Twine("gen_").concat(Helper).str();
  emitCallTcg(Out, Func, Beg, End);
}

inline void tempNew(raw_ostream &Out, const TcgV &Value) {
  if (Value.Kind == IrValue) {
    Out << "tcg_temp_new_i" << std::to_string(Value.TcgSize) << "();\n";
  }
}

inline void tempNewPtr(raw_ostream &Out) { Out << "tcg_temp_new_ptr();\n"; }

inline void tempNewVec(raw_ostream &Out) {
  Out << "get_new_temp_vector_offset(" << NumTempVectorOffsets++ << ");\n";
}

inline void genNewLabel(raw_ostream &Out) { Out << "gen_new_label();\n"; }

inline void genSetLabel(raw_ostream &Out, const TcgV &L) {
  assert(L.Kind == IrLabel);
  Out << "gen_set_label(" << L << ");\n";
}

inline void defineNewTemp(raw_ostream &Out, const TcgV &Tcg) {
  assert(!Tcg.ConstantExpression);
  Out << tcg::getType(Tcg) << " " << tcg::getName(Tcg) << " = ";
  switch (Tcg.Kind) {
  case IrValue:
    tcg::tempNew(Out, Tcg);
    break;
  case IrPtr:
    tcg::tempNewPtr(Out);
    break;
  case IrPtrToOffset:
    tcg::tempNewVec(Out);
    break;
  case IrLabel:
    tcg::genNewLabel(Out);
    break;
  default:
    abort();
  }
}

static const TcgV MmuIndex =
    TcgV::makeImmediate("get_tb_mmu_index(tcg_ctx->gen_tb->flags)", 32, 32);

inline void genBr(raw_ostream &Out, const TcgV &L) {
  assert(L.Kind == IrLabel);
  Out << "tcg_gen_br(" << L << ");\n";
}

inline void genTempInit(raw_ostream &Out, const TcgV &Arg1,
                        const StringRef Str) {
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_const_i" << Arg1.TcgSize << "(" << Str << ");\n";
}

inline void genTempInit(raw_ostream &Out, const TcgV &Arg1, uint64_t Value) {
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_const_i" << Arg1.TcgSize << "((uint64_t)" << Value << "ULL);\n";
}

inline void genTempInit(raw_ostream &Out, const TcgV &Arg1, const TcgV &Arg2) {
  assert(Arg2.Kind == IrImmediate);
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_const_i" << Arg1.TcgSize << "(" << Arg2 << ");\n";
}

inline void genAssignConst(raw_ostream &Out, const TcgV &Arg1,
                           const StringRef Str) {
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_constant_i" << Arg1.TcgSize << "(" << Str << ");\n";
}

inline void genAssignConst(raw_ostream &Out, const TcgV &Arg1, uint64_t Value) {
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_constant_i" << Arg1.TcgSize << "((uint64_t)" << Value
      << "ULL);\n";
}

inline void genAssignConst(raw_ostream &Out, const TcgV &Arg1,
                           const TcgV &Arg2) {
  assert(Arg2.Kind == IrImmediate);
  Out << getType(Arg1) << ' ' << Arg1 << " = "
      << "tcg_constant_i" << Arg1.TcgSize << "(" << Arg2 << ");\n";
}

inline void genExtI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == 64);
  assert(Src.TcgSize == 32);
  emitCallTcg(Out, "tcg_gen_ext_i32_i64", {Dst, Src});
}

inline void genExtrlI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == 32);
  assert(Src.TcgSize == 64);
  emitCallTcg(Out, "tcg_gen_extrl_i64_i32", {Dst, Src});
}

inline void genExtuI32I64(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == 64);
  assert(Src.TcgSize == 32);
  emitCallTcg(Out, "tcg_gen_extu_i32_i64", {Dst, Src});
}

inline void genExtrhI64I32(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == 32);
  assert(Src.TcgSize == 64);
  emitCallTcg(Out, "tcg_gen_extrh_i64_i32", {Dst, Src});
}

inline void genExtract(raw_ostream &Out, bool Sign, const TcgV &Dst,
                       const TcgV &Src, const TcgV &Offset,
                       const TcgV &Length) {
  // assert(verifyArgs(Args, {IrValue | IrImmediate, IrImmediate,
  // IrImmediate}));
  assert(Dst.TcgSize == Src.TcgSize);
  const char *SignStr = (Sign) ? "s" : "";
  const TcgV &MSrc = materialize(Src);
  Out << "tcg_gen_" << SignStr << "extract_i" << Dst.TcgSize << "_safe(";
  emitArgListTcg(Out, {Dst, MSrc, Offset, Length});
  Out << ");\n";
}

inline void genDeposit(raw_ostream &Out, const TcgV &Dst, const TcgV &Into,
                       const TcgV &From, const TcgV &Offset,
                       const TcgV &Length) {
  // assert(verifyArgs(Args, {IrValue | IrImmediate, IrImmediate, IrImmediate,
  //                          IrValue | IrImmediate}));
  assert(Dst.TcgSize == Into.TcgSize);
  assert(Dst.TcgSize == From.TcgSize or From.Kind == IrImmediate);
  Out << "tcg_gen_deposit_i" << Dst.TcgSize << "_safe(";
  const TcgV MInto = materialize(Into);
  const TcgV MLength = materialize(Length);
  emitArgListTcg(Out, {Dst, MInto, MLength, From, Offset});
  Out << ");\n";
}

inline void genTruncPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  auto FuncStr = Twine("tcg_gen_trunc_i")
                     .concat(std::to_string(Src.TcgSize))
                     .concat("_ptr")
                     .str();
  emitCallTcg(Out, FuncStr, {Dst, Src});
}

inline void genConcat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src1,
                      const TcgV &Src2) {
  assert(Dst.TcgSize == 64);
  assert(Src1.TcgSize == 32);
  assert(Src2.TcgSize == 32);
  emitCallTcg(Out, "tcg_gen_concat_i32_i64", {Dst, Src1, Src2});
}

inline void genMov(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == Src.TcgSize);
  Out << "tcg_gen_mov_i" << Dst.TcgSize << "(" << Dst << ", " << Src << ");\n";
}

inline void genMovPtr(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Dst.TcgSize == Src.TcgSize);
  assert(Dst.Kind == IrPtr);
  assert(Src.Kind == IrPtr);
  Out << "tcg_gen_mov_ptr(" << Dst << ", " << Src << ");\n";
}

inline void genMovI(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  assert(Src.Kind == IrImmediate);
  Out << "tcg_gen_movi_i" << Dst.TcgSize << "(" << Dst << ", " << Src << ");\n";
}

inline void genMovcond(raw_ostream &Out, const StringRef Cond, const TcgV &Ret,
                       const TcgV &C1, const TcgV &C2, const TcgV &V1,
                       const TcgV &V2) {
  assert(Ret.TcgSize == C1.TcgSize);
  assert(Ret.TcgSize == C2.TcgSize);
  assert(Ret.TcgSize == V1.TcgSize);
  assert(Ret.TcgSize == V2.TcgSize);
  const TcgV mC1 = materialize(C1);
  const TcgV mC2 = materialize(C2);
  const TcgV mV1 = materialize(V1);
  const TcgV mV2 = materialize(V2);
  Out << "tcg_gen_movcond_i" << Ret.TcgSize << '(' << Cond << ", ";
  emitArgListTcg(Out, {Ret, mC1, mC2, mV1, mV2});
  Out << ");\n";
}

inline void genSetcond(raw_ostream &Out, const StringRef Cond, const TcgV &Dst,
                       const TcgV &Op1, const TcgV &Op2) {
  assert(Op1.TcgSize == Op2.TcgSize);
  assert(Op1.TcgSize == Dst.TcgSize);
  assert(Op1.TcgSize == 32 or Op1.TcgSize == 64);
  Out << "tcg_gen_setcond_i" << Dst.TcgSize << "(" << Cond << ", " << Dst
      << ", " << Op1 << ", " << Op2 << ");\n";
}

inline void genSetcondI(raw_ostream &Out, const StringRef Cond, const TcgV &Dst,
                        const TcgV &Op1, const TcgV &Op2) {
  assert(Op1.TcgSize == Dst.TcgSize);
  assert(Op1.TcgSize == 32 or Op1.TcgSize == 64);
  assert(Dst.Kind != IrImmediate && Op1.Kind != IrImmediate &&
         Op2.Kind == IrImmediate);
  Out << "tcg_gen_setcondi_i" << Dst.TcgSize << "(" << Cond << ", " << Dst
      << ", " << Op1 << ", " << Op2 << ");\n";
}

inline void genBrcond(raw_ostream &Out, const StringRef Cond, const TcgV &Arg1,
                      const TcgV &Arg2, const TcgV &Label) {
  assert(Arg1.TcgSize == Arg2.TcgSize);
  assert(Arg1.TcgSize == 32 || Arg1.TcgSize == 64);
  assert(Label.Kind == IrLabel);
  if (Arg2.Kind == IrImmediate) {
    Out << "tcg_gen_brcondi_i" << Arg1.TcgSize;
  } else {
    Out << "tcg_gen_brcond_i" << Arg1.TcgSize;
  }
  Out << "(" << Cond << ", " << materialize(Arg1) << ", " << Arg2 << ", "
      << Label << ");\n";
}

inline void genQemuLoad(raw_ostream &Out, const TcgV &Dst,
                        const TcgV &Ptr, const char* MemOpStr){
  assert(Dst.Kind == IrValue);
  assert(Ptr.Kind != IrImmediate);
  const auto MPtr = materialize(Ptr);
  Out << "tcg_gen_qemu_ld_i" << Dst.TcgSize << "(";
  emitArgListTcg(Out, {Dst, MPtr, MmuIndex});
  Out << ", " << MemOpStr << ");\n";
}

inline void genQemuStore(raw_ostream &Out, const TcgV &Ptr,
                         const TcgV &Src, const char *MemOpStr) {
  assert(Src.Kind == IrValue);
  assert(Ptr.Kind != IrImmediate);
  const auto MPtr = materialize(Ptr);
  Out << "tcg_gen_qemu_st_i" << Src.TcgSize << "(";
  emitArgListTcg(Out, {Src, MPtr, MmuIndex});
  Out << ", " << MemOpStr << ");\n";
}

inline void genLd(raw_ostream &Out, const TcgV &Dst, const TcgV &Ptr,
                  uint64_t Offset) {
  assert(Ptr.Kind == IrPtr);
  // First output the correct tcg function for the widths of Dst
  if (Dst.LlvmSize < Dst.TcgSize) {
    // TODO(anjo): Here we are using ldu, What about singed values?
    Out << "tcg_gen_ld" << Dst.LlvmSize << "u_i" << Dst.TcgSize;
  } else {
    Out << "tcg_gen_ld_i" << Dst.TcgSize;
  }
  // Then emit params
  Out << "(" << Dst << ", " << Ptr << ", " << Offset << ");\n";
}

inline void genSt(raw_ostream &Out, const TcgV &Ptr, const TcgV &Src,
                  uint64_t Offset) {
  assert(Ptr.Kind == IrPtr);
  // First output the correct tcg function for the widths of Dst
  if (Src.LlvmSize < Src.TcgSize) {
    Out << "tcg_gen_st" << Src.LlvmSize << "_i" << Src.TcgSize;
  } else {
    Out << "tcg_gen_st_i" << Src.TcgSize;
  }
  // Then emit params
  Out << "(" << Src << ", " << Ptr << ", " << Offset << ");\n";
}

inline void genVecBinOpName(raw_ostream &Out, StringRef Func, TcgVKind Kind) {
    Out << Func;
    switch (Kind) {
    case IrConst:
    case IrValue:
        Out << "s";
        break;
    case IrImmediate:
        Out << "i";
        break;
    case IrPtrToOffset:
        break;
    default:
        abort();
    }
}

inline void genVecBinOp(raw_ostream &Out, StringRef Func, const TcgV &Dst,
                        const TcgV &Src0, const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset,
  // IrValue|IrConst|IrImmediate|IrPtrToOffset}));
  const uint32_t VectorSizeInBytes = Dst.LlvmSize * Dst.VectorElementCount / 8;
  Out << "tcg_gen_gvec_";
  genVecBinOpName(Out, Func, Src1.Kind);
  Out << "(MO_" << Dst.LlvmSize << ", " << Dst
      << ", " << Src0 << ", " << Src1 << ", " << VectorSizeInBytes << ", "
      << VectorSizeInBytes << ");\n";
}

inline void genSignedSatAdd(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                            const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset, IrPtrToOffset}));
  assert(Dst.Kind == IrPtrToOffset);
  genVecBinOp(Out, "ssadd", Dst, Src0, Src1);
}

inline void genSignedSatSub(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                            const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset, IrPtrToOffset}));
  assert(Dst.Kind == IrPtrToOffset);
  genVecBinOp(Out, "sssub", Dst, Src0, Src1);
}

inline void genSignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                         const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset | IrValue | IrImmediate,
  //                          IrPtrToOffset | IrValue | IrImmediate}));
  switch (Dst.Kind) {
  case IrValue: {
    const TcgV MSrc0 = materialize(Src0);
    const TcgV MSrc1 = materialize(Src1);
    Out << "tcg_gen_smax_i" << Dst.TcgSize << "(" << Dst << ", " << MSrc0
        << ", " << MSrc1 << ");\n";
  } break;
  case IrPtrToOffset: {
    genVecBinOp(Out, "smax", Dst, Src0, Src1);
  } break;
  default:
    abort();
  }
}

inline void genUnsignedMax(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                           const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset | IrValue | IrImmediate,
  //                          IrPtrToOffset | IrValue | IrImmediate}));
  switch (Dst.Kind) {
  case IrValue: {
    const TcgV MSrc0 = materialize(Src0);
    const TcgV MSrc1 = materialize(Src1);
    Out << "tcg_gen_umax_i" << Dst.TcgSize << "(" << Dst << ", " << MSrc0
        << ", " << MSrc1 << ");\n";
  } break;
  case IrPtrToOffset: {
    genVecBinOp(Out, "umax", Dst, Src0, Src1);
  } break;
  default:
    abort();
  }
}

inline void genSignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                         const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset | IrValue | IrImmediate,
  //                          IrPtrToOffset | IrValue | IrImmediate}));
  switch (Dst.Kind) {
  case IrValue: {
    const TcgV MSrc0 = materialize(Src0);
    const TcgV MSrc1 = materialize(Src1);
    Out << "tcg_gen_smin_i" << Dst.TcgSize << "(" << Dst << ", " << MSrc0
        << ", " << MSrc1 << ");\n";
  } break;
  case IrPtrToOffset: {
    genVecBinOp(Out, "smin", Dst, Src0, Src1);
  } break;
  default:
    abort();
  }
}

inline void genUnsignedMin(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                           const TcgV &Src1) {
  // assert(verifyArgs(Args, {IrPtrToOffset | IrValue | IrImmediate,
  //                          IrPtrToOffset | IrValue | IrImmediate}));
  switch (Dst.Kind) {
  case IrValue: {
    const TcgV MSrc0 = materialize(Src0);
    const TcgV MSrc1 = materialize(Src1);
    Out << "tcg_gen_umin_i" << Dst.TcgSize << "(" << Dst << ", " << MSrc0
        << ", " << MSrc1 << ");\n";
  } break;
  case IrPtrToOffset: {
    genVecBinOp(Out, "umin", Dst, Src0, Src1);
  } break;
  default:
    assert(false);
  }
}

inline void genFunnelShl(raw_ostream &Out, const TcgV &Dst, const TcgV &Src0,
                         const TcgV &Src1, const TcgV &Shift) {
  // assert(verifyArgs(Args, {IrValue, IrValue, IrValue | IrImmediate})),
  assert(Src0.TcgSize == Dst.TcgSize);
  assert(Src1.TcgSize == Dst.TcgSize);
  assert(Shift.TcgSize == Dst.TcgSize);

  if (Dst.TcgSize == 32) {
    auto Temp = TcgV::makeTemp(64, 64, IrValue);
    tcg::defineNewTemp(Out, Temp);
    tcg::genConcat(Out, Temp, Src1, Src0);
    // TODO(anjo): How can we reuse the BinOp code?
    if (Shift.Kind == IrImmediate) {
      Out << "tcg_gen_shli_i64(" << Temp << ", " << Temp << ", " << Shift
          << ");\n";
    } else {
      auto Ext = TcgV::makeTemp(64, 64, IrValue);
      Out << tcg::getType(Ext) << " " << Ext << " = ";
      tcg::tempNew(Out, Ext);
      genExtuI32I64(Out, Ext, Shift);
      Out << "tcg_gen_shl_i64(" << Temp << ", " << Temp << ", " << Ext
          << ");\n";
    }
    tcg::genExtrhI64I32(Out, Dst, Temp);
  } else if (Dst.TcgSize == 64) {
    auto ASrc0 = materialize(Src0);
    auto ASrc1 = materialize(Src1);
    auto AShift = materialize(Shift);
    genCallHelper(Out, "helper_fshl_i64", {Dst, ASrc0, ASrc1, AShift});
  }
}

inline void genBitreverse(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue}));
  auto FuncName = Twine("helper_bitreverse")
                      .concat(Twine(Dst.LlvmSize))
                      .concat("_i")
                      .concat(Twine(Src.TcgSize))
                      .str();
  tcg::genCallHelper(Out, FuncName, {Dst, Src});
}

inline void genVecMemcpy(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                         const TcgV &Size) {
  // assert(verifyArgs(Args, {IrPtrToOffset, IrPtrToOffset,
  //                          IrValue | IrImmediate | IrConst,
  //                          IrValue | IrImmediate | IrConst}));
  //  TODO(anjo): We don't have a way to access the actual (possible) constant
  //  expression of Args2, it's a string... Otherwise we could
  //  const uint32_t LargestElementSize = std::min(1 << Log2_32(Args[2]), 64);
  // Out << "// hmm " << Args[1]->LlvmSize << "\n";
  Out << "tcg_gen_gvec_mov(MO_8"
      << ", " << Dst << ", " << Src << ", " << Size << ", " << Size << ");\n";
}

inline void genVecMemset(raw_ostream &Out, const TcgV &Dst, const TcgV &Src,
                         const TcgV &Size) {
  // assert(verifyArgs(Args, {IrPtrToOffset, IrValue | IrImmediate | IrConst,
  //                          IrValue | IrImmediate | IrConst,
  //                          IrValue | IrImmediate | IrConst}));
    switch (Src.Kind) {
    case IrValue:
    case IrConst:
        Out << "tcg_gen_gvec_dup_i" << Src.TcgSize << "(MO_" << Src.LlvmSize << ", "
            << Dst << ", " << Size << ", " << Size << ", " << Src << ");\n";
        break;
    case IrImmediate:
        Out << "tcg_gen_gvec_dup_imm" << "(MO_" << Src.LlvmSize << ", "
            << Dst << ", " << Size << ", " << Size << ", " << Src << ");\n";
        break;
    default:
        abort();
    }
}

inline void genVecSplat(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue | IrImmediate | IrConst}));
  const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
  // TODO(anjo): Remove
  const auto Size = TcgV::makeImmediate(Twine(VectorSizeInBytes).str(), 64, 64);
  genVecMemset(Out, Dst, Src, Size);
}

inline void genVecArrSplat(raw_ostream &Out, const TcgV &Env, const TcgV &Dst, SmallVector<TcgV, 16> &Arr) {
  assert(Env.Kind == IrEnv);
  // assert(verifyArgs(Args, {IrValue | IrImmediate | IrConst}));
  const uint32_t VectorSizeInBytes = vectorSizeInBytes(Dst);
  const std::string TmpName = mkName("varr");
  Out << "uint" << Dst.LlvmSize << "_t " << TmpName << "[] = {";
  emitArgListTcg(Out, Arr.begin(), Arr.end());
  Out << "};\n";
  Out << "tcg_gen_gvec_constant_i" << Dst.LlvmSize << "("
      << Env << ", " << Dst << ", " << TmpName
      << ", " << VectorSizeInBytes << ");\n";
}

inline void genAbs(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue | IrPtrToOffset, IrImmediate}));
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
    Out << "tcg_gen_gvec_abs("
        << "MO_" << Dst.LlvmSize << ", " << Dst << ", " << Src << ", "
        << VectorSize << ", " << VectorSize << ");\n";
  } break;
  default:
    abort();
  }
}

inline void genVecNot(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrPtrToOffset, IrPtrToOffset}));
  const uint32_t VectorSize = Dst.LlvmSize * Dst.VectorElementCount / 8;
  Out << "tcg_gen_gvec_not("
      << "MO_" << Src.LlvmSize << ", " << Dst << ", " << Src << ", "
      << VectorSize << ", " << VectorSize << ");\n";
}

// Count leading zeroes
inline void genClzi(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue, IrValue | IrImmediate | IrConst}));
  assert(Dst.TcgSize == Src.TcgSize);
  Out << "tcg_gen_clzi_i" << Dst.TcgSize << "(" << Dst << ", " << Src << ", "
      << Src.TcgSize << ");\n";
}

// Count trailing zeroes
inline void genCtzi(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue, IrValue | IrImmediate | IrConst}));
  assert(Dst.TcgSize == Src.TcgSize);
  Out << "tcg_gen_ctzi_i" << Dst.TcgSize << "(" << Dst << ", " << Src << ", "
      << Src.TcgSize << ");\n";
}

// Count 1's
inline void genCtpop(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue}));
  assert(Dst.TcgSize == Src.TcgSize);
  Out << "tcg_gen_ctpop_i" << Dst.TcgSize << "(" << Dst << ", " << Src
      << ");\n";
}

// Byteswap
inline void genBswap(raw_ostream &Out, const TcgV &Dst, const TcgV &Src) {
  // assert(verifyArgs(Args, {IrValue}));
  assert(Dst.TcgSize == Src.TcgSize);
  Out << "tcg_gen_bswap" << Dst.TcgSize << "_i" << Src.TcgSize << "(" << Dst
      << ", " << Src << ");\n";
}

} // namespace tcg
