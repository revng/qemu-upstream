#include "TcgGenPass.h"

#include "TcgEmit.h"
#include "TcgType.h"

#include "TcgGlobalMapPass.h"
#include "MapAnnotationsPass.h"
#include "TcgTempAllocationPass.h"

#include <utils/Common.h>

#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallBitVector.h"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/raw_ostream.h"

#include "PseudoInst.h"

#include <set> // For .merge() used by ExternalSymbols
#include <algorithm> // For std::swap

using namespace llvm;
using OffsetMapTy = TcgGlobalMapPass::Result;
using AnnotationMapTy = MapAnnotationsPass::AnnotationMapTy;
using TcgGlobal = TcgGlobalMapPass::TcgGlobal;

using ArgVector = SmallVector<TcgV, 4>;

class TcgSizeAdapter {
  raw_ostream &Out;
  const TcgV Orig;
  Optional<TcgV> Adapted;

public:
  TcgSizeAdapter(raw_ostream &Out, const TcgV &Orig) :
    Out(Out), Orig(Orig), Adapted(None) {}

  ~TcgSizeAdapter() { assert(!Adapted.hasValue()); }

  const TcgV get(uint32_t Size) {
    if (Orig.Kind == IrImmediate or (Orig.TcgSize == Size)) {
      return Orig;
    } else if (!Adapted.hasValue()) {
      initAdapted(Size);
    }
    return *Adapted;
  }

  void free() {
    if (Adapted.hasValue())
      Adapted = None;
  }

private:
  void initAdapted(uint32_t Size) {
    assert(!Adapted.hasValue());
    assert((Size == 32 and Orig.TcgSize == 64) or (Size == 64 and Orig.TcgSize == 32));

    auto New = TcgV::makeTemp(Size, Orig.LlvmSize, Orig.Kind);
    tcg::defineNewTemp(Out, New);
    if (Size == 32) {
      tcg::genExtrlI64I32(Out, New, Orig);
    } else {
      tcg::genExtuI32I64(Out, New, Orig);
    }
    Adapted = std::move(New);
  }
};

inline StringRef mapCPredicate(const CmpInst::Predicate &Pred) {
  switch (Pred) {
  case CmpInst::ICMP_EQ:  return "==";
  case CmpInst::ICMP_NE:  return "!=";
  case CmpInst::ICMP_UGT: return ">";
  case CmpInst::ICMP_UGE: return ">=";
  case CmpInst::ICMP_ULT: return "<";
  case CmpInst::ICMP_ULE: return "<=";
  case CmpInst::ICMP_SGT: return ">";
  case CmpInst::ICMP_SGE: return ">=";
  case CmpInst::ICMP_SLT: return "<";
  case CmpInst::ICMP_SLE: return "<=";
  default: assert(false);
  }
}

inline StringRef mapPredicate(const CmpInst::Predicate &Pred) {
  switch (Pred) {
  case CmpInst::ICMP_EQ:  return "TCG_COND_EQ";
  case CmpInst::ICMP_NE:  return "TCG_COND_NE";
  case CmpInst::ICMP_UGT: return "TCG_COND_GTU";
  case CmpInst::ICMP_UGE: return "TCG_COND_GEU";
  case CmpInst::ICMP_ULT: return "TCG_COND_LTU";
  case CmpInst::ICMP_ULE: return "TCG_COND_LEU";
  case CmpInst::ICMP_SGT: return "TCG_COND_GT";
  case CmpInst::ICMP_SGE: return "TCG_COND_GE";
  case CmpInst::ICMP_SLT: return "TCG_COND_LT";
  case CmpInst::ICMP_SLE: return "TCG_COND_LE";
  default: assert(false);
  }
}

class Mapper {
  raw_ostream &Out;
  llvm::DenseMap<Value *, TcgV> Map;
  llvm::DenseMap<BasicBlock *, TcgV> Labels;

  // Keep track of whether a TcgV has been defined already, or not
  SmallBitVector HasBeenDefined;

  std::set<StringRef> &ExternalSymbols;
  const TempAllocationData &TAD;

public:
  Mapper(raw_ostream &Out,
         const OffsetMapTy &OffsetMap,
         const Module &M,
         std::set<StringRef> &ExternalSymbols,
         const TempAllocationData &TAD)
    : Out(Out),
      ExternalSymbols(ExternalSymbols),
      TAD(TAD) {
    // Default to size of previously mapped TcgVs
    HasBeenDefined.resize(TAD.Map.size());
  }

  Expected<TcgV> getMapped(Value *V) {
    auto It = Map.find(V);
    if (It != Map.end()) {
      return It->second;
    }
    return mkError("Value not mapped");
  }

  TcgV mapBbAndEmit(BasicBlock *BB) {
    auto Find = Labels.find(BB);
    if (Find == Labels.end()) {
      TcgV Label = TcgV::makeLabel();
      tcg::defineNewTemp(Out, Label);
      return Labels.try_emplace(BB, Label).first->second;
    }
    return Find->second;
  }

  Expected<TcgBinOp> mapCBinOp(const Instruction::BinaryOps &Opcode,
                               const TcgV &Arg1,
                               const TcgV &Arg2) {
    assert(Arg1.Kind == IrImmediate and Arg2.Kind == IrImmediate);
    std::string Ret{};
    std::string CastArg1{};
    std::string CastArg2{};
    switch (Opcode) {
    case Instruction::Add:
      Ret = "+";
      break;
    case Instruction::And:
      Ret = "&";
      break;
    case Instruction::AShr:
      CastArg1 = "(int" + std::to_string(Arg1.LlvmSize) + "_t) ";
      Ret = ">>";
      break;
    case Instruction::LShr:
      CastArg1 = "(uint" + std::to_string(Arg1.LlvmSize) + "_t) ";
      Ret = ">>";
      break;
    case Instruction::Shl:
      Ret = "<<";
      break;
    case Instruction::Mul:
      Ret = "*";
      break;
    case Instruction::UDiv:
      CastArg1 = "(uint" + std::to_string(Arg1.LlvmSize) + "_t) ";
      Ret = "/";
      CastArg2 = "(uint" + std::to_string(Arg2.LlvmSize) + "_t) ";
      break;
    case Instruction::SDiv:
      CastArg1 = "(int" + std::to_string(Arg1.LlvmSize) + "_t) ";
      Ret = "/";
      CastArg2 = "(int" + std::to_string(Arg1.LlvmSize) + "_t) ";
      break;
    case Instruction::Or:
      Ret = "|";
      break;
    case Instruction::Sub:
      Ret = "-";
      break;
    case Instruction::Xor:
      Ret = "^";
      break;
    default:
      return mkError("binary operator not implemented");
    }

    auto NameStr = Twine("(").concat(CastArg1).concat(tcg::getName(Arg1))
      .concat(" ").concat(Ret).concat(" ")
      .concat(CastArg2).concat(tcg::getName(Arg2)).concat(")")
      .str();

    return (TcgBinOp) {
      .Code = NameStr,
    };
  }

  Expected<TcgBinOp> mapTempBinOp(const Instruction::BinaryOps &Opcode,
                                  const TcgV &Arg1,
                                  const TcgV &Arg2) {
    bool IsImmediate = (Arg1.Kind == IrImmediate or Arg2.Kind == IrImmediate);
    bool IsPtr = (Opcode == Instruction::Add and
                  (Arg1.Kind == IrPtr or Arg2.Kind == IrPtr));
    assert(IsImmediate or Arg1.TcgSize == Arg2.TcgSize);
    std::string Ret;

    // Check for valid boolean operations if operating on a boolean
    if (Arg1.LlvmSize == 1) {
      assert(Arg2.LlvmSize == 1);
      assert(Arg1.TcgSize == 32 or Arg1.TcgSize == 64);
      assert(Arg2.TcgSize == 32 or Arg2.TcgSize == 64);
      switch (Opcode) {
      case Instruction::And:
      case Instruction::Or:
      case Instruction::Xor:
        break;
      default:
        return mkError("Bitwise operator with unsupported operation");
      }
    }

    bool NeedSafe = false;
    switch (Opcode) {
    case Instruction::Add:  Ret = "tcg_gen_add";  break;
    case Instruction::Sub:  Ret = "tcg_gen_sub";  break;
    case Instruction::And:  Ret = "tcg_gen_and";  break;
    case Instruction::Or:   Ret = "tcg_gen_or";   break;
    case Instruction::Xor:  Ret = "tcg_gen_xor";  break;
    case Instruction::Mul:  Ret = "tcg_gen_mul";  break;
    case Instruction::UDiv: Ret = "tcg_gen_divu"; break;
    case Instruction::SDiv: Ret = "tcg_gen_div";  break;
    case Instruction::AShr: Ret = "tcg_gen_sar";  NeedSafe = true; break;
    case Instruction::LShr: Ret = "tcg_gen_shr";  NeedSafe = true; break;
    case Instruction::Shl:  Ret = "tcg_gen_shl";  NeedSafe = true; break;
    default: return mkError("binary operator not implemented");
    }

    if (IsImmediate)
      Ret += "i";

    if (IsPtr)
      Ret += "_ptr";
    else
      Ret += "_i" + std::to_string(Arg1.TcgSize);

    if (IsImmediate and NeedSafe)
      Ret += "_safe";

    return (TcgBinOp){
      .Code = Ret,
    };
  }

  Expected<TcgVecBinOp> mapVecBinOp(const Instruction::BinaryOps &Opcode,
                                    const TcgV &Arg1,
                                    const TcgV &Arg2) {
    bool IsShift = Opcode == Instruction::Shl or Opcode == Instruction::LShr or Opcode == Instruction::AShr;

    std::string suffix;
    switch (Arg2.Kind) {
    case IrPtrToOffset: suffix = (IsShift) ? "v" : ""; break;
    case IrValue:       suffix = "s"; break;
    case IrImmediate:   suffix = "i"; break;
    default: return mkError("Unsupported kind for 2nd arg of vector bin. op.");
    }

    TcgVecBinOp BinOp;
    if (Arg2.Kind ==  IrValue) {
        if (IsShift) {
            BinOp.RequiredOp2Size = 32;
        } else {
            BinOp.RequiredOp2Size = 64;
        }
    }

    switch (Opcode) {
    // upcast 64
    case Instruction::Add: BinOp.Code = "add" + suffix; break;
    case Instruction::Sub: BinOp.Code = "sub" + suffix; break;
    case Instruction::Mul: BinOp.Code = "mul" + suffix; break;
    case Instruction::And: BinOp.Code = "and" + suffix; break;
    case Instruction::Or:  BinOp.Code = "or"  + suffix; break;
    case Instruction::Xor: BinOp.Code = "xor" + suffix; break;

    // downcast 32
    case Instruction::Shl:  BinOp.Code = "shl" + suffix; break;
    case Instruction::LShr: BinOp.Code = "shr" + suffix; break;
    case Instruction::AShr: BinOp.Code = "sar" + suffix; break;
    default: return mkError("Unhandled vector bin. op.");
    }

    return BinOp;
  }

  void mapExplicitly(Value *Val, const TcgV &TcgVal) {
    assert(Map.find(Val) == Map.end());
    Map.try_emplace(Val, TcgVal);
  }

  void mapClear(Value *Val) {
    auto It = Map.find(Val);
    assert(It != Map.end());
    Map.erase(It);
  }

  Expected<TcgV> mapAndEmit(Value *V) {
    if (auto Mapped = getMapped(V)) {
      return Mapped.get();
    }

    auto It = TAD.Map.find(V);
    if (It == TAD.Map.end()) {
      return mkError("Unable to map value: ", V);
    }

    const TcgV Tcg = It->second;

    bool IsArg = TAD.Args.ArgInfoMap.find(V) != TAD.Args.ArgInfoMap.end();

    if (Tcg.Id >= HasBeenDefined.size()) {
      HasBeenDefined.resize(Tcg.Id+1);
    }

    if (!IsArg and !HasBeenDefined[Tcg.Id] and (!TAD.ReturnValue.hasValue() or Tcg != *TAD.ReturnValue) and Tcg.Kind !=  IrImmediate and Tcg.Kind != IrConst) {
      HasBeenDefined.set(Tcg.Id);

      if (Tcg.Kind == IrPtrToOffset) {
        auto ConstV = dyn_cast<Constant>(V);
        if (ConstV) {
          Constant *Splat = ConstV->getSplatValue();
          if (Splat) {
            // Constant splatted vector
            tcg::defineNewTemp(Out, Tcg);
            auto It = TAD.Map.find(Splat);
            assert(It != TAD.Map.end());
            // TODO(anjo): Remove
            auto Size = TcgV::makeImmediate(Twine(vectorSizeInBytes(Tcg)).str(), 64, 64);
            tcg::genVecMemset(Out, Tcg, It->second, Size);
          } else {
            // Constant non-splatted vector
            SmallVector<TcgV, 16> Arr;
            for (unsigned I = 0; I < Tcg.VectorElementCount; ++I) {
              Constant *Element = ConstV->getAggregateElement(I);
              auto It = TAD.Map.find(Element);
              assert(It != TAD.Map.end());
              Arr.push_back(It->second);
            }
            tcg::defineNewTemp(Out, Tcg);

            assert(TAD.Args.EnvPtr.hasValue());
            auto It = TAD.Map.find(*TAD.Args.EnvPtr);
            assert(It != TAD.Map.end());
            TcgV Env = It->second;
            tcg::genVecArrSplat(Out, Env, Tcg, Arr);
          }
        } else {
          tcg::defineNewTemp(Out, Tcg);
        }
      } else {
        tcg::defineNewTemp(Out, Tcg);
      }
    }
    return Map.try_emplace(V, It->second).first->second;
  }

  Expected<TcgV> mapCondAndEmit(Value *V, uint32_t TcgSize, uint32_t LlvmSize) {
    if (auto Mapped = getMapped(V)) {
      assert(Mapped.get().LlvmSize == 1);
      return Mapped.get();
    }

    auto It = TAD.Map.find(const_cast<Value *>(V));
    if (It == TAD.Map.end()) {
      return mkError("Unable to map cond: ", V);
    }

    const TcgV Tcg = It->second;
    if (Tcg.Id >= HasBeenDefined.size()) {
      HasBeenDefined.resize(Tcg.Id+1);
    }
    if (!HasBeenDefined[Tcg.Id] and (!TAD.ReturnValue.hasValue() or Tcg != *TAD.ReturnValue)) {
      HasBeenDefined.set(Tcg.Id);
      tcg::defineNewTemp(Out, Tcg);
    }
    return Map.try_emplace(V, It->second).first->second;
  }
};

struct TranslatedFunction {
  StringRef Name;
  std::string Decl;
  std::string Code;
  std::set<StringRef> ExternalSymbols;
};

static void ensureSignBitIsSet(raw_ostream &Out, const TcgV &V) {
  if (V.LlvmSize == V.TcgSize or V.Kind != IrValue) {
    return;
  }
  tcg::genExtract(Out, true, V, V,
                  TcgV::makeImmediate("0", V.TcgSize, V.LlvmSize),
                  TcgV::makeImmediate(Twine(V.LlvmSize).str(), V.TcgSize, V.LlvmSize));
}

static Expected<TranslatedFunction>
translateFunction(const Function *F,
                  const OffsetMapTy &OffsetMap,
                  const AnnotationMapTy &AnnotationMap,
                  const TempAllocationData &TAD,
                  const SmallPtrSet<Function *, 16> HasTranslatedFunction) {
  TranslatedFunction TF = {
    .Name = F->getName(),
  };

  // Remove prefix for helper functions to get cleaner emitted names
  TF.Name.consume_front("helper_");

  raw_string_ostream Out(TF.Code);
  raw_string_ostream HeaderWriter(TF.Decl);
  // TODO(anjo): Keep as buffered and flush
  Out.SetUnbuffered();
  HeaderWriter.SetUnbuffered();

  // Functions that should be ignored are convereted
  // to declarations, see FilterFunctionsPass.
  if (F->isDeclaration()) {
    return mkError("Function is not translated");
  }

  Mapper Mapper(Out, OffsetMap, *F->getParent(), TF.ExternalSymbols, TAD);
  Optional<TcgV> RetVal = None;


  Out << "// " << *F->getReturnType() << ' ' << F->getName() << '\n';
  HeaderWriter << "void " << "emit_" << TF.Name << '(';
  std::vector<TcgV> CArgs;

  if (!F->getReturnType()->isVoidTy()) {
    assert(TAD.ReturnValue.hasValue());
    CArgs.push_back(*TAD.ReturnValue);
  }

  for (Value *Arg : TAD.Args.Args) {
    Expected<TcgV> MaybeMapped = Mapper.mapAndEmit(Arg);
    if (!MaybeMapped) {
      return mkError("failed mapping arg");
    }
    CArgs.push_back(MaybeMapped.get());
  }

  auto CArgIt = CArgs.begin();
  if (CArgIt != CArgs.end()) {
    HeaderWriter << tcg::getType(*CArgIt) << ' ' << tcg::getName(*CArgIt);
    ++CArgIt;
  }
  while (CArgIt != CArgs.end()) {
    HeaderWriter << ", " << tcg::getType(*CArgIt) << ' ' << tcg::getName(*CArgIt);
    ++CArgIt;
  }

  // Copy over function declaration from header to source file
  HeaderWriter << ')';
  Out << HeaderWriter.str();
  Out << " {\n";
  HeaderWriter << ';';

  ReversePostOrderTraversal<Function *> RPOT((Function *) F);
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
        if (!Res)
          return Res.takeError();
      } break;
      case Instruction::Br: {
        //
        // TODO(anjo): This is bad.
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
        if (I.hasMetadata("dead-branch")) {
          break;
        }

        auto Branch = cast<BranchInst>(&I);
        if (Branch->isConditional()) {
          assert(Branch->getNumSuccessors() == 2);
          Expected<TcgV> Condition = Mapper.mapCondAndEmit(Branch->getCondition(), 32, 1);
          if (!Condition)
            return mkError("couldn't map brcond condition ",
                           Branch->getCondition());
          const TcgV CCondition = tcg::materialize(Condition.get());
          const TcgV True = Mapper.mapBbAndEmit(Branch->getSuccessor(0));
          const TcgV False = Mapper.mapBbAndEmit(Branch->getSuccessor(1));

          // Jump if condition is != 0
          auto Zero = TcgV::makeImmediate("0", CCondition.TcgSize, 1);
          tcg::genBrcond(Out, "TCG_COND_NE", CCondition, Zero, True);
          tcg::genBr(Out, False);
        } else {
          const TcgV Label = Mapper.mapBbAndEmit(Branch->getSuccessor(0));
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
          auto ResLlvmWidth = SExt->getDestTy()->getIntegerBitWidth();
          auto IntStr = Twine("")
            .concat("(int")
            .concat(Twine(ResLlvmWidth))
            .concat("_t) ")
            .concat("(int")
            .concat(Twine(SrcVal.get().TcgSize))
            .concat("_t) ")
            .concat(tcg::getName(SrcVal.get()))
            .str();
          auto Tcg = TcgV::makeImmediate(IntStr, ResLlvmWidth, ResLlvmWidth);
          Mapper.mapExplicitly(&I, Tcg);
        } else {
            Expected<TcgV> Res = Mapper.mapAndEmit(&I);
            if (!Res) {
              return Res.takeError();
            }
            if (Res.get().LlvmSize < 32) {
              return mkError("sext to unsupported size: ", &I);
            }
            if (SrcVal.get().Kind == IrPtrToOffset) {
              return mkError("sext on vector type not supported: ", &I);
            }
            if (SrcVal.get().LlvmSize > 1 and SrcVal.get().LlvmSize < 32) {
              // TODO(anjo): Here we are using the fact that we support (16,64), (8,64).
              auto FuncStr = Twine("tcg_gen_ext").concat(std::to_string(SrcVal.get().LlvmSize)).concat("s_i").concat(std::to_string(Res.get().TcgSize)).str();
              auto ASrcVal = TcgSizeAdapter(Out, SrcVal.get());
              tcg::emitCallTcg(Out, FuncStr, {Res.get(), ASrcVal.get(Res.get().TcgSize)});
              ASrcVal.free();
            } else if (SrcVal.get().LlvmSize == 1 and Res.get().TcgSize == 32) {
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
          auto ResLlvmWidth = ZExt->getDestTy()->getIntegerBitWidth();
          if (ResLlvmWidth > 64)
            return mkError("128-bit integers not supported: ", &I);
          auto IntStr = Twine("(uint")
            .concat(Twine(ResLlvmWidth))
            .concat("_t) (uint")
            .concat(Twine(SrcVal.get().TcgSize))
            .concat("_t) ")
            .concat(tcg::getName(SrcVal.get()))
            .str();
          auto Tcg = TcgV::makeImmediate(IntStr, ResLlvmWidth, ResLlvmWidth);
          Mapper.mapExplicitly(&I, Tcg);
          break;
        }

        auto DestTy = dyn_cast<IntegerType>(ZExt->getDestTy());
        if (!DestTy) {
          return mkError("zext to invalid type: ", ZExt);
        }
        const uint32_t ResLlvmSize = DestTy->getIntegerBitWidth();
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
        } else if (SrcTcgSize > Res.get().TcgSize and SrcLlvmSize == 1) {
          // Paradoxically we may need to emit an extract instruction for
          // when a zero extension is requested. This is to account for the
          // fact that "booleans" in tcg can be both 64- and 32-bit. So for
          // instance zext i1 -> i32, here i1 may actually be 64-bit.
          tcg::genExtrlI64I32(Out, Res.get(), SrcVal.get());
        } else {
          tcg::genExtuI32I64(Out, Res.get(), SrcVal.get());
        }
      } break;
      case Instruction::Trunc: {
        auto Trunc = cast<TruncInst>(&I);

        Expected<TcgV> SrcVal = Mapper.mapAndEmit(Trunc->getOperand(0));
        if (!SrcVal) {
          return mkError("Couldn't map value ", Trunc->getOperand(0));
        }
        if (SrcVal.get().Kind == IrPtrToOffset) {
          return mkError("trunc of vector type not allowed: ", &I);
        }
        if (SrcVal.get().Kind == IrImmediate) {
          Mapper.mapExplicitly(&I, SrcVal.get());
          break;
        }

        Expected<TcgV> Res = Mapper.mapAndEmit(&I);
        if (!Res) {
          return Res.takeError();
        }
        // TODO(anjo): move to TcgSizeAdapter?
        if (SrcVal.get().TcgSize == 64) {
          if (Res.get().LlvmSize == 32) {
            // 64 -> 32
            tcg::genExtrlI64I32(Out, Res.get(), SrcVal.get());
          } else {
            // 64 -> 16,8,1
            auto MRes = Res.get();
            auto MSrc = SrcVal.get();
            auto Offset = TcgV::makeImmediate("0", MRes.TcgSize, MRes.LlvmSize);
            auto Size = TcgV::makeImmediate(Twine(MRes.LlvmSize).str(), MRes.TcgSize, MRes.LlvmSize);
            auto Temp = TcgV::makeTemp(64, 64, IrValue);
            tcg::defineNewTemp(Out, Temp);
            tcg::genExtract(Out, false, Temp, MSrc, Offset, Size);
            tcg::genExtrlI64I32(Out, MRes, Temp);
          }
        } else if (SrcVal.get().TcgSize == 32) {
          // 32 -> 16,8,1
          // 16 -> 8,1
          //  8 -> 1
          auto MRes = Res.get();
          auto MSrc = SrcVal.get();
          auto Offset = TcgV::makeImmediate("0", MRes.TcgSize, MRes.LlvmSize);
          auto Size = TcgV::makeImmediate(Twine(MRes.LlvmSize).str(), MRes.TcgSize, MRes.LlvmSize);
          tcg::genExtract(Out, false, MRes, MSrc, Offset, Size);
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
        if (Op1.Kind == IrImmediate and 
            Op2.Kind != IrImmediate and 
            Bin->isCommutative()) {
            std::swap(Op1, Op2);
        }

        if (isa<IntegerType>(Bin->getType())) {
          if (Op1.Kind == IrImmediate and Op2.Kind == IrImmediate) {
            Expected<TcgBinOp> Decl = Mapper.mapCBinOp(Bin->getOpcode(), Op1, Op2);
            if (!Decl)
              return Decl.takeError();

            uint32_t LargestLlvmSize = std::max(Op1.LlvmSize,
                                                Op2.LlvmSize);
            uint32_t LargestTcgSize = llvmToTcgSize(LargestLlvmSize);

            auto Tcg = TcgV::makeImmediate(Decl.get().Code, LargestTcgSize, LargestLlvmSize);
            Mapper.mapExplicitly(Bin, Tcg);
          } else {
            Expected<TcgV> Res = Mapper.mapAndEmit(Bin);
            if (!Res)
              return mkError("couldn't map binary op res", &I);

            // Adapt sizes to account for boolean values, with LlvmSize == 1
            // and TcgSize == 32 or 64.  Materialize first op. to deal with
            // non-commutative ops.
            TcgSizeAdapter AOp1(Out, tcg::materialize(Op1));
            TcgSizeAdapter AOp2(Out, Op2);

            const uint32_t ResSize = Res.get().TcgSize;
            Expected<TcgBinOp> Decl = Mapper.mapTempBinOp(Bin->getOpcode(),
                                                          AOp1.get(ResSize),
                                                          AOp2.get(ResSize));
            if (!Decl)
              return Decl.takeError();

            tcg::emitBinOp(Out,
                           Decl.get(),
                           { Res.get(), AOp1.get(ResSize), AOp2.get(ResSize) });
            AOp1.free();
            AOp2.free();
          }
        } else if (isa<VectorType>(Bin->getType())) {
          Expected<TcgV> Res = Mapper.mapAndEmit(Bin);
          if (!Res)
            return Res.takeError();
          assert(Res.get().Kind == IrPtrToOffset);

          Expected<TcgVecBinOp> Decl = Mapper.mapVecBinOp(Bin->getOpcode(),
                                                          Op1, Op2);
          if (!Decl)
              return Decl.takeError();

          if (Decl.get().RequiredOp2Size) {
              const uint32_t Size = *(Decl.get().RequiredOp2Size);
              TcgSizeAdapter AOp2(Out, Op2);
              tcg::genVecBinOp(Out, Decl.get().Code, Res.get(), Op1, AOp2.get(Size));
              AOp2.free();
          } else {
              tcg::genVecBinOp(Out, Decl.get().Code, Res.get(), Op1, Op2);
          }
        }
      } break;
      case Instruction::Call: {
        auto Call = cast<CallInst>(&I);
        Function *F = Call->getCalledFunction();
        if (!F)
          return mkError("Indirect function calls not handled: ", &I);
        assert(F->hasName());
        StringRef Name = F->getName();

        // These are the calls we currently no-op
        if (Name.startswith("llvm.dbg") or
            // Ignore llvm.dbg and assertion stuffs
            Name.startswith("llvm.lifetime") or
            Name == "__assert_fail" or
            Name == "g_assertion_message_expr") {
          break;
        }

        Optional<TcgV> Res = None;
        //
        // NOTE(anjo): We are checking that the result of the call
        // has > 0 uses, this is the only situation (that I'm aware of)
        // where LLVM will not remove an unused value.
        //
        auto MapReturnValue = [&Mapper, &Call]() -> Expected<TcgV> {
            if (!Call->getType()->isVoidTy() and Call->getNumUses() > 0) {
              Expected<TcgV> Mapped = Mapper.mapAndEmit(Call);
              if (!Mapped)
                return mkError("Failed to map result of call instruction", Call);
              return Mapped.get();
            }
            return mkError("Invalid return type", Call);
        };

        ArgVector Args;
        for (uint32_t i = 0; i < Call->arg_size(); ++i) {
          if (auto Bb = dyn_cast<BasicBlock>(Call->getArgOperand(i))) {
            Args.push_back(Mapper.mapBbAndEmit(Bb));
          } else {
            Expected<TcgV> Mapped = Mapper.mapAndEmit(Call->getArgOperand(i));
            if (!Mapped)
              return Mapped.takeError();
            Args.push_back(Mapped.get());
          }
        }

        // Function names sometimes contain embedded type information to handle
        // polymorphic arguments, for instance
        //
        //   llvm.memcpy.p0i8.p0i8.i64
        //
        // specifying the source and desination pointer types as i8* and the
        // size argument as an i64.
        //
        // Find the index for the first '.' before the types are specified
        //
        //   llvm.memcpy.p0i8.p0i8.i64
        //              ^- index of this '.'
        // TODO(anjo): Is there a llvm function for this
        size_t IndexBeforeTypes = StringRef::npos;
        for (size_t i = Name.size()-1; i > 0; --i) {
          const char c = Name[i];
          bool ValidType = (c >= '0' and c <= '9') or
                            c == 'i' or
                            c == 'p' or
                            c == 'a' or
                            c == 'v' or
                            c == 'x';
          if (c == '.') {
            IndexBeforeTypes = i;
          } else if (!ValidType) {
            break;
          }
        }
        StringRef StrippedName = Name.substr(0, IndexBeforeTypes);

        if (F->isIntrinsic()) {
          switch (F->getIntrinsicID()) {
          case Intrinsic::abs: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genAbs(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::sadd_sat: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genSignedSatAdd(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::ssub_sat: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genSignedSatSub(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::smax: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genSignedMax(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::smin: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genSignedMin(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::umax: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genUnsignedMax(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::umin: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genUnsignedMin(Out, *MaybeRes, Args[0], Args[1]);
          } break;
          case Intrinsic::ctlz: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            if (Args[0].Kind == IrPtrToOffset) {
              return mkError("no gvec equivalent to clzi");
            }
            tcg::genClzi(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::cttz: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            if (Args[0].Kind == IrPtrToOffset) {
              return mkError("no gvec equivalent to cttz");
            }
            tcg::genCtzi(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::ctpop: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            if (Args[0].Kind == IrPtrToOffset) {
              return mkError("no gvec equivalent to ctpop");
            }
            tcg::genCtpop(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::bswap: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genBswap(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::fshl: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genFunnelShl(Out, *MaybeRes, Args[0], Args[1], Args[2]);
          } break;
          case Intrinsic::bitreverse: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genBitreverse(Out, *MaybeRes, Args[0]);
          } break;
          case Intrinsic::memcpy: {
            tcg::genVecMemcpy(Out, Args[0], Args[1], Args[2]);
          } break;
          case Intrinsic::memset: {
            tcg::genVecMemset(Out, Args[0], Args[1], Args[2]);
          } break;
          default: return mkError("Unhandled LLVM intrinsic: ", Call);
          }
        } else if (auto Inst = getPseudoInstFromCall(Call)) {
          switch (Inst.get()) {
          case IdentityMap: {
            Mapper.mapExplicitly(Call, Args[0]);
          } break;
          case PtrAdd: {
            if (Args[0].Kind == IrPtr) {
              Expected<TcgV> MaybeRes = MapReturnValue();
              if (!MaybeRes)
                return MaybeRes.takeError();
              Res = *MaybeRes;
              if (Args[1].Kind == IrConst or Args[1].Kind == IrImmediate) {
                Out << "tcg_gen_addi_ptr(" << tcg::getName(*Res) << ", " << tcg::getName(Args[0]) << ", " << tcg::getName(Args[1]) << ");\n";
              } else {
                // TODO(anjo): Move to separate llvm ir opcode
                auto Ptr = TcgV::makeTemp(TcgTargetPtrSize, TcgTargetPtrSize, IrPtr);
                tcg::defineNewTemp(Out, Ptr);
                tcg::genTruncPtr(Out, Ptr, Args[1]);

                Out << "tcg_gen_add_ptr(" << tcg::getName(*Res) << ", " << tcg::getName(Args[0]) << ", " << tcg::getName(Ptr) << ");\n";
              }
            } else if (Args[0].Kind == IrImmediate or Args[0].Kind == IrConst) {
              if (Args[1].Kind == IrConst or Args[1].Kind == IrImmediate) {
                const auto ConstExpr = Twine()
                  .concat("(").concat(c::uintStr(Res->TcgSize)).concat(" *)")
                  .concat("((uintptr_t) ").concat(tcg::getName(Args[0]))
                  .concat(" + ")
                  .concat(tcg::getName(Args[1])).concat(")")
                  .str();
                auto Tcg = TcgV::makeImmediate(ConstExpr, Args[0].TcgSize, Args[0].LlvmSize);
                Mapper.mapExplicitly(Call, Tcg);
              } else {
                Expected<TcgV> MaybeRes = MapReturnValue();
                if (!MaybeRes)
                  return MaybeRes.takeError();
                Res = *MaybeRes;

                // TODO(anjo): Move to separate llvm ir opcode
                auto Ptr = TcgV::makeTemp(TcgTargetPtrSize, TcgTargetPtrSize, IrPtr);
                tcg::defineNewTemp(Out, Ptr);
                tcg::genTruncPtr(Out, Ptr, Args[1]);

                Out << "tcg_gen_addi_ptr(" << tcg::getName(*Res) << ", " << tcg::getName(Ptr) << ", " << tcg::getName(Args[0]) << ");\n";
              }
            } else if (Args[0].Kind == IrPtrToOffset) {
              if (Args[1].Kind == IrConst or Args[1].Kind == IrImmediate) {
                // TODO(anjo): This one is a problem as we currently have no way of expression IrPtrToOffset + ConstantExpression
                auto Tcg = TcgV(tcg::getName(Args[0]) + " + " + Args[1].Name, Args[0].TcgSize, Args[0].LlvmSize, Args[0].VectorElementCount, IrPtrToOffset);
                Tcg.ConstantExpression = true;
                errs() << "Mapping ptraddr:\n" << " " << *Call << "\n";
                Mapper.mapExplicitly(Call, Tcg);
              } else {
                return mkError("ptradd on vector types requires immediate offset: ", Call);
              }
            }
          } break;
          case AccessGlobalArray: {
            auto Offset = cast<ConstantInt>(Call->getArgOperand(0))->getZExtValue();
            auto It = OffsetMap.find(Offset);
            assert(It != OffsetMap.end());
            TcgGlobal Global = It->second;
            uint32_t LlvmSize = Global.Size;
            uint32_t TcgSize = llvmToTcgSize(LlvmSize);
            if (Args[1].Kind != IrImmediate)
              return mkError("globalArray access with non-immediate index: ", Call);
            auto Code = Global.Code.str() + "[" + Args[1].Name + "]";
            auto Tcg = TcgV::makeConstantExpression(Code, TcgSize, LlvmSize, IrValue);
            Mapper.mapExplicitly(Call, Tcg);
          } break;
          case AccessGlobalValue: {
            auto Offset = cast<ConstantInt>(Call->getArgOperand(0))->getZExtValue();
            auto It = OffsetMap.find(Offset);
            assert(It != OffsetMap.end());
            TcgGlobal Global = It->second;
            auto LlvmSize = Global.Size;
            auto TcgSize = llvmToTcgSize(LlvmSize);
            auto Tcg = TcgV::makeConstantExpression(Global.Code.str(), TcgSize, LlvmSize, IrValue);
            Mapper.mapExplicitly(Call, Tcg);
          } break;
          case Brcond: {
            auto LlvmPred = static_cast<ICmpInst::Predicate>(cast<ConstantInt>(Call->getOperand(0))->getZExtValue());
            StringRef TcgPred = mapPredicate(LlvmPred);
            tcg::genBrcond(Out, TcgPred, Args[1], Args[2], Args[3]);
            if (!Call->hasMetadata("fallthrough")) {
              tcg::genBr(Out, Args[4]);
            }
          } break;
          case Movcond: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes)
                return MaybeRes.takeError();
            Res = *MaybeRes;
            auto LlvmPred = static_cast<ICmpInst::Predicate>(cast<ConstantInt>(Call->getOperand(0))->getZExtValue());
            StringRef TcgPred = mapPredicate(LlvmPred);
            if (CmpInst::isSigned(LlvmPred)) {
              ensureSignBitIsSet(Out, Args[1]);
              ensureSignBitIsSet(Out, Args[2]);
            }
            tcg::genMovcond(Out, TcgPred, *Res, Args[1], Args[2], Args[3], Args[4]);
          } break;
          case VecSplat: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecSplat(Out, *MaybeRes, Args[0]);
          } break;
          case VecNot: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecNot(Out, *MaybeRes, Args[0]);
          } break;
          case VecAddScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "add", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecSubScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "sub", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecMulScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "mul", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecXorScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "xor", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecOrScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "or", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecAndScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "and", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecShlScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "shl", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecLShrScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "shr", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecAShrScalar: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            tcg::genVecBinOp(Out, "sar", *MaybeRes, Args[0], Args[1]);
          } break;
          case VecNotStore: {
            tcg::genVecNot(Out, Args[0], Args[1]);
          } break;
          case VecAddStore: {
            tcg::genVecBinOp(Out, "add", Args[0], Args[1], Args[2]);
          } break;
          case VecSubStore: {
            tcg::genVecBinOp(Out, "sub", Args[0], Args[1], Args[2]);
          } break;
          case VecMulStore: {
            tcg::genVecBinOp(Out, "mul", Args[0], Args[1], Args[2]);
          } break;
          case VecXorStore: {
            tcg::genVecBinOp(Out, "xor", Args[0], Args[1], Args[2]);
          } break;
          case VecOrStore: {
            tcg::genVecBinOp(Out, "or", Args[0], Args[1], Args[2]);
          } break;
          case VecAndStore: {
            tcg::genVecBinOp(Out, "and", Args[0], Args[1], Args[2]);
          } break;
          case VecShlStore: {
            tcg::genVecBinOp(Out, "shl", Args[0], Args[1], Args[2]);
          } break;
          case VecLShrStore: {
            tcg::genVecBinOp(Out, "shr", Args[0], Args[1], Args[2]);
          } break;
          case VecAShrStore: {
            tcg::genVecBinOp(Out, "sar", Args[0], Args[1], Args[2]);
          } break;
          case VecAddScalarStore: {
            tcg::genVecBinOp(Out, "add", Args[0], Args[1], Args[2]);
          } break;
          case VecSubScalarStore: {
            tcg::genVecBinOp(Out, "sub", Args[0], Args[1], Args[2]);
          } break;
          case VecMulScalarStore: {
            tcg::genVecBinOp(Out, "mul", Args[0], Args[1], Args[2]);
          } break;
          case VecXorScalarStore: {
            tcg::genVecBinOp(Out, "xor", Args[0], Args[1], Args[2]);
          } break;
          case VecOrScalarStore: {
            tcg::genVecBinOp(Out, "or", Args[0], Args[1], Args[2]);
          } break;
          case VecAndScalarStore: {
            tcg::genVecBinOp(Out, "and", Args[0], Args[1], Args[2]);
          } break;
          case VecShlScalarStore: {
            tcg::genVecBinOp(Out, "shl", Args[0], Args[1], Args[2]);
          } break;
          case VecLShrScalarStore: {
            tcg::genVecBinOp(Out, "shr", Args[0], Args[1], Args[2]);
          } break;
          case VecAShrScalarStore: {
            tcg::genVecBinOp(Out, "sar", Args[0], Args[1], Args[2]);
          } break;
          case VecSignedSatAddStore: {
            tcg::genSignedSatAdd(Out, Args[0], Args[1], Args[2]);
          } break;
          case VecSignedSatSubStore: {
            tcg::genSignedSatSub(Out, Args[0], Args[1], Args[2]);
          } break;
          case VecSelectStore: {
            Out << "tcg_gen_gvec_bitsel("
              << "MO_" << "8" << ", "
              << tcg::getName(Args[0]) << ", "
              << tcg::getName(Args[1]) << ", "
              << tcg::getName(Args[2]) << ", "
              << tcg::getName(Args[3]) << ", "
              << "128" << ", "
              << "128" << ");\n";
          } break;
          //case VecFunnelShrStore: {
          //} break;
          case VecAbsStore: {
            tcg::genAbs(Out, Args[0], Args[1]);
          } break;
          case VecSignedMaxStore: {
            tcg::genSignedMax(Out, Args[0], Args[1], Args[2]);
          } break;
          case VecUnsignedMaxStore: {
            tcg::genUnsignedMax(Out, Args[0], Args[1], Args[2]);
          } break;
          case VecSignedMinStore: {
            tcg::genSignedMin(Out, Args[0], Args[1], Args[2]);
          } break;
          case VecUnsignedMinStore: {
            tcg::genUnsignedMin(Out, Args[0], Args[1], Args[2]);
          } break;
          //case VecCtlzStore: {
          //} break;
          //case VecCttzStore: {
          //} break;
          //case VecCtpopStore: {
          //} break;
          case HostLoad: {
            Expected<TcgV> MaybeRes = MapReturnValue();
            if (!MaybeRes) {
              return MaybeRes.takeError();
            }
            uint8_t Sign = cast<ConstantInt>(Call->getOperand(1))->getZExtValue();
            uint8_t Size = cast<ConstantInt>(Call->getOperand(2))->getZExtValue();
            uint8_t Endianness = cast<ConstantInt>(Call->getOperand(3))->getZExtValue();
            std::string MemOpStr = "MO_";
            raw_string_ostream MemOpStream(MemOpStr);
            switch (Endianness) {
            case 0: break; // do nothing
            case 1: MemOpStream << "LE"; break;
            case 2: MemOpStream << "BE"; break;
            default: abort();
            }
            switch (Sign) {
            case 0: MemOpStream << "U"; break;
            case 1: MemOpStream << "S"; break;
            default: abort();
            }
            switch (Size) {
            case 1: MemOpStream << "B"; break;
            case 2: MemOpStream << "W"; break;
            case 4: MemOpStream << "L"; break;
            case 8: MemOpStream << "Q"; break;
            default: abort();
            }
            tcg::genQemuLoad(Out, *MaybeRes, Args[0], MemOpStream.str().c_str());
          } break;
          case HostStore: {
            uint8_t Size = cast<ConstantInt>(Call->getOperand(2))->getZExtValue();
            uint8_t Endianness = cast<ConstantInt>(Call->getOperand(3))->getZExtValue();
            std::string MemOpStr = "MO_";
            raw_string_ostream MemOpStream(MemOpStr);
            switch (Endianness) {
            case 0: break; // do nothing
            case 1: MemOpStream << "LE"; break;
            case 2: MemOpStream << "BE"; break;
            default: abort();
            }
            // Always unsigned for stores
            MemOpStream << "U";
            switch (Size) {
            case 1: MemOpStream << "B"; break;
            case 2: MemOpStream << "W"; break;
            case 4: MemOpStream << "L"; break;
            case 8: MemOpStream << "Q"; break;
            default: abort();
            }
            tcg::genQemuStore(Out, Args[0], Args[1], MemOpStream.str().c_str());
          } break;
          default:
              return mkError(Twine("Unmapped pseudo instruction ").concat(pseudoInstName(Inst.get())).str());
          }
        }

        else if (StrippedName == "cpu_ldub_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_UB");
        } else if (StrippedName == "cpu_lduw_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_LEUW");
        } else if (StrippedName == "cpu_lduw_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_BEUW");
        } else if (StrippedName == "cpu_ldl_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_LESL");
        } else if (StrippedName == "cpu_ldl_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_BESL");
        } else if (StrippedName == "cpu_ldq_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_LESQ");
        } else if (StrippedName == "cpu_ldq_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuLoad(Out, *MaybeRes, Args[1], "MO_BESQ");
        }

        else if (StrippedName == "cpu_stb_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_UB");
        } else if (StrippedName == "cpu_stw_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_LEUW");
        } else if (StrippedName == "cpu_stw_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_BEUW");
        } else if (StrippedName == "cpu_stl_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_LEUL");
        } else if (StrippedName == "cpu_stl_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_BEUL");
        } else if (StrippedName == "cpu_stq_le_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_LEUQ");
        } else if (StrippedName == "cpu_stq_be_data_ra") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genQemuStore(Out, *MaybeRes, Args[1], "MO_BEUQ");
        }

        else if (StrippedName == "extract32") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genExtract(Out, false, *MaybeRes, Args[0], Args[1], Args[2]);
        } else if (StrippedName == "extract64") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genExtract(Out, false, *MaybeRes, Args[0], Args[1], Args[2]);
        } else if (StrippedName == "sextract32") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genExtract(Out, true, *MaybeRes, Args[0], Args[1], Args[2]);
        } else if (StrippedName == "sextract64") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genExtract(Out, true, *MaybeRes, Args[0], Args[1], Args[2]);
        } else if (StrippedName == "deposit32") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genDeposit(Out, *MaybeRes, Args[0], Args[1], Args[2], Args[3]);
        } else if (StrippedName == "deposit64") {
          Expected<TcgV> MaybeRes = MapReturnValue();
          if (!MaybeRes) {
            return MaybeRes.takeError();
          }
          tcg::genDeposit(Out, *MaybeRes, Args[0], Args[1], Args[2], Args[3]);
        }

        else if (Name.startswith("helper")) {
          // Map and adapt arguments to the call
          SmallVector<TcgV, 8> IArgs;
          for (auto Arg : Args) {
            IArgs.push_back(tcg::materialize(Arg));
          }
          tcg::genCallHelper(Out, Name, IArgs.begin(), IArgs.end());
        }

        else {

          if (F->isDeclaration()) {
            return mkError("call to declaration: ", Call);
          }

          if (!HasTranslatedFunction.contains(F)) {
            return mkError("call to function which failed to translate: ", Call);
          }

          // Map and adapt arguments to the call

          Expected<TcgV> MaybeRes = MapReturnValue();

          StringRef Name = F->getName();
          Name.consume_front("helper_");
          Out << "emit_" << Name << "(";

          if (MaybeRes) {
            Out << tcg::getName(MaybeRes.get());
            if (!Args.empty()) {
              Out << ", ";
            }
          }

          for (int i = 0; i < Args.size(); ++i) {
            Out << tcg::getName(tcg::materialize(Args[i]));
            if (i < Args.size()-1) {
              Out << ", ";
            }
          }
          Out << ");\n";
        }

      } break;
      case Instruction::ICmp: {
        auto ICmp = cast<ICmpInst>(&I);
        Expected<TcgV> Op1 = Mapper.mapAndEmit(I.getOperand(0));
        if (!Op1) {
          return mkError("Couldn't map first op: ", ICmp);
        }
        Expected<TcgV> Op2 = Mapper.mapAndEmit(I.getOperand(1));
        if (!Op2) {
          return mkError("Couldn't map first op: ", ICmp);
        }
        // If both operands are immediates (constant expressions, we can perform the operation
        // as a constant expression.
        if (Op1.get().Kind == IrImmediate and Op2.get().Kind == IrImmediate) {
          auto Pred = mapCPredicate(ICmp->getPredicate());
          const Twine Decl = "(" + tcg::getName(Op1.get()) + " " + Pred + " " + tcg::getName(Op2.get()) + ")";
          const std::string Str = Decl.str();

          auto Tcg = TcgV::makeImmediate(Str, Op1.get().TcgSize, 1);
          Mapper.mapExplicitly(ICmp, Tcg);
          break;
        }

        auto Pred = mapPredicate(ICmp->getPredicate());

        if (Op1.get().Kind == IrPtrToOffset) {
          Expected<TcgV> Res = Mapper.mapCondAndEmit(&I, Op1.get().TcgSize, Op1.get().LlvmSize);
          if (!Res) {
            return mkError("couldn't map icmp result", &I);
          }
          // TODO(anjo):
          //   - res type is actually 128 x i1, we want 128 x i8 to match arguments
          auto VectorSize = Res.get().LlvmSize * Res.get().VectorElementCount /  8;
          Out << "tcg_gen_gvec_cmp("
            << Pred << ", "
            << "MO_" << Res.get().LlvmSize << ", "
            << tcg::getName(Res.get()) << ", "
            << tcg::getName(Op1.get()) << ", "
            << tcg::getName(Op2.get()) << ", "
            << VectorSize << ", "
            << VectorSize << ");\n";
        } else {
          Expected<TcgV> Res = Mapper.mapCondAndEmit(&I, Op1.get().TcgSize, 1);
          if (!Res) {
            return mkError("couldn't map icmp result", &I);
          }
          auto IOp1 = tcg::materialize(Op1.get());
          if (ICmp->isSigned()) {
            ensureSignBitIsSet(Out, IOp1);
            ensureSignBitIsSet(Out, Op2.get());
          }
          if (Op2.get().Kind == IrImmediate) {
            tcg::genSetcondI(Out, Pred, Res.get(), IOp1, Op2.get());
          } else {
            tcg::genSetcond(Out, Pred, Res.get(), IOp1, Op2.get());
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
          return mkError("Select statements for pointer types not supported: ", Select);
        }
        Expected<TcgV> Cond = Mapper.mapAndEmit(Select->getCondition());
        if (!Cond) {
          return mkError("Error mapping select cond");
        }
        Expected<TcgV> True = Mapper.mapAndEmit(Select->getTrueValue());
        if (!True) {
          return mkError("Couldn't map True for select instruction: ", Select);
        }
        Expected<TcgV> False = Mapper.mapAndEmit(Select->getFalseValue());
        if (!False) {
          return mkError("Couldn't map True for select instruction: ", Select);
        }

        if (Res.get().Kind == IrPtrToOffset) {
          // TODO(anjo): handle size
          // TODO(anjo): move to Emit.cpp
          Out << "tcg_gen_gvec_bitsel("
            << "MO_" << "8" << ", "
            << tcg::getName(Res.get()) << ", "
            << tcg::getName(Cond.get()) << ", "
            << tcg::getName(True.get()) << ", "
            << tcg::getName(False.get()) << ", "
            << "128" << ", "
            << "128" << ");\n";
        } else if (Cond.get().Kind == IrImmediate) {
          assert(Res.get().Kind != IrImmediate);
          auto CTrue = tcg::materialize(True.get());
          auto CFalse = tcg::materialize(False.get());
          // TODO(anjo): Move to TcgEmit.cpp
          // TODO(anjo): c::ternary(cond, true, false) ?
          Out << "tcg_gen_mov_i" << Res.get().TcgSize << "(" << tcg::getName(Res.get()) << ", "
            << tcg::getName(Cond.get()) << " ? " << tcg::getName(CTrue) << " : " << tcg::getName(CFalse)
            << ");\n";
        } else {
          TcgV Zero = TcgV::makeImmediate("0", Res.get().TcgSize, 1);
          auto ACond = TcgSizeAdapter(Out, Cond.get());
          auto ATrue = TcgSizeAdapter(Out, True.get());
          auto AFalse = TcgSizeAdapter(Out, False.get());
          if (True.get().Kind == IrImmediate or False.get().Kind == IrImmediate) {
            auto CTrue = tcg::materialize(ATrue.get(Res.get().TcgSize));
            auto CFalse = tcg::materialize(AFalse.get(Res.get().TcgSize));

            tcg::genMovcond(Out,
                            "TCG_COND_NE",
                            Res.get(),
                            ACond.get(CTrue.TcgSize),
                            Zero,
                            CTrue,
                            CFalse);
          } else {
            tcg::genMovcond(Out,
                            "TCG_COND_NE",
                            Res.get(),
                            ACond.get(True.get().TcgSize),
                            Zero,
                            ATrue.get(Res.get().TcgSize),
                            AFalse.get(Res.get().TcgSize));
          }
          ACond.free();
          ATrue.free();
          AFalse.free();
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
        // TODO(anjo): The register allocation pass does not take immediates into account,
        // so we will end up with an immediate TcgV for which TAD.SkipReturnMov == true,
        // and so we won't emit anything for
        //      __attribute__((annotate("llvm-to-tcg")))
        //      __attribute__((annotate("immediate: 0, 1")))
        //      int test(int a, int b) {
        //           return a + b;
        //      }
        if (Tcg.get().Kind == IrImmediate) {
          tcg::genMovI(Out, *TAD.ReturnValue, Tcg.get());
        } else if (!TAD.SkipReturnMov) {
          tcg::genMov(Out, *TAD.ReturnValue, Tcg.get());
        }
      } break;
      case Instruction::BitCast: {
        // TODO(anjo): We need to clean up bitcasts, they are
        // very hacky atm. Maybe we should have alloca's actually
        // return a pointer, and use bitcasts to "update" the type.

        // We currently identity-map `BitCast`s
        //
        // If the bitcast has a larger lifetime than the source
        // variable, we need to allocate a new variable so we
        // don't accidentally free too soon.
        // TODO(anjo): Move to pre-backend pass? And keep only
        // relevant bitcasts
        auto Bitcast = cast<BitCastInst>(&I);
        Expected<TcgV> SrcVal = Mapper.mapAndEmit(Bitcast->getOperand(0));
        if (!SrcVal) {
          return SrcVal.takeError();
        }
        if (SrcVal.get().Kind == IrPtrToOffset) {
          auto PtrTy = dyn_cast<PointerType>(Bitcast->getType());
          if (!PtrTy)
            return mkError("bitcast to non-pointer type: ", Bitcast);
          auto VecTy = dyn_cast<VectorType>(PtrTy->getPointerElementType());
          if (!VecTy)
            return mkError("bitcast to unsuppored type: ", Bitcast);
          auto IntTy = cast<IntegerType>(VecTy->getElementType());
          auto LlvmSize = IntTy->getBitWidth();
          auto VectorElements = getVectorElementCount(VecTy);
          auto VectorSize = LlvmSize*VectorElements;
          auto TcgVectorSize = (VectorSize <= 32) ? 32 : 64;
          TcgV Tcg = SrcVal.get();
          Tcg.TcgSize = TcgVectorSize;
          Tcg.LlvmSize = LlvmSize;
          Tcg.VectorElementCount = VectorElements;
          Tcg.Kind = IrPtrToOffset;
          Mapper.mapExplicitly(Bitcast, Tcg);
        } else {
          auto BitcastTy = Bitcast->getType();
          if (auto PtrTy = dyn_cast<PointerType>(BitcastTy)) {
            if (auto IntTy = dyn_cast<IntegerType>(PtrTy->getPointerElementType())) {
              const uint32_t TcgSize = llvmToTcgSize(IntTy->getBitWidth());
              if (TcgSize == SrcVal.get().TcgSize)
                Mapper.mapExplicitly(Bitcast, SrcVal.get());
              else
                return mkError("Invalid bitcast changes tcg size: ", &I);
            } else if (isa<ArrayType>(PtrTy->getPointerElementType())) {
              return mkError("Bitcast to unsupported type: ", &I);
            } else {
              Mapper.mapExplicitly(Bitcast, SrcVal.get());
            }
          } else if (auto VecTy = dyn_cast<VectorType>(BitcastTy)) {
            // TODO(anjo): When does a bare cast like this happen?
            auto IntTy = cast<IntegerType>(VecTy->getElementType());
            auto LlvmSize = IntTy->getBitWidth();
            auto VectorElements = getVectorElementCount(VecTy);
            auto VectorSize = LlvmSize * VectorElements;
            auto TcgVectorSize = (VectorSize <= 32) ? 32 : 64;
            TcgV Tcg = SrcVal.get();
            Tcg.TcgSize = TcgVectorSize;
            Tcg.LlvmSize = LlvmSize;
            Tcg.VectorElementCount = VectorElements;
            Tcg.Kind = IrPtrToOffset;
            Mapper.mapExplicitly(Bitcast, Tcg);
          } else {
            return mkError("Unhandled bitcast type: ", Bitcast);
          }
        }
      } break;
      case Instruction::Load: {
        auto Load = cast<LoadInst>(&I);
        auto LlvmPtr = Load->getPointerOperand();

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
          auto Ptr = Mapped.get();
          auto Val = TcgV::makeImmediate(Twine("*").concat(tcg::getName(Ptr)).str(), Res.get().TcgSize, Res.get().LlvmSize);
          tcg::genMovI(Out, Res.get(), Val);
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
          // artifact of IrPtrToOffset arguments being pointers. Stores
          // to results are instead taken care of by whatever instruction
          // generated the result.
          // TODO(anjo): Move to @id?
          if (isa<VectorType>(Load->getType())) {
            Mapper.mapExplicitly(Load, Mapped.get());
            break;
          }
        } break;
        default:
          return mkError("Load from unsupported TcgV type");
        };

      } break;
      case Instruction::Store: {
        auto Store = cast<StoreInst>(&I);
        Expected<TcgV> Val = Mapper.mapAndEmit(Store->getValueOperand());
        if (!Val) {
          return Val.takeError();
        }
        auto LlvmPtr = Store->getPointerOperand();
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
          tcg::genSt(Out, Mapped.get(), tcg::materialize(Val.get()), 0);
        } else if (Mapped.get().Kind == IrPtrToOffset) {
          // Stores to IrPtrToOffset are ignored, they are an artifact of
          // IrPtrToOffset arguments being pointers. Stores to results are
          // instead taken care of by whatever instruction generated the
          // result.
        } else {
          return mkError("Store to unsupported TcgV kind: ", Store);
        }
      } break;
      case Instruction::Unreachable: {
        Out << "/* unreachable */\n";
        // TODO(anjo): Is there any way to crash at runtime here?
        // I think most things such as division by zero and double free
        // are caught at translation time.
      } break;
      case Instruction::Switch: {
        auto Switch = cast<SwitchInst>(&I);
        // Operands to switch instructions alternate between
        // case values and the corresponding label:
        //   Operands: { Cond, DefaultLabel, Case0, Label0, Case1, Label1, ... }
        Expected<TcgV> Val = Mapper.mapAndEmit(Switch->getOperand(0));
        if (!Val) {
          return Val.takeError();
        }
        const TcgV DefaultLabel = Mapper.mapBbAndEmit(cast<BasicBlock>(Switch->getOperand(1)));
        for (uint32_t i = 2; i < Switch->getNumOperands(); i += 2) {
          Expected<TcgV> BranchVal = Mapper.mapAndEmit(Switch->getOperand(i));
          if (!BranchVal) {
            return BranchVal.takeError();
          }
          const TcgV BranchLabel = Mapper.mapBbAndEmit(cast<BasicBlock>(Switch->getOperand(i+1)));
          tcg::genBrcond(Out, "TCG_COND_EQ", Val.get(), BranchVal.get(), BranchLabel);
        }
        tcg::genBr(Out, DefaultLabel);
      } break;
      default: {
        return mkError("Instruction not yet implemented", &I);
      }
      }
    }
  }

  Out << "}\n";

  return TF;
}

#include "llvm/Analysis/CallGraph.h"
#include "llvm/ADT/DepthFirstIterator.h"

PreservedAnalyses TcgGenPass::run(Module &M, ModuleAnalysisManager &MAM) {
  auto &FAM = MAM.getResult<FunctionAnalysisManagerModuleProxy>(M).getManager();
  const auto &OffsetMap = MAM.getResult<TcgGlobalMapPass>(M);
  const auto &AnnotationMap = MAM.getResult<MapAnnotationsPass>(M).Map;
  auto &CG = MAM.getResult<CallGraphAnalysis>(M);

  // Preamble
  OutSource << "#include \"qemu/osdep.h\"\n";
  OutSource << "#include \"qemu/log.h\"\n";
  OutSource << "#include \"cpu.h\"\n";
  OutSource << "#include \"tcg/tcg-op.h\"\n";
  OutSource << "#include \"tcg/tcg-op-gvec.h\"\n";
  OutSource << "#include \"tcg/tcg.h\"\n";
  OutSource << "#include \"exec/exec-all.h\"\n";
  OutSource << "#include \"exec/helper-gen.h\"\n";
  OutSource << '\n';

  OutSource << "#include \"helper2tcg/tcg_safe.h\"\n";
  OutSource << "#include \"helper2tcg/tcg_global_mappings.h\"\n";
  OutSource << "#include \"helper2tcg/tcg-ext.h\"\n";
  OutSource << '\n';

  OutSource << "#include \"" << HeaderPath << "\"\n";
  OutSource << '\n';

  // Emit extern definitions for all global TCGv_* that are mapped
  // to the CPUState.
  for (const auto &[_, Global] : OffsetMap) {
    const uint32_t Size = llvmToTcgSize(Global.Size);
    OutSource << "extern " << "TCGv_i" << Size << " " << Global.Code;
    if (Global.NumElements > 1)
      OutSource << "[" << Global.NumElements << "]";
    OutSource << ";\n";
  }

  // Attempt to translate all functions and collect external
  // symbols, nothing is emitted yet.
  std::vector<TranslatedFunction> TranslatedFunctions;
  SmallPtrSet<Function *, 16> HasTranslatedFunction;
  std::set<StringRef> ExternalSymbols;
  for (Function &F : M) {
    if (F.isDeclaration()) {
      continue;
    }

    tcg::resetNameIndices();

    // Depth first traversal of call graph, we know there are
    // not loops.
    //errs() << "NODE--------------\n";
    //errs() << " | " << F.getName() << "\n";
    CallGraphNode *Node = CG[&F];
    for (auto It = po_begin(Node), End = po_end(Node); It != End; ++It) {
      Function *F = It->getFunction();
      if (F and F->hasName() and !F->isDeclaration() and !HasTranslatedFunction.contains(F)) {
        //errs() << "  " << It->getFunction()->getName() << "\n";
        auto &TAD = FAM.getResult<TcgTempAllocationPass>(*F);
        Expected<TranslatedFunction> Translated = translateFunction(F, OffsetMap, AnnotationMap, TAD, HasTranslatedFunction);
        if (Translated) {
          TranslatedFunctions.push_back(*Translated);
          ExternalSymbols.merge(Translated->ExternalSymbols);
          OutLog << F->getName() << ": OK\n";
          HasTranslatedFunction.insert(F);
        } else {
          OutLog << F->getName() << ": " << Translated.takeError() << "\n";
        }
      }
    }
    //errs() << "--------------\n";

    //auto &TAD = FAM.getResult<TcgTempAllocationPass>(F);
    //Expected<TranslatedFunction> Translated = translateFunction(&F, OffsetMap, AnnotationMap, TAD);
    //if (Translated) {
    //  TranslatedFunctions.push_back(*Translated);
    //  ExternalSymbols.merge(Translated->ExternalSymbols);
    //  OutLog << F.getName() << ": OK\n";
    //  HasTranslatedFunction.insert(&F);
    //} else {
    //  OutLog << F.getName() << ": " << Translated.takeError() << "\n";
    //}
  }

  // Emit all external symbols as "extern char ...", so that
  // taking the address of these makes sense.
  for (StringRef S : ExternalSymbols) {
    OutSource << "extern char " << S << ";\n";
  }

  // Emit translated functions
  for (auto &TF : TranslatedFunctions) {
    OutSource << TF.Code << '\n';
    OutHeader << TF.Decl << '\n';
    OutEnabled << TF.Name << '\n';
  }

  return PreservedAnalyses::all();
}
