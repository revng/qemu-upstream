#include "TcgTempAllocationPass.h"
#include "MapAnnotationsPass.h"
#include "llvm-compat.h"
#include "PseudoInst.h"

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <utils/Common.h>

using namespace llvm;

// TODO(anjo): move these to a common place
cl::opt<uint32_t> TcgTargetPtrSize("tcg-target-ptr-size",
                                   cl::desc("<TCG pointer size>"),
                                   cl::init(32));

// Type to represent a list of free TcgV's that can be reused when
// we need a new temporary. Exists for the duration of a function,
// and is expected to be small <= 8 free TcgV's at any time.
//
// This justifies the type being an array, since iteration times to
// find a free element will be small.
using FreeListVector = SmallVector<TcgV, 8>;

// Finds the first TcgV in FreeList with a matching TcgSize and Kind 
// Iterates over the FreeList to find a TcgV with matching TcgSize and Kind
static Optional<TcgV> findFreeTcgV(FreeListVector &FreeList,
                                   uint32_t TcgSize,
                                   TcgVKind Kind) {
  for (size_t i = 0; i < FreeList.size(); ++i) {
    if (FreeList[i].TcgSize == TcgSize and FreeList[i].Kind == Kind) {
      TcgV Tcg = FreeList[i];
      // Swap-remove
      FreeList[i] = FreeList.back();
      FreeList.pop_back();
      return Tcg;
    }
  }
  return None;
}

//
// Functions for mapping an LLVM Value to a TcgV
//

inline std::string constantIntToStr(Constant *C) {
  SmallString<20> ResultStr;
  auto *Int = cast<ConstantInt>(C);
  const APInt Value = Int->getUniqueInteger();
  const char *SuffixStr = "";
  const bool Negative = Int->isNegative();
  if (Value.ugt(UINT32_MAX)) {
    SuffixStr = Int->isNegative() ? "ll" : "ull";
  }
  if (Int->getBitWidth() == 1) {
    ResultStr = (Value.getBoolValue()) ? "true" : "false";
  } else {
    bool IsMax = (Negative) ? Value.isMaxSignedValue() : Value.isMaxValue();
    bool IsMin = Negative and Value.isMinSignedValue();
    unsigned Bitwidth = Value.getBitWidth();
    if (IsMax) {
      return Twine("INT").concat(Twine(Bitwidth)).concat("_MAX").str();
    } else if (IsMin) {
      return Twine("INT").concat(Twine(Bitwidth)).concat("_MIN").str();
    } else {
      Value.toString(ResultStr, 10, Value.isNegative(), true);
    }
  }
  return Twine(ResultStr).concat(SuffixStr).str();
}

static Expected<TcgV> mapInteger(TempAllocationData &TAD, 
                                 FreeListVector &FreeList,
                                 Value *V) {
  auto *Ty = cast<IntegerType>(V->getType());

  uint32_t LlvmSize = Ty->getBitWidth();
  if (LlvmSize > 64) {
    return mkError("Bit widths > 64 are not supported: ", V);
  }

  if (auto *Const = dyn_cast<ConstantInt>(V)) {
    // Constant integer
    auto Tcg = TcgV::makeImmediate(constantIntToStr(Const),
                                   llvmToTcgSize(LlvmSize),
                                   LlvmSize);
    return TAD.Map.try_emplace(V, Tcg).first->second;
  } else if (isa<Argument>(V)) {
    // Argument
    uint32_t TcgSize = llvmToTcgSize(LlvmSize);
    if (auto ArgInfoIt = TAD.Args.ArgInfoMap.find(V);
        ArgInfoIt != TAD.Args.ArgInfoMap.end() and
        ArgInfoIt->second == ArgImmediate) {
      auto Tcg = TcgV::makeImmediate(tcg::mkName("i"),
                                           TcgSize,
                                           LlvmSize);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    } else {
      auto Tcg = TcgV::makeTemp(TcgSize, LlvmSize, IrValue);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    }
  } else {
    // Non-constant integer
    uint32_t TcgSize = 0;
    if (auto ICmp = dyn_cast<ICmpInst>(V)) {
      // icmp's return i1's and are used as either 32-bit or 64-bit TCGv in
      // QEMU.  Assume the TcgSize from operands.
      assert(LlvmSize == 1);
      auto *IntTy0 = dyn_cast<IntegerType>(ICmp->getOperand(0)->getType());
      if (!IntTy0) {
        return mkError("Icmp on non-integer type");
      }
      TcgSize = llvmToTcgSize(IntTy0->getBitWidth());
    } else {
      // Normal integer, get the TcgSize from the LlvmSize as for constants
      TcgSize = llvmToTcgSize(LlvmSize);
    }

    Optional<TcgV> Tcg = findFreeTcgV(FreeList, TcgSize, IrValue);
    if (Tcg) {
      // Found a TcgV of the corresponding TcgSize, update LlvmSize
      Tcg->LlvmSize = LlvmSize;
      return TAD.Map.try_emplace(V, *Tcg).first->second;
    } else {
      // Otherwise, create a new value
      const auto Tcg = TcgV::makeTemp(TcgSize, LlvmSize, IrValue);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    }
  }
}

static Expected<TcgV> mapPointer(TempAllocationData &TAD, 
                                 FreeListVector &FreeList,
                                 Value *V) {
  auto *Ty = cast<PointerType>(V->getType());
  if (isa<GlobalValue>(V) and V->hasName()) {
    // If the pointer is to a named global `named_global`, add it as an
    // external symbol of the function, so we can emit
    //
    //   extern char named_global;
    //
    // in the backend. Here we map it to an immediate with
    // value `&named_global`.
    //
    // This gets the byte-aligned address of the named global.
    const StringRef Name = V->getName();
    TAD.ExternalSymbols.insert(Name);
    auto Tcg = TcgV::makeImmediate(Twine("&").concat(Name).str(),
                                   TcgTargetPtrSize,
                                   TcgTargetPtrSize);
    return TAD.Map.try_emplace(V, Tcg).first->second;
  } else if (isa<Argument>(V)) {
    if (auto ArgInfoIt = TAD.Args.ArgInfoMap.find(V);
        ArgInfoIt != TAD.Args.ArgInfoMap.end() and
        ArgInfoIt->second == ArgPtrToOffset) {
      const auto Tcg = TcgV::makeVector(TcgTargetPtrSize, TcgTargetPtrSize, 1);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    } else {
      auto IsEnv = (TAD.Args.EnvPtr.hasValue() && *TAD.Args.EnvPtr == V);
      const auto Tcg = TcgV::makeTemp(TcgTargetPtrSize, TcgTargetPtrSize,
                                      IsEnv ? IrEnv : IrPtr);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    }
  } else if (isa<AllocaInst>(V)) {
    // `alloca`s represent stack variables in LLVM IR and return
    // pointers, we can simply map them to `IrValue`s
    auto IntTy = dyn_cast<IntegerType>(Ty->getPointerElementType());
    if (!IntTy) {
      return mkError("alloca with unsupported type: ", V);
    }

    const uint32_t LlvmBitWidth = IntTy->getBitWidth();
    if (LlvmBitWidth > 64) {
      return mkError("alloca with unsupported size: ", V);
    }
    const uint32_t TcgBitWidth = llvmToTcgSize(LlvmBitWidth);

    // find or create a new IrValue
    Optional<TcgV> Tcg = findFreeTcgV(FreeList, TcgBitWidth, IrValue);
    if (Tcg) {
      return TAD.Map.try_emplace(V, *Tcg).first->second;
    } else {
      const auto Tcg = TcgV::makeTemp(TcgBitWidth, LlvmBitWidth, IrValue);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    }
  } else {
    // Otherwise, find or create a new IrPtr of the target pointer size
    Optional<TcgV> Tcg = findFreeTcgV(FreeList, TcgTargetPtrSize, IrPtr);
    if (Tcg) {
      return TAD.Map.try_emplace(V, *Tcg).first->second;
    } else {
      auto Tcg = TcgV::makeTemp(TcgTargetPtrSize, TcgTargetPtrSize, IrPtr);
      return TAD.Map.try_emplace(V, Tcg).first->second;
    }
  }

  return mkError("Unable to map constant ", V);
}

static Expected<TcgV> mapValue(TempAllocationData &Data,
                               FreeListVector &FreeList,
                               Value *V) {
  if (auto It = Data.Map.find(V); It != Data.Map.end()) {
    return It->second;
  }

  if (auto Int = dyn_cast<IntegerType>(V->getType())) {
    return mapInteger(Data, FreeList, V);
  } else if (auto PtrTy = dyn_cast<PointerType>(V->getType())) {
    return mapPointer(Data, FreeList, V);
  } else if (auto VecTy = dyn_cast<VectorType>(V->getType())) {

    auto IntTy = dyn_cast<IntegerType>(VecTy->getElementType());
    if (!IntTy) {
      return mkError("Vectors of non-integer element type not supported!\n");
    }
    uint32_t ElementCount = getVectorElementCount(VecTy);
    uint32_t ElementWidth = IntTy->getBitWidth();

    if (auto ICmp = dyn_cast<ICmpInst>(V)) {
      auto VecTy = cast<VectorType>(ICmp->getOperand(0)->getType());
      auto IntTy = cast<IntegerType>(VecTy->getElementType());
      ElementWidth = IntTy->getBitWidth();
    }

    uint32_t VectorWidth = ElementCount * ElementWidth;
    uint32_t TcgVectorWidth = llvmToTcgSize(VectorWidth);

    //// NOTE(anjo): A constant vector such as
    ////   <32 x i32> <i32 255, i32 255, ..., i32 255>
    //// where all elements have the same value, counts as a
    //// Constant and not ConstantVector, since all values are
    //// the "same" constant.
    ////
    //// Turns out not even constant vectors where all elements
    //// aren't the same counts as a ConstantVector...
    Optional<TcgV> Tcg = findFreeTcgV(FreeList, TcgVectorWidth, IrPtrToOffset);
    if (Tcg) {
      Tcg->LlvmSize = ElementWidth;
      Tcg->VectorElementCount = ElementCount;
    } else {
      Tcg = TcgV::makeVector(TcgVectorWidth, ElementWidth, ElementCount);
    }

    if (auto *Const = dyn_cast<Constant>(V)) {
      if (ElementWidth != 32 and ElementWidth != 16) {
        return mkError("non-splat vector constant that is not 32- or 16-bit: ", V);
      }

      // Make sure all arguments being splatted are mapped
      if (Constant *Splat = Const->getSplatValue()) {
        // Single splatted element
        Expected<TcgV> TcgSplat = mapInteger(Data, FreeList, Splat);
        assert(TcgSplat);
      } else {
        // Multipe integer constants
        for (unsigned I = 0; I < Tcg->VectorElementCount; ++I) {
          Value *V = Const->getAggregateElement(I);
          Expected<TcgV> TcgSplat = mapInteger(Data, FreeList, V);
          assert(TcgSplat);
        }
      }
    }

    return Data.Map.try_emplace(V, *Tcg).first->second;
  }

  return mkError("Unable to map value ", V);
}

static bool shouldSkipInstruction(const Instruction *const I, bool SkipReturnMov) {
  // Skip returns if we're skipping return mov's
  if (isa<ReturnInst>(I) and SkipReturnMov) {
    return true;
  }
  // Skip assertions
  auto Call = dyn_cast<CallInst>(I);
  if (!Call) {
    return false;
  }
  Function *F = Call->getCalledFunction();
  if (!F) {
    return false;
  }
  StringRef Name = F->getName();
  return (Name == "__assert_fail" or
          Name == "g_assertion_message_expr");
}

static bool shouldSkipValue(const Value *const V) {
  return (isa<GlobalValue>(V) or 
          isa<ConstantExpr>(V) or
          isa<BasicBlock>(V));
}

// Wrapper function to extract operands from GEP, call,
// and other instruction
static const iterator_range<User::const_op_iterator>
getOperands(const Instruction *const I) {
  if (auto *GEP = dyn_cast<GetElementPtrInst>(I)) {
    return GEP->operands();
  } else if (auto *Call = dyn_cast<CallInst>(I)) {
    return Call->args();
  } else {
    return I->operands();
  }
}

// A mapping of the return TCG variable to the value RetV is valid
// if no use of an argument is found between the use of value (where
// IBegin/BbBegin starts) and it's definition
// use of an argument is found between the old mapping
// and the new.
static bool isRetMapValid(Arguments &Args,
                          po_iterator<Function *> BbBegin,
                          po_iterator<Function *> BbEnd,
                          BasicBlock::reverse_iterator IBegin,
                          BasicBlock::reverse_iterator IEnd,
                          Value *RetV) {
  auto BbIt = BbBegin;
  auto IIt = IBegin;

  do {
    do {
      if (cast<Value>(&*IIt) == RetV) {
        return true;
      }

      for (auto &V : getOperands(&*IIt)) {
        if (isa<Argument>(V) and Args.ArgInfoMap[V] != ArgImmediate) {
          return false;
        }
      }
    } while (++IIt != IEnd);

    ++BbIt;
    IBegin = (*BbIt)->rbegin();
    IEnd = (*BbIt)->rend();
    IIt = IBegin;
  } while (BbIt != BbEnd);

  return false;
}

using TTAP = TcgTempAllocationPass;
TTAP::Result TTAP::run(Function &F, FunctionAnalysisManager &FAM) {
  FreeListVector FreeList;
  TempAllocationData Data;

  if (F.isDeclaration()) {
    return Data;
  }

  const auto &AnnotationResult =
      compat::getModuleAnalysisManagerProxyResult<MapAnnotationsPass>(FAM, F);
  const auto &AnnotationMap = AnnotationResult->Map;

  // Use function annotation data to force argument kind
  if (auto It = AnnotationMap.find(&F); It != AnnotationMap.end()) {
    for (const Annotation &Ann : It->second) {
      ArgumentKind Kind;
      switch (Ann.Kind) {
      case AnnLlvmToTcg:
        break;
      case AnnImmediate:
        Kind = ArgImmediate;
        break;
      case AnnPtrToOffset:
        Kind = ArgPtrToOffset;
        break;
      default:
        abort();
      }

      for (uint32_t i : Ann.ArgIndices) {
        assert(i < F.arg_size());
        Data.Args.ArgInfoMap[F.getArg(i)] = Kind;
      }
    }
  }

  for (Argument &Arg : F.args()) {
    // Check if argument corresponds to Env, if so set the special
    // EnvPtr field.
    if (auto Ptr = dyn_cast<PointerType>(Arg.getType())) {
      if (auto *Struct = dyn_cast<StructType>(Ptr->getPointerElementType())) {
        if (Struct->getName() == "struct.CPUArchState") {
          assert(!Data.Args.EnvPtr.hasValue());
          Data.Args.EnvPtr = &Arg;
        }
      }
    }

    // If we didn't force an argument kind via annotations, assume ArgTemp 
    if (Data.Args.ArgInfoMap.find(&Arg) == Data.Args.ArgInfoMap.end()) {
      Data.Args.ArgInfoMap[&Arg] = ArgTemp;
    }

    Data.Args.Args.insert(&Arg);
  }

  // The above only maps arguments that are actually used, make a final pass
  // over the arguments to map unused and immediate arguments.
  for (auto V : Data.Args.Args) {
    Expected<TcgV> Arg = mapValue(Data, FreeList, V);
    if (!Arg) {
      dbgs() << "Failed mapping arg: " << Arg.takeError() << "\n";
    }
    auto Info = Data.Args.ArgInfoMap[V];
  }

  // FilterFunctionsPass removes all functions with non-int/void return types,
  // assert this assumption.
  Type *RetTy = F.getReturnType();
  assert(isa<IntegerType>(RetTy) or RetTy->isVoidTy());
  // Map integer return values
  if (auto IntTy = dyn_cast<IntegerType>(RetTy)) {
    Data.ReturnValue = TcgV::makeTemp(llvmToTcgSize(IntTy->getBitWidth()),
                                      IntTy->getBitWidth(), IrValue);
  }

  // Begin/end iterators over basic blocks in the function. Used for checking
  // that the initial return value map is valid and later used for iterating
  // over basic blocks.
  auto ItBbBegin = po_begin(&F);
  auto ItBbEnd   = po_end(&F);

  // Skip mov's to return value if possible, results of previous
  // instructions might have been assigned the return value.
  //
  // This is possible if:
  //   1. The return value is not an argument.
  //   2. The return value is not a constant.
  //   3. No use of an argument has occured after the definition of the
  //      value being returned.
  {
    Instruction &I = *ItBbBegin->rbegin();

    if (auto Ret = dyn_cast<ReturnInst>(&I);
        Ret and Ret->getNumOperands() == 1) {
      Value *RetV = Ret->getReturnValue();
      if (!isa<Argument>(RetV) and
          !isa<ConstantInt>(RetV) and
        isRetMapValid(Data.Args,
                      ItBbBegin, ItBbEnd,
                      (*ItBbBegin)->rbegin(), (*ItBbBegin)->rend(),
                      RetV)) {
        Data.Map.try_emplace(RetV, *Data.ReturnValue);
        Data.SkipReturnMov = true;
      }
    }
  }

  // Iterate over instructions in reverse and try to allocate TCG variables.
  //
  // The algorithm is very straight forward, we keep a FreeList of TCG
  // variables we can reuse.  Variables are allocated on first use and
  // "freed" on definition.
  //
  // We allow reuse of the return TCG variable in order to save one variable
  // and skip the return mov if possible.  Since source and return variables
  // can overlap, when take the conservative route and only allow reuse of
  // the return variable if no arguments have been used.

  bool SeenArgUse = false;

  for (auto ItBb = ItBbBegin; ItBb != ItBbEnd; ++ItBb) {
    BasicBlock *BB = *ItBb;
    // Loop over instructions in the basic block in reverse
    for (auto IIt = BB->rbegin(), IEnd = BB->rend(); IIt != IEnd; ++IIt) {
      Instruction &I = *IIt;
      if (shouldSkipInstruction(&I, Data.SkipReturnMov)) {
        continue;
      }

      // For calls to the identity mapping pseudo instruction
      // we simply want to propagate the type allocated for the result of
      // the call to the operand.
      if (auto Call = dyn_cast<CallInst>(&I)) {
        auto Inst = getPseudoInstFromCall(Call);
        if (Inst and Inst.get() == IdentityMap) {
          auto It = Data.Map.find(cast<Value>(&I));
          if (It != Data.Map.end()) {
            TcgV Tcg = It->second;
            Tcg.LlvmSize = cast<IntegerType>(Call->getArgOperand(0)->getType())
                               ->getBitWidth();
            Data.Map.try_emplace(Call->getArgOperand(0), Tcg);
            continue;
          }
        }
      }

      // Use ptradd.* and bitcasts to infer vector type of arguments
      // NOTE: The ptr-to-offset attribute is useful for forcing vector types.
      // Vector types should be infered from bitcasts and stores, but
      // functions on vectors containing a single memcpy don't need casts
      // and will therefore be missed.
      if (auto BitCast = dyn_cast<BitCastInst>(&I)) {
        Value *Op = BitCast->getOperand(0);
        if (auto PtrTy = dyn_cast<PointerType>(BitCast->getType())) {
          auto VecTy = dyn_cast<VectorType>(PtrTy->getPointerElementType());
          if (VecTy) {
            auto Call = dyn_cast<CallInst>(Op);
            if (Call and Call->getCalledFunction()->getName().startswith("ptradd")) {
              Op = Call->getArgOperand(0);
            }

            if (isa<Argument>(Op)) {
              Data.Args.ArgInfoMap[Op] = ArgPtrToOffset;
            }
          }
        }
      }

      // Check if we've encountered any non-immediate argument yet
      for (const Use &U : getOperands(&I)) {
        if (isa<Argument>(U) and Data.Args.ArgInfoMap[U] != ArgImmediate) {
          SeenArgUse = true;
        }
      }

      // Free up variables as they are defined, iteration is in post order
      // meaning uses of vars always occur before definitions.
      bool IsArg = Data.Args.ArgInfoMap.find(cast<Value>(&I))
                   != Data.Args.ArgInfoMap.end();
      auto It = Data.Map.find(cast<Value>(&I));
      if (!IsArg and It != Data.Map.end() and !cast<Value>(&I)->getType()->isVoidTy()) {
        TcgV &Tcg = It->second;
        switch (Tcg.Kind) {
        case IrValue:
        case IrPtr:
        case IrPtrToOffset:
          FreeList.push_back(Tcg);
          break;
        case IrConst:
        case IrEnv:
        case IrImmediate:
          break;
        default:
          abort();
        }
      }

      // Loop over operands and assign TcgV's. On first encounter of a given
      // operand we assign a new TcgV from the FreeList.
      for (const Use &V : getOperands(&I)) {
        auto It = Data.Map.find(V);
        if (It != Data.Map.end() or shouldSkipValue(V)) {
          continue;
        }

        Expected<TcgV> Tcg = mapValue(Data, FreeList, V);
        if (!Tcg) {
          continue;
        }

        // If our value V got mapped to the return value,
        // make sure the mapping is valid
        //
        // A mapping to the return value is valid as long as
        // an argument has not been used.  This is to prevent clobbering in
        // the case that arguments and the return value overlap.
        if (Data.ReturnValue.hasValue() and *Tcg == *Data.ReturnValue) {
          bool Valid = isRetMapValid(Data.Args, ItBb, ItBbEnd, IIt, IEnd, V);
          if (!SeenArgUse and Valid) {
            continue;
          }

          // The mapping was not valid, erase it and assign a new one
          Data.Map.erase(V);
          assert(mapValue(Data, FreeList, V));
        }
      }
    }
  }

  return Data;
}

llvm::AnalysisKey TTAP::Key{};
