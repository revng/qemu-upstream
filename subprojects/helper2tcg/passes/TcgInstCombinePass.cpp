#include "TcgInstCombinePass.h"

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/InstIterator.h"

#include "llvm-compat.h"
#include <utils/Common.h>

#include "PseudoInst.h"

#include <algorithm> // for std::swap

using namespace llvm;
using namespace PatternMatch;

using EraseInstVec = SmallVector<Instruction *, 16>;
using UsageCountMap = DenseMap<Value *, uint16_t>;

// Helper function to remove an instruction only if all uses have been removed.
// This way we can keep track instruction uses without having to modify the IR,
// or without having to iterate over all uses everytime we wish to remove an
// instruction.
inline void addToEraseVectorIfUnused(EraseInstVec &InstToErase,
                                     UsageCountMap &UsageMap,
                                     Value *V) {
  auto I = dyn_cast<Instruction>(V);
  if (!I)
    return;

  // Add V to map if not there
  if (UsageMap.count(V) == 0)
    UsageMap[V] = V->getNumUses();

  // Erase if count reaches zero
  if (--UsageMap[V] == 0) {
    InstToErase.push_back(I);
    UsageMap.erase(V);
  }
}

// TODO(anjo): Comment
inline Value *shrinkVectorValue(Module &M,
                                       bool IsSplat, Value *V,
                                       unsigned DesiredSize) {
  if (IsSplat) {
    auto *SplatV = dyn_cast<ConstantInt>(cast<CallInst>(V)->getArgOperand(0));
    if (!SplatV) {
      return nullptr;
    }

    const APInt &IntValue = SplatV->getValue();
    if (!IntValue.isIntN(DesiredSize))
      return nullptr;

    auto *OldVecTy = cast<VectorType>(V->getType());
    auto *NewIntTy = IntegerType::get(M.getContext(), DesiredSize);
    auto *NewVecTy = VectorType::get(NewIntTy, OldVecTy->getElementCount());

    FunctionCallee Fn = pseudoInstFunction(M, VecSplat, NewVecTy, {NewVecTy});
    IRBuilder<> Builder(cast<Instruction>(V));
    CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(NewIntTy, IntValue)});
    V->replaceAllUsesWith(Call);
    return Call;
  } else if (auto SExt = dyn_cast<SExtInst>(V)) {
    // Check that source type matches desired type, meaning we can remove the
    // sext
    auto OldVecTy = cast<VectorType>(SExt->getSrcTy());
    auto OldIntTy = cast<IntegerType>(OldVecTy->getElementType());
    if (OldIntTy->getBitWidth() != DesiredSize)
      return nullptr;
    return SExt->getOperand(0);
  } else if (auto ZExt = dyn_cast<ZExtInst>(V)) {
    // Check that source type matches desired type, meaning we can remove the
    // sext
    auto OldVecTy = cast<VectorType>(ZExt->getSrcTy());
    auto OldIntTy = cast<IntegerType>(OldVecTy->getElementType());
    if (OldIntTy->getBitWidth() != DesiredSize)
      return nullptr;
    return ZExt->getOperand(0);
  } else if (auto CDV = dyn_cast<ConstantDataVector>(V)) {
    // Create a new constant vector with a smaller size
    // and copy over elements if possible.
    // TODO(anjo): "if possible"
    SmallVector<Constant *, 32> Values;
    Values.reserve(CDV->getNumElements());
    auto NewIntTy = IntegerType::get(M.getContext(), DesiredSize);
    for (unsigned i = 0; i < CDV->getNumElements(); ++i) {
      Values.push_back(ConstantInt::get(NewIntTy, CDV->getElementAsInteger(i)));
    }
    return ConstantVector::get(Values);
  }

  return nullptr;
}

struct SplatInfo {
  ShuffleVectorInst *Shuffle;
  VectorType *VecTy;
  Value *SplatV;
};

struct BinOpShrinkInfo {
  BinaryOperator *Op;
  IntegerType *IntTy;
  VectorType *VecTy;
};

struct BinOpSplatInfo {
  Instruction *I;
};

struct BinOpStoreInfo {
  StoreInst *Store;
  Type *PtrTy;
  Type *ValueTy; 
};

// TODO(anjo): Way to many arguments
bool convertSelectICmpToMinMax(Module &M, SelectInst *Select, ICmpInst *ICmp,
                               ICmpInst::Predicate &Pred,
                               Value *ICmpOp0, Value *ICmpOp1,
                               Value *SelectOp0, Value *SelectOp1,
                               EraseInstVec &InstToErase,
                               UsageCountMap &UsageMap) {
#if LLVM_VERSION_MAJOR > 11
  if (ICmpOp0 != SelectOp0 or ICmpOp1 != SelectOp1) {
    return false;
  }

  Intrinsic::ID Intrin;
  switch (Pred) {
  case ICmpInst::ICMP_SGT: Intrin = Intrinsic::smax; break;
  case ICmpInst::ICMP_UGT: Intrin = Intrinsic::umax; break;
  case ICmpInst::ICMP_SLT: Intrin = Intrinsic::smin; break;
  case ICmpInst::ICMP_ULT: Intrin = Intrinsic::umin; break;
  default: return false;
  }

  auto Ty = Select->getType();
  auto MaxMinF = Intrinsic::getDeclaration(&M, Intrin, {Ty});

  IRBuilder<> Builder(Select);
  auto Call = Builder.CreateCall(MaxMinF, {ICmpOp0, ICmpOp1});
  Select->replaceAllUsesWith(Call);

  InstToErase.push_back(Select);
  addToEraseVectorIfUnused(InstToErase, UsageMap, ICmp);
  return true;
#else
  return false
#endif
}

void convertSelectICmp(Module &M, SelectInst *Select, ICmpInst *ICmp,
                       EraseInstVec &InstToErase, UsageCountMap &UsageMap) {
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
  if (convertSelectICmpToMinMax(M, Select, ICmp, Pred,
                                ICmpOp0, ICmpOp1,
                                SelectOp0, SelectOp1,
                                InstToErase, UsageMap)) {
    return;
  }

  // If min/max failed we fallback to converting to a movcond
  //   %5 = call @Movcond.[cond].*(%1, %0, %3, %4)

  // We only handle integers, we have no movcond equivalent in gvec
  auto *IntTy = dyn_cast<IntegerType>(Select->getType());
  if (!IntTy) {
    return;
  }

  // If the type of the comparison does not match the return type of the
  // select statement, we cannot do anything so skip
  if (ICmpOp0->getType() != IntTy)
    return;

  IRBuilder<> Builder(Select);
  if (cast<IntegerType>(ICmpOp0->getType())->getBitWidth() < IntTy->getBitWidth()) {
    if (ICmp->isSigned(Pred)) {
      ICmpOp0 = Builder.CreateSExt(ICmpOp0, IntTy);
      ICmpOp1 = Builder.CreateSExt(ICmpOp1, IntTy);
    } else {
      ICmpOp0 = Builder.CreateZExt(ICmpOp0, IntTy);
      ICmpOp1 = Builder.CreateZExt(ICmpOp1, IntTy);
    }
  }

  // Create @Movcond.[slt|...].* function
  FunctionCallee Fn = pseudoInstFunction(M, Movcond, IntTy, {IntTy, IntTy, IntTy, IntTy, IntTy});
  CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(IntTy, Pred), ICmpOp0, ICmpOp1, SelectOp0, SelectOp1});
  Select->replaceAllUsesWith(Call);

  InstToErase.push_back(Select);
  addToEraseVectorIfUnused(InstToErase, UsageMap, ICmp);
}

llvm::PreservedAnalyses
TcgInstCombinePass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  for (Function &F : M) {
    if (F.isDeclaration()) {
      continue;
    }

    EraseInstVec InstToErase;
    UsageCountMap UsageMap;
    SmallPtrSet<Value *, 8> Splats;
    SmallVector<SplatInfo, 8> SplatWorklist;
    SmallVector<BinOpShrinkInfo, 8> BinOpShrinkWorklist;
    SmallVector<BinOpSplatInfo, 8> BinOpSplatWorklist;
    SmallVector<BinOpStoreInfo, 8> VecOpStoreWorklist;
    LLVMContext &Context = F.getContext();

    // Perform a first pass over all instructions and:
    //   1. Perform simpler and independent replacements
    //   2. Save other replacements with dependencies to worklists
    for (Instruction &I : instructions(F)) {
      {
        if (I.getOpcode() == Instruction::AShr) {
          auto *IntTy = dyn_cast<IntegerType>(I.getType());
          if (IntTy and IntTy->getBitWidth() < 32) {
            SmallVector<Value *, 8> UsesToExplore;
            SmallVector<Value *, 8> Downcasts;

            IRBuilder<> Builder(&I);
            auto *IntTy = Builder.getInt32Ty();

            Value *Op1 = I.getOperand(0);
            Value *Op2 = I.getOperand(1);
            Value *NewOp1 = I.getOperand(0);
            Value *NewOp2 = I.getOperand(1);
            if (auto *ConstInt = dyn_cast<ConstantInt>(Op1)) {
              NewOp1 = ConstantInt::get(IntTy, ConstInt->getZExtValue());
            } else {
              NewOp1 = Builder.CreateSExt(Op1, IntTy);
            }
            if (auto *ConstInt = dyn_cast<ConstantInt>(Op2)) {
              NewOp2 = ConstantInt::get(IntTy, ConstInt->getZExtValue());
            } else {
              NewOp2 = Builder.CreateSExt(Op2, IntTy);
            }
            auto *AShr = Builder.CreateAShr(NewOp1, NewOp2);
            auto *Trunc = Builder.CreateTrunc(AShr, I.getType());
            I.replaceAllUsesWith(Trunc);
            InstToErase.push_back(&I);


          }
        }
      }

      // Convert vector intrinsics
      //   %0 = insertelement ...
      //   %1 = shuffle ...
      // to
      //   %0 = call @Splat.*
      {
          Value *V;
          if (match(&I, compat_m_Shuffle(compat_m_InsertElt(m_Value(), m_Value(V), m_ZeroInt()), m_Value(), compat_m_ZeroMask()))) {
            SplatWorklist.push_back(SplatInfo{
                                    cast<ShuffleVectorInst>(&I),
                                    cast<VectorType>(I.getType()),
                                    V});
            InstToErase.push_back(&I);
            addToEraseVectorIfUnused(InstToErase, UsageMap, I.getOperand(0));
          }
      }

      // Depends on the above
      //
      // For binary ops on vectors, shrink
      //   %0 = @Splat.vNxM(...) or
      //        [s|z]ext <KxL> to <NxM> or
      //        <iM ...> (constant vector)
      //   %1 = @Splat.vNxM(...) or
      //        [s|z]ext <KxL> to <NxM> or
      //        <iM ...> (constant vector)
      //   %2 = <NxM> %0 op <NxM> %1
      //   %3 = trunc <NxM> to <KxL>
      // to
      //   %2 = <KxL> ... op <KxL> ...
      {
          Value *A, *B;
          if (match(&I, m_Trunc(m_Shift(m_Value(A), m_Value(B))))) {
            if (auto VecTy = dyn_cast<VectorType>(A->getType())) {
              auto NewIntTy = cast<IntegerType>(cast<VectorType>(I.getType())->getElementType());
              auto NewVecTy = VectorType::get(NewIntTy, VecTy->getElementCount());

              BinOpShrinkWorklist.push_back(BinOpShrinkInfo{
                                              cast<BinaryOperator>(I.getOperand(0)),
                                              NewIntTy,
                                              NewVecTy
                                            });
            }
          }
      }

      // Depends on the above
      //
      // For binary ops on vectors, convert
      //   %0 = ...
      //   %1 = @Splat.vNxM(%2) or
      //        <iM %2> (constant vector)
      //   %2 = <NxM> %0 op <NxM> %1
      // to
      //   %0 = ...
      //   %2 = <NxM> %0 op %1
      {
        if (auto *Store = dyn_cast<StoreInst>(&I)) {
          Value* ValueOp = Store->getValueOperand();
          Type *ValueTy = ValueOp->getType();
          if (ValueTy->isVectorTy()) {
            auto *Bitcast = cast<BitCastInst>(Store->getPointerOperand());
            Value *PtrOp = Bitcast->getOperand(0);
            Type *PtrTy = Bitcast->getType();
            // Ensure store and binary op. are in the same basic block since the
            // op. is moved to the store.
            bool InSameBB = cast<Instruction>(ValueOp)->getParent() 
                            == I.getParent();

            if (InSameBB) {
              VecOpStoreWorklist.push_back(BinOpStoreInfo{
                .Store = Store,
                .PtrTy = PtrTy,
                .ValueTy = ValueTy,
              });
            }
          }
        }
      }

      // Depends on vector binary ops.
      //
      // For binary ops on vectors where the result is stored to a
      // pointer
      //   %3 = <NxM> %1 [op] <NxM> %2
      //   %4 = bitcast i8* %0 to <NxM>*
      //   store <NxM> %3, <NxM>* %4
      // to
      //   call @Vec[Op]Store.*(%0, %1, %2)
      //
      // This deals with the duality of pointers and vectors, and
      // simplifies the backend.  We previously kept a map on the
      // side to propagate "vector"-ness from %3 to %4 via the store,
      // no longer!
      {
        Value *LHS, *RHS;
        if (match(&I, m_Shift(m_Value(LHS), m_Value(RHS))) ||
            match(&I,   m_And(m_Value(LHS), m_Value(RHS))) ||
            match(&I,   m_Xor(m_Value(LHS), m_Value(RHS))) ||
            match(&I,    m_Or(m_Value(LHS), m_Value(RHS))) ||
            match(&I,   m_Add(m_Value(LHS), m_Value(RHS))) ||
            match(&I,   m_Mul(m_Value(LHS), m_Value(RHS)))) {
          if (!isa<VectorType>(LHS->getType()) or
              !isa<VectorType>(RHS->getType())) {
            continue;
          }
          BinOpSplatWorklist.push_back(BinOpSplatInfo{&I});
        }
      }

      // Independent of above
      //
      // Convert
      //   %2 = icmp [sgt|ugt|slt|ult] %0, %1
      //   %5 = select %2, %3, %4
      // to
      //   %5 = [s|u][max|min] %0, %1;                          if %0 == %3 && %1 == %4
      //   %5 = call @Movcond.[cond].*(%1, %0, %3, %4);         otherwise
      if (auto Select = dyn_cast<SelectInst>(&I)) {
        if (auto ICmp = dyn_cast<ICmpInst>(Select->getCondition())) {
          convertSelectICmp(M, Select, ICmp, InstToErase, UsageMap);
        }
      }

      // Independent of above
      //
      // Convert
      //   %2 = icmp [sgt|ugt|slt|ult] %0, %1
      //   %5 = select %2, %3, %4
      // to
      //   %5 = [s|u][max|min] %0, %1;                          if %0 == %3 && %1 == %4
      //   %5 = call @Movcond.[cond].*(%1, %0, %3, %4);         otherwise
      if (auto *Call = dyn_cast<CallInst>(&I)) {
        Function *F = Call->getCalledFunction();
        StringRef Name = F->getName();
        if (Name.consume_front("cpu_")) {
          bool IsLoad = Name.consume_front("ld");
          bool IsStore = !IsLoad and Name.consume_front("st");
          if (IsLoad or IsStore) {
            bool Signed = !Name.consume_front("u");
            uint8_t Size = 0;
            switch (Name[0]) {
              case 'b': Size = 1; break;
              case 'w': Size = 2; break;
              case 'l': Size = 4; break;
              case 'q': Size = 8; break;
              default: abort();
            }

            uint8_t Endianness = 0; // unknown
            if (Size > 1) {
              Name = Name.drop_front(2);
              switch (Name[0]) {
                case 'l': Endianness = 1; break;
                case 'b': Endianness = 2; break;
                default: abort();
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
              FunctionCallee Fn = pseudoInstFunction(M, HostLoad, RetTy, {AddrTy, FlagTy, FlagTy, FlagTy});
              NewCall = Builder.CreateCall(Fn, {AddrOp, SignOp, SizeOp, EndianOp});
            } else {
              Value *ValueOp = Call->getArgOperand(2);
              IntegerType *ValueTy = cast<IntegerType>(ValueOp->getType());
              FunctionCallee Fn = pseudoInstFunction(M, HostLoad, Builder.getVoidTy(), {AddrTy, ValueTy, FlagTy, FlagTy});
              NewCall = Builder.CreateCall(Fn, {AddrOp, ValueOp, SizeOp, EndianOp});
            }
            Call->replaceAllUsesWith(NewCall);
            InstToErase.push_back(Call);
          }
        }
      }

      // Independent of above
      //
      // Depends on binary op one
      //
      // Convert
      //   %1 = xor <KxL> %0, L -1
      // to
      //   %1 = call @VecNot.vKxL(%0)
      //
      //{
      //  Value *V;
      //  Constant *C;
      //  if (match(&I, m_Xor(m_Value(V), m_Constant(C)))) {
      //    auto ConstInt = dyn_cast<ConstantInt>(C);
      //    auto VecTy = dyn_cast<VectorType>(V->getType());

      //    // Make sure arguments have the correct type, and that the
      //    // second arg. is -1
      //    if (!VecTy or !ConstInt or ConstInt->getSExtValue() != -1)
      //      continue;

      //    // Create the @not.* function
      //    auto FT = FunctionType::get(VecTy, {VecTy}, false);
      //    auto FnName = Twine("not.")
      //      .concat(appendTypeToTwine(VecTy))
      //      .str();
      //    auto Fn = M.getOrInsertFunction(FnName, FT);

      //    IRBuilder<> Builder(&I);
      //    auto Call = Builder.CreateCall(Fn, {V});
      //    I.replaceAllUsesWith(Call);
      //    InstToErase.push_back(&I);
      //  }
      //}
    }

    // Perform a second pass over the instructions. Can be combined with the
    // above by using a worklist and making sure we have access to the
    // BasicBlock.
    //
    // Depends on icmp,select -> @movcond
    //
    // Convert
    //   %cond = icmp [cond] i32 %0, i32 %1
    //   br i1 %cond, label %true, label %false
    // to
    //   call void @brcond.[cond].i32(i32 %0, i32 %1, label %true.exit, label %false)
    //   br i1 %cond, label %true, label %false !dead-branch
    // note the old branch still remains as @brcond.* is not an actual branch
    // instruction. Removing the old branch would result in broken IR.
    //
    // Additionally if the %false basic block immediatly succeeds the current
    // one, we can ignore the false branch and fallthrough, this is indicated via
    // !fallthrough metadata on the call.
    ReversePostOrderTraversal<Function *> RPOT(&F);
    for (auto BbIt = RPOT.begin(); BbIt != RPOT.end(); ++BbIt) {
      BasicBlock &BB = **BbIt;
      for (Instruction &I : BB) {
        auto *ICmp = dyn_cast<ICmpInst>(&I);
        if (!ICmp) {
          continue;
        }

        // Since we want to remove the icmp instruction we ensure that
        // all uses are branch instructions that can be converted into
        // @brcond.* calls.
        bool HasNonBrUsers = false;
        for (User *U : ICmp->users()) {
          if (!isa<BranchInst>(U)) {
            HasNonBrUsers = true;
            break;
          }
        }
        if (HasNonBrUsers) {
          continue;
        }

        Value *Op0 = ICmp->getOperand(0);
        Value *Op1 = ICmp->getOperand(1);
        auto *CmpIntTy = dyn_cast<IntegerType>(Op0->getType());
        if (!CmpIntTy) {
          continue;
        }
        for (User *U : ICmp->users()) {
          auto *Br = cast<BranchInst>(U);

          BasicBlock *True = Br->getSuccessor(0);
          BasicBlock *False = Br->getSuccessor(1);

          auto NextIt = BbIt;
          BasicBlock *NextBB = &**(++NextIt);
          // If the next basic block is either of our true/false branches,
          // we can fallthrough instead of branching.
          bool Fallthrough = (NextBB == True or NextBB == False);

          // If the succeeding basic block is the true branch we invert
          // the condition so we can !fallthrough instead.
          ICmpInst::Predicate Predicate;
          if (NextBB == True) {
            std::swap(True, False);
            Predicate = ICmp->getInversePredicate();
          } else {
            Predicate = ICmp->getPredicate();
          }

          IRBuilder<> Builder(Br);
          FunctionCallee Fn = pseudoInstFunction(M, Brcond, Builder.getVoidTy(), {CmpIntTy, CmpIntTy, CmpIntTy, True->getType(), False->getType()});
          CallInst *Call = Builder.CreateCall(Fn, {ConstantInt::get(CmpIntTy, Predicate), Op0, Op1, True, False});

          if (Fallthrough) {
            MDTuple *N = MDNode::get(Context, MDString::get(Context, ""));
            Call->setMetadata("fallthrough", N);
          }

          //
          // TODO(anjo): I'm not a fan.
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
          // Note also: LLVM expectects the BB to end in a single branch.
          //
          BranchInst *DeadBranch = Builder.CreateCondBr(
              ConstantInt::getFalse(Context), True, False);
          {
            MDTuple *N = MDNode::get(Context, MDString::get(Context, ""));
            DeadBranch->setMetadata("dead-branch", N);
          }

          InstToErase.push_back(Br);
        }
        InstToErase.push_back(ICmp);
      }
    }

    // Finally iterate over all worklists and perform replacements in the
    // correct order.

    // Replace vector shuffles with @splat function calls
    for (SplatInfo &S : SplatWorklist) {
      IRBuilder<> Builder(S.Shuffle);
      FunctionCallee Fn = pseudoInstFunction(M, VecSplat, S.VecTy, {S.SplatV->getType()});
      CallInst *Call = Builder.CreateCall(Fn, {S.SplatV});
      S.Shuffle->replaceAllUsesWith(Call);
      Splats.insert(Call);
    }

    // Shrink vector types in bin ops.
    for (BinOpShrinkInfo &BB : BinOpShrinkWorklist) {
      Value *A = BB.Op->getOperand(0);
      Value *B = BB.Op->getOperand(1);
      Value *ShrunkOp1 = shrinkVectorValue(M, Splats.contains(A), A, BB.IntTy->getBitWidth());
      Value *ShrunkOp2 = shrinkVectorValue(M, Splats.contains(B), B, BB.IntTy->getBitWidth());
      if (!ShrunkOp1 or !ShrunkOp2) {
        continue;
      }

      BinaryOperator::Create(BB.Op->getOpcode(),
                             ShrunkOp1,
                             ShrunkOp2, Twine(), BB.Op);
    }

    // Forward splat in vector bin. ops. to immediate
    for (BinOpSplatInfo &BB : BinOpSplatWorklist) {
      Value *SplatOp = BB.I->getOperand(1);
      Value *SplatValue = nullptr;
      if (auto *C = dyn_cast<Constant>(SplatOp)) {
        SplatValue = C->getSplatValue();
        if (!SplatValue) {
          if (Splats.contains(SplatOp)) {
            SplatValue = cast<CallInst>(SplatOp)->getOperand(0);
            addToEraseVectorIfUnused(InstToErase, UsageMap, SplatOp);
          } else {
            continue;
          }
        }
      } else {
        // calls, muls, adds, non-const intstructions
        continue;
      }

      auto VecTy = cast<VectorType>(BB.I->getOperand(0)->getType());
      auto IntTy = cast<IntegerType>(VecTy->getElementType());

      auto ConstInt = dyn_cast<ConstantInt>(SplatValue);
      bool ConstIsNegOne = ConstInt and ConstInt->getSExtValue() == -1;
      bool IsNot = cast<BinaryOperator>(BB.I)->getOpcode() == Instruction::Xor
                   and ConstIsNegOne;
      if (IsNot) {
        FunctionCallee Fn = pseudoInstFunction(M, VecNot, VecTy, {VecTy});
        IRBuilder<> Builder(BB.I);
        CallInst *Call = Builder.CreateCall(Fn, {BB.I->getOperand(0)});
        BB.I->replaceAllUsesWith(Call);
      } else {
        PseudoInst Inst;
        switch (cast<BinaryOperator>(BB.I)->getOpcode()) {
        case Instruction::Add:  Inst = VecAddScalar;  break;
        case Instruction::Sub:  Inst = VecSubScalar;  break;
        case Instruction::Mul:  Inst = VecMulScalar;  break;
        case Instruction::Xor:  Inst = VecXorScalar;  break;
        case Instruction::Or:   Inst = VecOrScalar;   break;
        case Instruction::And:  Inst = VecAndScalar;  break;
        case Instruction::Shl:  Inst = VecShlScalar;  break;
        case Instruction::LShr: Inst = VecLShrScalar; break;
        case Instruction::AShr: Inst = VecAShrScalar; break;
        default: abort();
        }
        FunctionCallee Fn = pseudoInstFunction(M, Inst, VecTy, {VecTy, IntTy});
        IRBuilder<> Builder(BB.I);
        CallInst *Call = Builder.CreateCall(Fn, {BB.I->getOperand(0), SplatValue});
        BB.I->replaceAllUsesWith(Call);
      }
    }

    // Combine vector binary op. and stores
    for (BinOpStoreInfo &Info : VecOpStoreWorklist) {
      Value *ValueOp = Info.Store->getValueOperand();
      Value *PtrOp = Info.Store->getPointerOperand();
      if (auto *BinOp = dyn_cast<BinaryOperator>(ValueOp)) {
        PseudoInst Inst;
        switch (BinOp->getOpcode()) {
        case Instruction::Add:  Inst = VecAddStore;  break;
        case Instruction::Sub:  Inst = VecSubStore;  break;
        case Instruction::Mul:  Inst = VecMulStore;  break;
        case Instruction::Xor:  Inst = VecXorStore;  break;
        case Instruction::Or:   Inst = VecOrStore;   break;
        case Instruction::And:  Inst = VecAndStore;  break;
        case Instruction::Shl:  Inst = VecShlStore;  break;
        case Instruction::LShr: Inst = VecLShrStore; break;
        case Instruction::AShr: Inst = VecAShrStore; break;
        default: abort();
        }
        IRBuilder<> Builder(Info.Store);
        FunctionCallee Fn = pseudoInstFunction(M, Inst, Builder.getVoidTy(),
                                               {Info.PtrTy,
                                                Info.ValueTy,
                                                Info.ValueTy});
        Builder.CreateCall(Fn, {PtrOp,
                           BinOp->getOperand(0),
                           BinOp->getOperand(1)});
        // Remove store instruction, this ensures DCE can cleanup the rest,
        // we also remove ValueOp here since it's a call and won't get cleaned
        // by DCE.
        InstToErase.push_back(Info.Store);
      } else if (auto *Call = dyn_cast<CallInst>(ValueOp)) {
        Function *F = Call->getCalledFunction();
        if (Expected<PseudoInst> OldInst = getPseudoInstFromCall(Call)) {
          // Map scalar vector pseudo instructions to store variants
          PseudoInst NewInst;
          switch (OldInst.get()) {
          case VecNot:        NewInst = VecNotStore;        break;
          case VecAddScalar:  NewInst = VecAddScalarStore;  break;
          case VecSubScalar:  NewInst = VecSubScalarStore;  break;
          case VecMulScalar:  NewInst = VecMulScalarStore;  break;
          case VecXorScalar:  NewInst = VecXorScalarStore;  break;
          case VecOrScalar:   NewInst = VecOrScalarStore;   break;
          case VecAndScalar:  NewInst = VecAndScalarStore;  break;
          case VecShlScalar:  NewInst = VecShlScalarStore;  break;
          case VecLShrScalar: NewInst = VecLShrScalarStore; break;
          case VecAShrScalar: NewInst = VecAShrScalarStore; break;
          default: abort();
          }
          IRBuilder<> Builder(Info.Store);
          if (NewInst == VecNotStore) {
            // Unary ops
            FunctionCallee Fn = pseudoInstFunction(M, NewInst, Builder.getVoidTy(),
                                                   {Info.PtrTy,
                                                    Info.ValueTy});
            Builder.CreateCall(Fn, {PtrOp, Call->getOperand(0)});
            InstToErase.push_back(cast<Instruction>(ValueOp));
          } else {
            // Binary ops
            FunctionCallee Fn = pseudoInstFunction(M, NewInst, Builder.getVoidTy(),
                                                   {Info.PtrTy,
                                                    Info.ValueTy,
                                                    Call->getOperand(1)->getType()});
            Builder.CreateCall(Fn, {PtrOp, Call->getOperand(0), Call->getOperand(1)});
            InstToErase.push_back(cast<Instruction>(ValueOp));
          }
          // Remove store instruction, this ensures DCE can cleanup the rest,
          // we also remove ValueOp here since it's a call and won't get cleaned
          // by DCE.
          InstToErase.push_back(Info.Store);
        } else if (F->isIntrinsic()) {
          Instruction *Inst = cast<Instruction>(ValueOp);
          PseudoInst NewInst;
          switch (F->getIntrinsicID()) {
          case Intrinsic::sadd_sat: NewInst = VecSignedSatAddStore; break;
          case Intrinsic::ssub_sat: NewInst = VecSignedSatSubStore; break;
          case Intrinsic::fshr: NewInst = VecFunnelShrStore; break;
          case Intrinsic::abs: NewInst = VecAbsStore; break;
          case Intrinsic::smax: NewInst = VecSignedMaxStore; break;
          case Intrinsic::umax: NewInst = VecUnsignedMaxStore; break;
          case Intrinsic::smin: NewInst = VecSignedMinStore; break;
          case Intrinsic::umin: NewInst = VecUnsignedMinStore; break;
          case Intrinsic::ctlz: NewInst = VecCtlzStore; break;
          case Intrinsic::cttz: NewInst = VecCttzStore; break;
          case Intrinsic::ctpop: NewInst = VecCtpopStore; break;
          default:
            dbgs() << "Uhandled vector + bitcast + store op. " << *ValueOp << "\n";
            abort();
          }
          const uint8_t ArgCount = pseudoInstArgCount(NewInst);
          // Add one to account for extra store pointer argument of Vec*Store
          // pseudo instructions.
          assert(ArgCount > 0 and ArgCount-1 <= Inst->getNumOperands());
          IRBuilder<> Builder(Info.Store);
          SmallVector<Type *, 8> ArgTys;
          SmallVector<Value *, 8> Args;
          ArgTys.push_back(Info.PtrTy);
          Args.push_back(PtrOp);
          for (unsigned I = 0; I < ArgCount-1; ++I) {
            Value *Op = Inst->getOperand(I);
            ArgTys.push_back(Op->getType());
            Args.push_back(Op);
          }
          FunctionCallee Fn = pseudoInstFunction(M, NewInst, Builder.getVoidTy(),
                                                 ArgTys);
          Builder.CreateCall(Fn, Args);
          InstToErase.push_back(cast<Instruction>(ValueOp));
          // Remove store instruction, this ensures DCE can cleanup the rest,
          // we also remove ValueOp here since it's a call and won't get cleaned
          // by DCE.
          InstToErase.push_back(Info.Store);
        } else {
          dbgs() << "Uhandled vector + bitcast + store op. " << *ValueOp << "\n";
          abort();
        }
      } else {
        Instruction *Inst = cast<Instruction>(ValueOp);
        PseudoInst NewInst;
        switch (Inst->getOpcode()) {
        case Instruction::Trunc: NewInst = VecTruncStore; break;
        case Instruction::Select: NewInst = VecSelectStore; break;
        default:
          dbgs() << "Uhandled vector + bitcast + store op. " << *ValueOp << "\n";
          abort();
        }
        const uint8_t ArgCount = pseudoInstArgCount(NewInst);
        // Add one to account for extra store pointer argument of Vec*Store
        // pseudo instructions.
        assert(ArgCount > 0 and ArgCount-1 <= Inst->getNumOperands());
        IRBuilder<> Builder(Info.Store);
        SmallVector<Type *, 8> ArgTys;
        SmallVector<Value *, 8> Args;
        ArgTys.push_back(Info.PtrTy);
        Args.push_back(PtrOp);
        for (unsigned I = 0; I < ArgCount-1; ++I) {
          Value *Op = Inst->getOperand(I);
          ArgTys.push_back(Op->getType());
          Args.push_back(Op);
        }
        FunctionCallee Fn = pseudoInstFunction(M, NewInst, Builder.getVoidTy(),
                                               ArgTys);
        Builder.CreateCall(Fn, Args);
        InstToErase.push_back(cast<Instruction>(ValueOp));
        // Remove store instruction, this ensures DCE can cleanup the rest,
        // we also remove ValueOp here since it's a call and won't get cleaned
        // by DCE.
        InstToErase.push_back(Info.Store);
      }
    }

    for (Instruction *I : InstToErase) {
      I->eraseFromParent();
    }
  }

  return PreservedAnalyses::all();
}
