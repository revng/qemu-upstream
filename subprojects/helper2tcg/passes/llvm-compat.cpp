#include "llvm-compat.h"

#if LLVM_VERSION_MAJOR > 10
#include "llvm/CodeGen/CommandFlags.h"
#else
#include "llvm/CodeGen/CommandFlags.inc"
#endif

#include <string>

// Static variables required by LLVM
// 
// Defining RegisterCodeGenFlags with static duration registers extra
// codegen commandline flags for specifying the target arch.
#if LLVM_VERSION_MAJOR > 10
static llvm::codegen::RegisterCodeGenFlags CGF;
#endif
static llvm::ExitOnError ExitOnErr;

namespace compat {

using namespace llvm;

#if LLVM_VERSION_MAJOR > 10
llvm::TargetMachine *getTargetMachine(llvm::Triple &TheTriple) {
  const TargetOptions Options{};
  std::string Error;
  const Target *TheTarget = llvm::TargetRegistry::lookupTarget(llvm::codegen::getMArch(), TheTriple, Error);
  // Some modules don't specify a triple, and this is okay.
  if (!TheTarget) {
    return nullptr;
  }

  return TheTarget->createTargetMachine(TheTriple.getTriple(), llvm::codegen::getCPUStr(), llvm::codegen::getFeaturesStr(),
                                        Options, llvm::codegen::getExplicitRelocModel(),
                                        llvm::codegen::getExplicitCodeModel(),
                                        llvm::CodeGenOpt::Aggressive);
}
#else
llvm::TargetMachine *getTargetMachine(llvm::Triple &TheTriple) {
  const TargetOptions Options{};
  std::string Error;
  const Target *TheTarget = llvm::TargetRegistry::lookupTarget(MArch, TheTriple, Error);
  // Some modules don't specify a triple, and this is okay.
  if (!TheTarget) {
    return nullptr;
  }

  return TheTarget->createTargetMachine(TheTriple.getTriple(), getCPUStr(),
                                        getFeaturesStr(), Options, getRelocModel(),
                                        getCodeModel(),
                                        llvm::CodeGenOpt::Aggressive);
}
#endif

#if LLVM_VERSION_MAJOR <= 11
static bool unifyReturnBlocks(Function &F) {
  std::vector<BasicBlock *> ReturningBlocks;

  for (BasicBlock &I : F)
    if (isa<ReturnInst>(I.getTerminator()))
      ReturningBlocks.push_back(&I);

  if (ReturningBlocks.size() <= 1)
    return false;

  // Insert a new basic block into the function, add PHI nodes (if the function
  // returns values), and convert all of the return instructions into
  // unconditional branches.
  BasicBlock *NewRetBlock = BasicBlock::Create(F.getContext(),
                                               "UnifiedReturnBlock", &F);

  PHINode *PN = nullptr;
  if (F.getReturnType()->isVoidTy()) {
    ReturnInst::Create(F.getContext(), nullptr, NewRetBlock);
  } else {
    // If the function doesn't return void... add a PHI node to the block...
    PN = PHINode::Create(F.getReturnType(), ReturningBlocks.size(),
                         "UnifiedRetVal");
    NewRetBlock->getInstList().push_back(PN);
    ReturnInst::Create(F.getContext(), PN, NewRetBlock);
  }

  // Loop over all of the blocks, replacing the return instruction with an
  // unconditional branch.
  for (BasicBlock *BB : ReturningBlocks) {
    // Add an incoming element to the PHI node for every return instruction that
    // is merging into this new block...
    if (PN)
      PN->addIncoming(BB->getTerminator()->getOperand(0), BB);

    BB->getInstList().pop_back();  // Remove the return insn
    BranchInst::Create(NewRetBlock, BB);
  }

  return true;
}

static bool unifyUnreachableBlocks(Function &F) {
  std::vector<BasicBlock *> UnreachableBlocks;

  for (BasicBlock &I : F)
    if (isa<UnreachableInst>(I.getTerminator()))
      UnreachableBlocks.push_back(&I);

  if (UnreachableBlocks.size() <= 1)
    return false;

  BasicBlock *UnreachableBlock =
      BasicBlock::Create(F.getContext(), "UnifiedUnreachableBlock", &F);
  new UnreachableInst(F.getContext(), UnreachableBlock);

  for (BasicBlock *BB : UnreachableBlocks) {
    BB->getInstList().pop_back(); // Remove the unreachable inst.
    BranchInst::Create(UnreachableBlock, BB);
  }

  return true;
}

llvm::PreservedAnalyses UnifyFunctionExitNodesPass::run(llvm::Function &F, llvm::FunctionAnalysisManager &AM) {
  bool Changed = false;
  Changed |= unifyUnreachableBlocks(F);
  Changed |= unifyReturnBlocks(F);
  return Changed ? PreservedAnalyses() : llvm::PreservedAnalyses::all();
}

#endif

}
