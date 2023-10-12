#include "IdMapPass.h"
#include "llvm-compat.h"
#include <utils/Common.h>

#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"

#include "PseudoInst.h"
#include "backend/TcgType.h"

using namespace llvm;

PreservedAnalyses IdMapPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  for (auto &F : M) {

    SmallVector<Instruction *> InstToErase;

    for (auto &I : instructions(F)) {
      if (auto *ZExt = dyn_cast<ZExtInst>(&I)) {
        auto *IntTy0 = dyn_cast<IntegerType>(ZExt->getOperand(0)->getType());
        auto *IntTy1 = dyn_cast<IntegerType>(ZExt->getType());
        if (IntTy0 and IntTy1) {
          uint32_t LlvmSize0 = IntTy0->getBitWidth();
          uint32_t LlvmSize1 = IntTy1->getBitWidth();

          if (LlvmSize0 == 1) {
            if (auto *ICmp = dyn_cast<ICmpInst>(ZExt->getOperand(0))) {
              auto *ICmpOp = ICmp->getOperand(0);
              LlvmSize0 = cast<IntegerType>(ICmpOp->getType())->getBitWidth();
            }
          }

          uint32_t TcgSize0 = llvmToTcgSize(LlvmSize0);
          uint32_t TcgSize1 = llvmToTcgSize(LlvmSize1);

          if (TcgSize0 == TcgSize1) {
            FunctionCallee Fn = pseudoInstFunction(M, IdentityMap, IntTy1, {IntTy0});
            IRBuilder<> Builder(&I);
            CallInst *Call = Builder.CreateCall(Fn, {ZExt->getOperand(0)}); 
            ZExt->replaceAllUsesWith(Call);
            InstToErase.push_back(&I);
          }
        }
      }
    }

    for (auto *I : InstToErase) {
      I->eraseFromParent();
    }
  }

  return PreservedAnalyses::none();
}
