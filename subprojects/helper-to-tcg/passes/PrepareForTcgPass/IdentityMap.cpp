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

#include "IdentityMap.h"
#include <PseudoInst.h>
#include "backend/TcgType.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Value.h>

using namespace llvm;

void identityMap(Module &M, Function &F)
{
    SmallVector<Instruction *, 8> InstToErase;

    for (auto &I : instructions(F)) {
        auto *ZExt = dyn_cast<ZExtInst>(&I);
        if (ZExt) {
            auto *IntTy0 =
                dyn_cast<IntegerType>(ZExt->getOperand(0)->getType());
            auto *IntTy1 = dyn_cast<IntegerType>(ZExt->getType());
            if (IntTy0 and IntTy1) {
                uint32_t LlvmSize0 = IntTy0->getBitWidth();
                uint32_t LlvmSize1 = IntTy1->getBitWidth();

                if (LlvmSize0 == 1) {
                    auto *ICmp = dyn_cast<ICmpInst>(ZExt->getOperand(0));
                    if (ICmp) {
                        auto *ICmpOp = ICmp->getOperand(0);
                        LlvmSize0 =
                            cast<IntegerType>(ICmpOp->getType())->getBitWidth();
                    }
                }

                uint32_t TcgSize0 = llvmToTcgSize(LlvmSize0);
                uint32_t TcgSize1 = llvmToTcgSize(LlvmSize1);

                if (TcgSize0 == TcgSize1) {
                    FunctionCallee Fn =
                        pseudoInstFunction(M, IdentityMap, IntTy1, {IntTy0});
                    IRBuilder<> Builder(&I);
                    CallInst *Call =
                        Builder.CreateCall(Fn, {ZExt->getOperand(0)});
                    ZExt->replaceAllUsesWith(Call);
                    InstToErase.push_back(&I);
                }
            }
        } else if (isa<FreezeInst>(&I)) {
            auto *IntTy0 = dyn_cast<IntegerType>(I.getOperand(0)->getType());
            auto *IntTy1 = dyn_cast<IntegerType>(I.getType());
            FunctionCallee Fn =
                pseudoInstFunction(M, IdentityMap, IntTy1, {IntTy0});
            IRBuilder<> Builder(&I);
            CallInst *Call = Builder.CreateCall(Fn, {I.getOperand(0)});
            I.replaceAllUsesWith(Call);
            InstToErase.push_back(&I);
        }
    }

    for (auto *I : InstToErase) {
        I->eraseFromParent();
    }
}
