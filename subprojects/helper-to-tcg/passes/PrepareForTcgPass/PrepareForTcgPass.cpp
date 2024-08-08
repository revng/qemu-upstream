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

#include <CmdLineOptions.h>
#include <PrepareForTcgPass.h>
#include "TransformGEPs.h"
#include <llvm/ADT/SCCIterator.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/Transforms/Utils/Local.h>

using namespace llvm;

static void removeFunctionsWithLoops(Module &M, ModuleAnalysisManager &MAM)
{
    // Iterate over all Strongly Connected Components (SCCs), a SCC implies
    // the existence of loops if:
    //   - it has more than one node, or;
    //   - it has a self-edge.
    SmallVector<Function *, 16> FunctionsToRemove;
    for (Function &F : M) {
        if (F.isDeclaration()) {
            continue;
        }
        for (auto It = scc_begin(&F); !It.isAtEnd(); ++It) {
#if LLVM_VERSION_MAJOR > 10
            if (It.hasCycle()) {
#else
            if (It.hasLoop()) {
#endif
                FunctionsToRemove.push_back(&F);
                break;
            }
        }
    }

    for (auto *F : FunctionsToRemove) {
        F->deleteBody();
    }
}

inline void demotePhis(Function &F)
{
    if (F.isDeclaration()) {
        return;
    }

    SmallVector<PHINode *, 10> Phis;
    for (auto &I : instructions(F)) {
        if (auto *Phi = dyn_cast<PHINode>(&I)) {
            Phis.push_back(Phi);
        }
    }

    for (auto *Phi : Phis) {
        DemotePHIToStack(Phi);
    }
}

static void collectTcgGlobals(Module &M, TcgGlobalMap &ResultTcgGlobalMap)
{
    auto *Map = M.getGlobalVariable(TcgGlobalMappingsName);
    if (!Map) {
        return;
    }

    // In case the `tcg_global_mappings` array is empty,
    // casting to `ConstantArray` will fail, even though it's a
    // `[0 x %struct.cpu_tcg_mapping]`.
    auto *MapElems = dyn_cast<ConstantArray>(Map->getOperand(0));
    if (!MapElems) {
        return;
    }

    for (auto Row : MapElems->operand_values()) {
        auto *ConstRow = cast<ConstantStruct>(Row);

        // Get code string
        auto *CodePtr = ConstRow->getOperand(0);
        auto CodeStr =
            cast<ConstantDataArray>(
                cast<Constant>(CodePtr->getOperand(0))->getOperand(0))
                ->getAsString();
        CodeStr = CodeStr.rtrim('\0');

        // Get offset in cpu env
        auto *Offset = cast<ConstantInt>(ConstRow->getOperand(3));
        // Get size of variable in cpu env
        auto *SizeInBytes = cast<ConstantInt>(ConstRow->getOperand(4));
        unsigned SizeInBits = 8 * SizeInBytes->getLimitedValue();

        auto *Stride = cast<ConstantInt>(ConstRow->getOperand(5));
        auto *NumElements = cast<ConstantInt>(ConstRow->getOperand(6));

        ResultTcgGlobalMap[Offset->getLimitedValue()] = {
            CodeStr, SizeInBits, NumElements->getLimitedValue(),
            Stride->getLimitedValue()};
    }
}

PreservedAnalyses PrepareForTcgPass::run(Module &M, ModuleAnalysisManager &MAM)
{
    removeFunctionsWithLoops(M, MAM);
    for (Function &F : M) {
        demotePhis(F);
    }
    collectTcgGlobals(M, ResultTcgGlobalMap);
    for (Function &F : M) {
        transformGEPs(M, F, ResultTcgGlobalMap);
    }
    return PreservedAnalyses::none();
}
