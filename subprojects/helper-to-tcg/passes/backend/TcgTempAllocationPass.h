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

#include "FunctionAnnotation.h"
#include "backend/TcgType.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/Support/Error.h>

//
// TcgTempAllocationPass
//
// Analysis over the IR that performs basic register allocation to assign
// identifiers representing TCGv's to all values in a given function.
//
// Note: Input code is assumed to be loop free, which drastically simplifies
// the register allocation. This assumption is reasonable as we expect code
// with loops to be either unrolled or vectorized, and we currently don't emit
// for loops in C.
//
// This pass also contains the logic for mapping various LLVM values to a TcgV
// struct, which is necessary in order to figure out what type we need for in
// TCG.
//

namespace llvm
{
class Function;
}

enum ArgumentKind {
    ArgTemp,
    ArgImmediate,
    ArgPtrToOffset,
};

struct Arguments {
    llvm::Optional<const llvm::Value *> EnvPtr;
    llvm::DenseMap<const llvm::Value *, ArgumentKind> ArgInfoMap;
    llvm::SmallSet<const llvm::Value *, 8> Args;
};

struct TempAllocationData {
    // Mapping of LLVM Values to the corresponding TcgV
    llvm::DenseMap<const llvm::Value *, TcgV> Map;

    // Whether or not the final mov in an instruction can safely
    // be ignored or not.
    bool SkipReturnMov = false;
    llvm::Optional<TcgV> ReturnValue;
    Arguments Args;

    inline TcgV map(const llvm::Value *V, const TcgV &T)
    {
        return Map.try_emplace(V, T).first->second;
    }
};

llvm::Expected<TempAllocationData>
allocateTemporaries(const llvm::Function &F,
                    const AnnotationMapTy &Annotations);
