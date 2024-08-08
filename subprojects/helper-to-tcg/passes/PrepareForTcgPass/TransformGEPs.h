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

#include "TcgGlobalMap.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

//
// Transform of module that converts getelementptr (GEP) operators to
// pseudo instructions:
//   - call @AccessGlobalArray(OffsetInEnv, Index)
//     if OffsetInEnv is mapped to a global TCGv array.
//
//   - call @AccessGlobalValue(OffsetInEnv)
//     if OffsetInEnv is mapped to a global TCGv value.
//
//   - pointer math, if above fails.
//

void transformGEPs(llvm::Module &M, llvm::Function &F,
                   const TcgGlobalMap &TcgGlobals);
