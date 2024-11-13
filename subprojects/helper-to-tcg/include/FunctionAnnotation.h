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

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>
#include <stdint.h>

namespace llvm
{
class Function;
}

// Different kind of function annotations which control the behaviour
// of helper-to-tcg.
enum class AnnotationKind : uint8_t {
    // Function should be translated
    HelperToTcg,
    // Declares a list of arguments as immediates
    Immediate,
    // Declares a list of arguments as vectors, represented by offsets into
    // the CPU state
    PtrToOffset,
};

// Annotation data which may be attached to a function
struct Annotation {
    // Indices of function arguments the annotation applies to, only
    // used for AnnotationKind::[Immediate|PtrToOffset].
    llvm::SmallVector<uint8_t, 4> ArgIndices;
    AnnotationKind Kind;
};

// Map from Function * to a list of struct Annotation.  std::map is used here
// which allocates for each mapped pair due to the value being large
// (at least 48*3 bits).  If ArgIndices were to be stored out-of-band this could
// be reduced, and DenseMap would be more appropriate.
using AnnotationVectorTy = llvm::SmallVector<Annotation, 3>;
using AnnotationMapTy = llvm::DenseMap<llvm::Function *, AnnotationVectorTy>;
