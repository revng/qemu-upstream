//
//  Copyright(c) 2026 rev.ng Labs Srl. All Rights Reserved.
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

#include "PrepareForOptPass.hpp"

#include <cstdlib>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/StringSet.h>
#include <llvm/Demangle/Demangle.h>
#include <llvm/Support/Debug.h>

#define DEBUG_TYPE "prepare-for-opt"

using namespace llvm;

static void demangleFunctionNames(Module &M) {
    ItaniumPartialDemangler Demangler;
    StringSet DemangledNames;
    for (Function &F : M) {
        if (Demangler.partialDemangle(F.getName().str().c_str())) {
            // partialDemangle() returns true on failure,
            // name not mangled so skip.
            continue;
        }

        char *BaseName = Demangler.getFunctionBaseName(nullptr, nullptr);
        if (!BaseName) {
            continue;
        }

        // If another function had the same basename we just accept and
        // use the mangled name.
        if (DemangledNames.contains(BaseName)) {
            free(BaseName);
            continue;
        }
        DemangledNames.insert(BaseName);

        // Overwrite function name by its base name, this gives us better
        // naming in generated code without having to carry around a separate
        // Function -> StringRef mapping for names.
        F.setName(BaseName);

        free(BaseName);
    }
}

PreservedAnalyses PrepareForOptPass::run(Module &M,
                                         ModuleAnalysisManager &MAM) {
    demangleFunctionNames(M);
    return PreservedAnalyses::none();
}
