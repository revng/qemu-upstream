//
//  Copyright(c) 2024-2025 rev.ng Labs Srl. All Rights Reserved.
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

#include <llvm/ADT/StringRef.h>
#include <llvm/Demangle/Demangle.h>

inline std::string getDemangleFunctionName(llvm::StringRef MangledName) {
    std::string DemangledName = llvm::demangle(MangledName.str());
    if (MangledName != DemangledName) {
        // If the function name changed when trying to demangle the name,
        // the name was mangled.  The resulting demangled name might look
        // something like
        //
        //   namespace::subnamespace::function(...)
        //
        // extract the function name, this assumes 0 name collisions in
        // the output.
        size_t Index = 0;
        // Remove namespaces
        Index = DemangledName.find_last_of(':');
        if (Index != std::string::npos) {
            DemangledName = DemangledName.substr(Index + 1);
        }
        // Remove arguments
        Index = DemangledName.find_first_of('(');
        if (Index != std::string::npos) {
            DemangledName = DemangledName.substr(0, Index);
        }
    }
    return DemangledName;
}
