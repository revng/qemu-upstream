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

#include <llvm/Support/Error.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/ModuleSlotTracker.h>

inline llvm::Error mkError(const llvm::StringRef Msg)
{
    return llvm::createStringError(llvm::inconvertibleErrorCode(), Msg);
}

// TODO: Usage of mkError and dbgs() for serializing Values is __really__ slow,
// and should only occur for error reporting.  Wrap these in a class with a
// ModuleSlotTracker.
inline llvm::Error mkError(const llvm::StringRef Msg, const llvm::Value *V)
{
    std::string Str;
    llvm::raw_string_ostream Stream(Str);
    Stream << Msg;
    Stream << *V;
    Stream.flush();
    return llvm::createStringError(llvm::inconvertibleErrorCode(), Str);
}
