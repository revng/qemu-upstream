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

#include "PseudoInst.h"
#include "llvm-compat.h"

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Twine.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>

using namespace llvm;

#define PSEUDO_INST_DEF(name, ret, args) #name
static const char *PseudoInstName[] = {
#include "PseudoInst.inc"
};
#undef PSEUDO_INST_DEF

#define PSEUDO_INST_ARGVEC(...)                                                \
    (sizeof((PseudoInstArg[]){__VA_ARGS__}) / sizeof(PseudoInstArg))

#define PSEUDO_INST_DEF(name, ret, args) args
static uint8_t PseudoInstArgCount[] = {
#include "PseudoInst.inc"
};
#undef PSEUDO_INST_DEF

// In order to map from a Function * to a PseudoInst, we keep a map
// of all Functions created, this simplifies mapping of callee's to
// a PseudoInst that can be switched over.
static DenseMap<Function *, PseudoInst> MapFuncToInst;

// Converts llvm `Type`s to a string representation
// that can be embedded in function names for basic overloading.
//
// Ex.
//
//      *i32 -> "pi32"
//      [8 x i8] -> "a8xi8"
//      <128 x i8> -> "v128xi8"
//
// LLVM has an implementation of a similar function used by intrinsics,
// called getMangledTypeStr, but it's not exposed.
inline std::string getMangledTypeStr(Type *Ty)
{
    std::string TypeStr = "";
    llvm::raw_string_ostream TypeStream(TypeStr);
    switch (Ty->getTypeID()) {
    case Type::ArrayTyID: {
        auto *ArrayTy = cast<ArrayType>(Ty);
        std::string ElementStr = getMangledTypeStr(ArrayTy->getElementType());
        TypeStream << "a" << ArrayTy->getNumElements() << "x" << ElementStr;
    } break;
#if LLVM_VERSION_MAJOR >= 11
    case Type::FixedVectorTyID: {
#else
    case Type::VectorTyID: {
#endif
        auto *VecTy = cast<VectorType>(Ty);
        uint32_t ElementCount = compat::getVectorElementCount(VecTy);
        std::string ElementStr = getMangledTypeStr(VecTy->getElementType());
        TypeStream << "v" << ElementCount << "x" << ElementStr;
    } break;
    case Type::StructTyID: {
        auto *StructTy = cast<StructType>(Ty);
        TypeStream << StructTy->getName();
    } break;
    case Type::IntegerTyID: {
        auto *IntTy = cast<IntegerType>(Ty);
        TypeStream << "i" << IntTy->getBitWidth();
    } break;
    case Type::PointerTyID: {
        auto *PtrTy = cast<PointerType>(Ty);
        std::string ElementStr =
            getMangledTypeStr(PtrTy->getPointerElementType());
        TypeStream << "p" << ElementStr;
    } break;
    default:
        abort();
    }

    return TypeStream.str();
}

const char *pseudoInstName(PseudoInst Inst) { return PseudoInstName[Inst]; }

uint8_t pseudoInstArgCount(PseudoInst Inst) { return PseudoInstArgCount[Inst]; }

llvm::FunctionCallee pseudoInstFunction(llvm::Module &M, PseudoInst Inst,
                                        llvm::Type *RetType,
                                        llvm::ArrayRef<llvm::Type *> ArgTypes)
{
    auto *FT = llvm::FunctionType::get(RetType, ArgTypes, false);

    std::string FnName{PseudoInstName[Inst]};
    if (!RetType->isVoidTy()) {
        FnName += ".";
        FnName += getMangledTypeStr(RetType);
    }
    for (llvm::Type *Ty : ArgTypes) {
        if (Ty->isLabelTy()) {
            continue;
        }
        FnName += ".";
        FnName += getMangledTypeStr(Ty);
    }

    llvm::FunctionCallee Fn = M.getOrInsertFunction(FnName, FT);
    auto *F = cast<Function>(Fn.getCallee());
    MapFuncToInst.insert({F, Inst});

    return Fn;
}

// Takes value as convenience
PseudoInst getPseudoInstFromCall(const CallInst *Call)
{
    Function *F = Call->getCalledFunction();

    auto It = MapFuncToInst.find(F);
    if (It == MapFuncToInst.end()) {
        return InvalidPseudoInst;
    }

    return It->second;
}
