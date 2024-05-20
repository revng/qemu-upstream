#pragma once

#include <stdint.h>

#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/ArrayRef.h"

// Pseudo instructions refers to extra LLVM instructions implemented as
// calls to undefined functions.  They are useful for amending LLVM IR to
// simplify mapping to TCG in the backend, e.g.
//
//   %2 = call i32 @IdentityMap.i32.i16(i16 %1)
//
// is a pseudo opcode used to communicate that %1 and %2 should be mapped
// to the same value in TCG.

enum PseudoInstArg {
    ArgInt,
    ArgVec,
    ArgPtr,
    ArgLabel,
    ArgVoid,
};

#define PSEUDO_INST_ARGVEC(...) \
    {__VA_ARGS__}

#define PSEUDO_INST_DEF(name, ret, args) name
enum PseudoInst : uint8_t {
#include "PseudoInst.inc"
};
#undef PSEUDO_INST_DEF

// Maps PseudoInst + return/argument types to a FunctionCallee that can be
// called.
llvm::FunctionCallee pseudoInstFunction(llvm::Module &M,
                                        PseudoInst Inst,
                                        llvm::Type *RetType,
                                        llvm::ArrayRef<llvm::Type *> ArgTypes);

// Reverse mapping of above, takes a call instruction and attempts to map the
// callee to a PseudoInst.
llvm::Expected<PseudoInst> getPseudoInstFromCall(llvm::CallInst *Call);
