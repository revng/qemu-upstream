/*
 * QEMU TCG JIT using LLVM
 *
 * Copyright (C) 2020, 2021, Zhang Boyang <zhangboyang.id@gmail.com>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */

/*
 * This file is a placeholder. It forces build system to link our program with
 * a C++ linker, which is required by LLVM (even we are using C interface).
 * Moreover, At some day we may have to write some C++ code eventually.
 */

#include <llvm-c/Core.h>
#include <llvm/IR/Instructions.h>
using namespace llvm;

extern "C"
void QLLVMSetMustTailCall(LLVMValueRef Call, LLVMBool isMustTailCall) {
    unwrap<CallInst>(Call)->setTailCallKind(
        isMustTailCall ? CallInst::TCK_MustTail : CallInst::TCK_None);
}
