# helper-to-tcg

`helper-to-tcg` is a standalone LLVM IR to TCG translator, with the goal of simplifying the implementation of complicated instructions in TCG. Instruction semantics can be specified either directly in LLVM IR or any language that can be compiled to it (C, C++, ...). However, the tool is tailored towards QEMU helper functions written in C.

Internally, `helper-to-tcg` consists of a mix of custom and built-in transformation and analysis passes that are applied to the input LLVM IR sequentially. The pipeline of passes is laid out as follows
```
           +---------------+    +-----+    +---------------+    +------------+
LLVM IR -> | PrepareForOpt | -> | -Os | -> | PrepareForTcg | -> | TcgGenPass | -> TCG
           +---------------+    +-----+    +---------------+    +------------+
```
where the custom passes performs:
* `PrepareForOpt` - Early culling of unneeded functions, mapping of function annotations, removal of `noinline` added by `-O0`
* `PrepareForTcg` - Post-optimization pass that tries to get the IR as close to Tinycode as possible, goal is to take complexity away from the backend;
* `TcgGenPass` - Backend pass that allocates TCG variables to LLVM values, and emits final TCG C code.

As for LLVM optimization, `-Os` strikes a good balance between unrolling and vectorization, from testing. More aggressive optimization levels would often unroll loops over compacting it with loop vectorization.

## Project Structure

* `get-llvm-ir.py` - Helper script to convert a QEMU .c file to LLVM IR by getting compile flags from `compile_commands.json`.
* `pipeline` - Implementation of pipeline orchestrating LLVM passes and handling input.
* `passes` - Implementation of custom LLVM passes (`PrepareForOpt`,`PrepareForTcg`,`TcgGenPass`).
* `include` - Shared headers between `passes/pipeline`.
* `tests` - Simple end-to-end tests of C functions we expect to be able to translate, tests fail if any function fails to translate, output is not verified.

## Example Translations

`helper-to-tcg` is able to deal with a wide variety of helper functions, the following code snippet contains two examples from the Hexagon architecture implementing the semantics of a predicated and instruction (`A2_pandt`) and a vectorized signed saturated 2-element scalar product (`V6_vdmpyhvsat`).

```c
int32_t HELPER(A2_pandt)(CPUHexagonState *env, int32_t RdV,
                         int32_t PuV, int32_t RsV, int32_t RtV)
{
    if(fLSBOLD(PuV)) {
        RdV=RsV&RtV;
    } else {
        CANCEL;
    }
    return RdV;
}

void HELPER(V6_vdmpyhvsat)(CPUHexagonState *env,
                           void * restrict VdV_void,
                           void * restrict VuV_void,
                           void * restrict VvV_void)
{
    fVFOREACH(32, i) {
        size8s_t accum = fMPY16SS(fGETHALF(0,VuV.w[i]),fGETHALF(0, VvV.w[i]));
        accum += fMPY16SS(fGETHALF(1,VuV.w[i]),fGETHALF(1, VvV.w[i]));
        VdV.w[i] = fVSATW(accum);
    }
}
```
For the above snippet, `helper-to-tcg` produces the following TCG
```c
void emit_A2_pandt(TCGv_i32 temp0, TCGv_env env, TCGv_i32 temp4,
                   TCGv_i32 temp8, TCGv_i32 temp7, TCGv_i32 temp6) {
    TCGv_i32 temp2 = tcg_temp_new_i32();
    tcg_gen_andi_i32(temp2, temp8, 1);
    TCGv_i32 temp5 = tcg_temp_new_i32();
    tcg_gen_and_i32(temp5, temp6, temp7);
    tcg_gen_movcond_i32(TCG_COND_EQ, temp0, temp2, tcg_constant_i32(0), temp4, temp5);
}

void emit_V6_vdmpyhvsat(TCGv_env env, intptr_t vec3,
                        intptr_t vec7, intptr_t vec6) {
     VectorMem mem = {0};
     intptr_t vec0 = temp_new_gvec(&mem, 128);
     tcg_gen_gvec_shli(MO_32, vec0, vec7, 16, 128, 128);
     intptr_t vec5 = temp_new_gvec(&mem, 128);
     tcg_gen_gvec_sari(MO_32, vec5, vec0, 16, 128, 128);
     intptr_t vec1 = temp_new_gvec(&mem, 128);
     tcg_gen_gvec_shli(MO_32, vec1, vec6, 16, 128, 128);
     tcg_gen_gvec_sari(MO_32, vec1, vec1, 16, 128, 128);
     tcg_gen_gvec_mul(MO_32, vec1, vec1, vec5, 128, 128);
     intptr_t vec2 = temp_new_gvec(&mem, 128);
     tcg_gen_gvec_sari(MO_32, vec2, vec7, 16, 128, 128);
     tcg_gen_gvec_sari(MO_32, vec0, vec6, 16, 128, 128);
     tcg_gen_gvec_mul(MO_32, vec2, vec0, vec2, 128, 128);
     tcg_gen_gvec_ssadd(MO_32, vec3, vec1, vec2, 128, 128);
}
```

In the first case, the predicated and instruction was made branchless by using a conditional move, and in the latter case the inner loop of the vectorized scalar product could be converted to a few vectorized shifts and multiplications, folllowed by a vectorized signed saturated addition.

## Usage

Building `helper-to-tcg` produces a binary implementing the pipeline outlined above, going from LLVM IR to TCG.

### Specifying Functions to Translate

Unless `--translate-all-helpers` is specified, the default behaviour of `helper-to-tcg` is to only translate functions annotated via a special `"helper-to-tcg"` annotation. Functions called by annotated functions will also be translated, see the following example:

```c
// Function will be translated, annotation provided
__attribute__((annotate ("helper-to-tcg")))
int f(int a, int b) {
    return 2 * g(a, b);
}

// Function will be translated, called by annotated `f()` function
int g(int a, int b) {
    ...
}

// Function will not be translated
int h(int a, int b) {
    ...
}
```

### Immediate and Vector Arguments

Function annotations are in some cases used to provide extra information to `helper-to-tcg` not otherwise present in the IR. For example, whether an integer argument should actually be treated as an immediate rather than a register, or if a pointer argument should be treated as a `gvec` vector (offset into `CPUArchState`). For instance:
```c
__attribute__((annotate ("helper-to-tcg")))
__attribute__((annotate ("immediate: 1")))
int f(int a, int i) {
    ...
}

__attribute__((annotate ("helper-to-tcg")))
__attribute__((annotate ("ptr-to-offset: 0, 1")))
void g(void * restrict a, void * restrict b) {
    ...
}
```
where `"immediate: 1"` tells `helper-to-tcg` that the argument with index `1` should be treated as an immediate (multiple arguments are specified through a comma separated list). Similarly `"ptr-to-offset: 0, 1"` indicates that arguments width index 0 and 1 should be treated as offsets from `CPUArchState` (given as `intptr_t`), rather than actual pointer arguments. For the above code, `helper-to-tcg` emits
```c
void emit_f(TCGv_i32 res, TCGv_i32 a, int i) {
    ...
}

void emit_g(intptr_t a, intptr_t b) {
    ...
}
```

### Loads and Stores

Translating loads and stores is slightly trickier, as some QEMU specific assumptions are made. Loads and stores in the input are assumed to go through the `cpu_[st|ld]*()` functions defined in `exec/cpu_ldst.h` that a helper function would use. 

If using standalone input functions (not QEMU helper functions), loads and stores are still represented by `cpu_[st|ld]*()` which needs to be declared, consider:
```c
/* Opaque CPU state type, will be mapped to tcg_env */
struct CPUArchState;
typedef struct CPUArchState CPUArchState;

/* Prototype of QEMU helper guest load/store functions, see exec/cpu_ldst.h */
uint32_t cpu_ldub_data(CPUArchState *, uint32_t ptr);
void cpu_stb_data(CPUArchState *, uint32_t ptr, uint32_t data);

uint32_t helper_ld8(CPUArchState *env, uint32_t addr) {
    return cpu_ldub_data(env, addr);
}

void helper_st8(CPUArchState *env, uint32_t addr, uint32_t data) {
    return cpu_stb_data(env, addr, data);
}
```
implementing an 8-bit load and store instruction, these will be translated to the following TCG.
```c
void emit_ld8(TCGv_i32 temp0, TCGv_env env, TCGv_i32 temp1) {
    tcg_gen_qemu_ld_i32(temp0, temp1, tb_mmu_index(tcg_ctx->gen_tb->flags), MO_UB);
}

void emit_st8(TCGv_env env, TCGv_i32 temp0, TCGv_i32 temp1) {
    tcg_gen_qemu_st_i32(temp1, temp0, tb_mmu_index(tcg_ctx->gen_tb->flags), MO_UB);
}
```
Note, the emitted code assumes the definition of a `tb_mmu_index()` function to retrieve the current CPU MMU index, the name of this function can be configured via the `--mmu-index-function` flag.

### Mapping CPU State

In QEMU, commonly accessed fields in the `CPUArchState` are often mapped to global `TCGv*` variables representing that piece of CPU state in TCG. When translating helper functions (or other C functions), a method of specifying which fields in the CPU state should be mapped to which globals is needed. To this end, a declarative approach is taken, where mappings between CPU state and globals can be consumed by both `helper-to-tcg` and runtime QEMU for instantiating the `TCGv` globals themselves.

Users must define this mapping via a global `cpu_tcg_mapping []` array, as can be seen in the following example where `mapped_field` of `CPUArchState` is mapped to the global `tcg_field`. For more complicated examples see the tests in `tests/cpustate.c`.
```c
#include <stdint.h>
#include "tcg/tcg-global-mappings.h"

/* Define a CPU state with some different fields */

typedef struct CPUArchState {
    uint32_t mapped_field;
    uint32_t unmapped_field;
} CPUArchState;

/* Dummy struct, in QEMU this would correspond to TCGv_i32 in tcg.h */
typedef struct TCGv_i32 {} TCGv_i32;

/* Global TCGv representing CPU state */
TCGv_i32 tcg_field;

/*
 * Finally provide a mapping of CPUArchState to TCG globals we care about, here
 * we map mapped_field to tcg_field
 */
cpu_tcg_mapping mappings[] = {
    CPU_TCG_MAP(CPUArchState, tcg_field, mapped_field, NULL),
};

uint32_t helper_mapped(CPUArchState *env) {
    return env->mapped_field;
}

uint32_t helper_unmapped(CPUArchState *env) {
    return env->unmapped_field;
}
```
Note, the name of the `cpu_tcg_mapping[]` is provided via the `--tcg-global-mappings` flag. For the above example, `helper-to-tcg` emits
```c
extern TCGv_i32 tcg_field;

void emit_mapped(TCGv_i32 temp0, TCGv_env env) {
    tcg_gen_mov_i32(temp0, tcg_field);
}

void emit_unmapped(TCGv_i32 temp0, TCGv_env env) {
    TCGv_ptr ptr1 = tcg_temp_new_ptr();
    tcg_gen_addi_ptr(ptr1, env, 128ull);
    tcg_gen_ld_i32(temp0, ptr1, 0);
}
```
where accesses in the input C code are correctly mapped to the corresponding TCG globals. The unmapped `CPUArchState` access turns into pointer math and a load, whereas the mapped access turns into a `mov` from a global.

### Automatic Calling of Generated Code

Finally, calling the generated code is as simple as including the output of `helper-to-tcg` into the project and manually calling `emit_*(...)`. However, when dealing with an existing frontend that has a lot of helper functions already in use, we simplify this process somewhat for non-vector instructions. `helper-to-tcg` can emit a dispatcher, which for the above CPU state mapping example looks like
```c
int helper_to_tcg_dispatcher(void *func, TCGTemp *ret_temp, int nargs, TCGTemp **args) {
    if ((uintptr_t) func == (uintptr_t) helper_mapped) {
        TCGv_i32 temp0 = temp_tcgv_i32(ret_temp);
        TCGv_env env = temp_tcgv_ptr(args[0]);
        emit_mapped(temp0, env);
        return 1;
    }
    if ((uintptr_t) func == (uintptr_t) helper_unmapped) {
        TCGv_i32 temp0 = temp_tcgv_i32(ret_temp);
        TCGv_env env = temp_tcgv_ptr(args[0]);
        emit_unmapped(temp0, env);
        return 1;
    }
    return 0;
}
```
Here `emit_mapped()` and `emit_unmapped()` are automatically called if the current helper function call being translated `void *func` corresponds to either of the input helper functions. If the fronend then defines
```c
#ifdef CONFIG_HELPER_TO_TCG
#define TARGET_HELPER_DISPATCHER helper_to_tcg_dispatcher
#endif
```
in `cpu-param.h`, then calls to `gen_helper_mapped()` for instance, will end up in `emit_mapped()` with no change to frontends. Additionally, dispatching from helper calls allows for easy toggling of `helper-to-tcg`, which is increadibly useful for testing purposes.

### Simple Command Usage

Assume a `helpers.c` file with functions to translate, then to obtain LLVM IR
```bash
$ clang helpers.c -O0 -Xclang -disable-O0-optnone -S -emit-llvm
```
which produces `helpers.ll` to be fed into `helper-to-tcg`
```bash
$ ./helper-to-tcg helpers.ll --translate-all-helpers
```
where `--translate-all-helpers` means "translate all functions starting with helper_*". Finally, the above command produces `helper-to-tcg-emitted.[c|h]` with emitted TCG code.
