#include <stdint.h>
#include "tcg-global-mappings.h"

typedef struct SpecialData {
    uint32_t a;
    uint32_t unmapped_field;
} SpecialData;

typedef struct CPUArchState {
    uint32_t regs[32];
    uint32_t unmapped_field;
    SpecialData data[8];
    uint32_t mapped_field;
} CPUArchState;

/* Dummy struct, in QEMU this would correspond to TCGv_i32 in tcg.h */
typedef struct TCGv_i32 {} TCGv_i32;
/* Global TCGv's representing CPU state */
TCGv_i32 tcg_regs[32];
TCGv_i32 tcg_a[8];
TCGv_i32 tcg_field;

cpu_tcg_mapping mappings[] = {
    CPU_TCG_MAP_ARRAY(CPUArchState, tcg_regs, regs, NULL),
    CPU_TCG_MAP_ARRAY_OF_STRUCTS(CPUArchState, tcg_a, data, a, NULL),
    CPU_TCG_MAP(CPUArchState, tcg_field, mapped_field, NULL),
};

__attribute__((annotate ("immediate: 1")))
uint32_t helper_reg(CPUArchState *env, uint32_t i) {
    return env->regs[i];
}

__attribute__((annotate ("immediate: 1")))
uint32_t helper_data_a(CPUArchState *env, uint32_t i) {
    return env->data[i].a;
}

uint32_t helper_single_mapped(CPUArchState *env) {
    return env->mapped_field;
}

uint32_t helper_unmapped(CPUArchState *env) {
    return env->unmapped_field;
}
