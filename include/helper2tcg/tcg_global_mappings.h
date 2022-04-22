#ifndef TCG_GLOBAL_MAP_H
#define TCG_GLOBAL_MAP_H

#include "qemu/osdep.h"
#include "cpu.h"
#include "tcg/tcg.h"
#include "tcg/tcg-op.h"

typedef struct cpu_tcg_mapping {
    const char *tcg_var_name;
    void *tcg_var_base_address;

    const char * const *cpu_var_names;
    size_t cpu_var_base_offset;
    size_t cpu_var_size;
    size_t cpu_var_stride;

    size_t number_of_elements;
} cpu_tcg_mapping;

#define structsizeof(S, member) \
    sizeof(((S*)0)->member)

#define STRUCT_ARRAY_SIZE(S, array) \
    (structsizeof(S, array)/structsizeof(S, array[0]))

/*
 * Following are a few macros that aid in constructing
 * `cpu_tcg_mapping`s for a few common cases.
 */

/* Map between single CPU register and to TCG global */
#define cpu_tcg_map(tcg_var, cpu_var, name_str)                 \
    (cpu_tcg_mapping) {                                         \
        .tcg_var_name = stringify(tcg_var),                     \
        .tcg_var_base_address = &tcg_var,                       \
        .cpu_var_names = (const char *[]) {name_str},           \
        .cpu_var_base_offset = offsetof(CPUArchState, cpu_var), \
        .cpu_var_size = structsizeof(CPUArchState, cpu_var),    \
        .cpu_var_stride = 0,                                    \
        .number_of_elements = 1,                                \
    }

/* Map between array of CPU registers and array of TCG globals. */
#define cpu_tcg_map_array(tcg_var, cpu_var, names)                      \
    (cpu_tcg_mapping) {                                                 \
        .tcg_var_name = #tcg_var,                                       \
        .tcg_var_base_address = tcg_var,                                \
        .cpu_var_names = names,                                         \
        .cpu_var_base_offset = offsetof(CPUArchState, cpu_var),         \
        .cpu_var_size = structsizeof(CPUArchState, cpu_var[0]),         \
        .cpu_var_stride = structsizeof(CPUArchState, cpu_var[0]),       \
        .number_of_elements = STRUCT_ARRAY_SIZE(CPUArchState, cpu_var), \
    }

/*
 * Map between single member in an array of structs to an array
 * of TCG globals, e.g. maps
 *
 *     cpu_state.array_of_structs[i].member
 *
 * to
 *
 *     tcg_global_member[i]
 */
#define cpu_tcg_map_array_of_structs(tcg_var, cpu_struct, cpu_var, names)     \
    (cpu_tcg_mapping) {                                                       \
        .tcg_var_name = #tcg_var,                                             \
        .tcg_var_base_address = tcg_var,                                      \
        .cpu_var_names = names,                                               \
        .cpu_var_base_offset = offsetof(CPUArchState, cpu_struct[0].cpu_var), \
        .cpu_var_size = structsizeof(CPUArchState, cpu_struct[0].cpu_var),    \
        .cpu_var_stride = structsizeof(CPUArchState, cpu_struct[0]),          \
        .number_of_elements = STRUCT_ARRAY_SIZE(CPUArchState, cpu_struct),    \
    }

extern cpu_tcg_mapping tcg_global_mappings[];
extern size_t tcg_global_mapping_count;

static inline void init_cpu_tcg_mappings(cpu_tcg_mapping *mappings, size_t size)
{
    /*
     * Paranoid assertion, this should always hold since
     * they're typedef'd to pointers. But you never know!
     */
    assert(sizeof(TCGv_i32) == sizeof(TCGv_i64) &&
           sizeof(TCGv_i32) == sizeof(TCGv));

    /*
     * Loop over entries in tcg_global_mappings and
     * create the `mapped to` TCGv's.
     */
    for (int i = 0; i < size; ++i) {
        cpu_tcg_mapping m = mappings[i];

        for (int j = 0; j < m.number_of_elements; ++j) {
            /*
             * Here we are using the fact that
             * sizeof(TCGv_i32) == sizeof(TCGv_i64) == sizeof(TCGv)
             */
            uintptr_t tcg_addr = (uintptr_t) m.tcg_var_base_address + j*sizeof(TCGv);

            size_t cpu_offset = m.cpu_var_base_offset + j*m.cpu_var_stride;

            const char *name = m.cpu_var_names[j];

            if (m.cpu_var_size < 8) {
                *(TCGv *) tcg_addr =
                    tcg_global_mem_new(tcg_env, cpu_offset, name);
            } else {
                *(TCGv_i64 *) tcg_addr =
                    tcg_global_mem_new_i64(tcg_env, cpu_offset, name);
            }
        }
    }
}

#endif /* TCG_GLOBAL_MAP_H */
