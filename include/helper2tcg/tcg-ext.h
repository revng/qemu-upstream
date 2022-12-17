#include "tcg/tcg-op.h"

static inline void tcg_gen_gvec_constant_i32(TCGv_env env, uint32_t dofs, uint32_t *arr, uint32_t maxsz)
{
    const uint32_t element_size = sizeof(arr[0]);
    for (uint32_t i = 0; i < maxsz/element_size; ++i)
    {
        tcg_gen_st_i32(tcg_constant_i32(arr[i]), env, dofs + i*element_size);
    }
}

static inline void tcg_gen_gvec_constant_i16(TCGv_env env, uint32_t dofs, uint16_t *arr, uint32_t maxsz)
{
    const uint32_t element_size = sizeof(arr[0]);
    for (uint32_t i = 0; i < maxsz/element_size; ++i)
    {
        tcg_gen_st16_i32(tcg_constant_i32(arr[i]), env, dofs + i*element_size);
    }
}
