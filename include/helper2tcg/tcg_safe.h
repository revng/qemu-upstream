#pragma once

#include "tcg/tcg-op.h"

static inline void tcg_gen_shli_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, int32_t arg2)
{
    if (arg2 >= 0 && arg2 < 32) {
      tcg_gen_shli_i32(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_shri_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, int32_t arg2)
{
    if (arg2 >= 0 && arg2 < 32) {
      tcg_gen_shri_i32(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i32(ret, 0);
    }
}


static inline void tcg_gen_sari_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, int32_t arg2)
{
    if (arg2 >= 0 && arg2 < 32) {
      tcg_gen_sari_i32(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_rotri_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, int32_t arg2)
{
    if (arg2 >= 0 && arg2 < 32) {
      tcg_gen_rotri_i32(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_rotli_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, int32_t arg2)
{
    if (arg2 >= 0 && arg2 < 32) {
      tcg_gen_rotli_i32(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_shli_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, int64_t arg2)
{
    if (arg2 >= 0 && arg2 < 64) {
      tcg_gen_shli_i64(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_shri_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, int64_t arg2)
{
    if (arg2 >= 0 && arg2 < 64) {
      tcg_gen_shri_i64(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i64(ret, 0);
    }
}


static inline void tcg_gen_sari_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, int64_t arg2)
{
    if (arg2 >= 0 && arg2 < 64) {
      tcg_gen_sari_i64(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_rotri_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, int64_t arg2)
{
    if (arg2 >= 0 && arg2 < 64) {
      tcg_gen_rotri_i64(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_rotli_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, int64_t arg2)
{
    if (arg2 >= 0 && arg2 < 64) {
      tcg_gen_rotli_i64(ret, arg1, arg2);
    } else {
      tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_extract_i64_safe(TCGv_i64 ret, TCGv_i64 arg,
                                            unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_extract_i64(ret, arg, ofs, len);
    } else {
        tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_sextract_i64_safe(TCGv_i64 ret, TCGv_i64 arg,
                                             unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_sextract_i64(ret, arg, ofs, len);
    } else {
        tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_extract_i32_safe(TCGv_i32 ret, TCGv_i32 arg,
                                            unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_extract_i32(ret, arg, ofs, len);
    } else {
        tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_sextract_i32_safe(TCGv_i32 ret, TCGv_i32 arg,
                                             unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_sextract_i32(ret, arg, ofs, len);
    } else {
        tcg_gen_movi_i32(ret, 0);
    }
}

static inline void tcg_gen_deposit_i64_safe(TCGv_i64 ret, TCGv_i64 arg1, TCGv_i64 arg2,
                                            unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_deposit_i64(ret, arg1, arg2, ofs, len);
    } else {
        tcg_gen_movi_i64(ret, 0);
    }
}

static inline void tcg_gen_deposit_i32_safe(TCGv_i32 ret, TCGv_i32 arg1, TCGv_i32 arg2,
                                            unsigned int ofs, unsigned int len)
{
    if (len > 0) {
        tcg_gen_deposit_i32(ret, arg1, arg2, ofs, len);
    } else {
        tcg_gen_movi_i32(ret, 0);
    }
}
