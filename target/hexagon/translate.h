/*
 *  Copyright(c) 2019-2021 Qualcomm Innovation Center, Inc. All Rights Reserved.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef HEXAGON_TRANSLATE_H
#define HEXAGON_TRANSLATE_H

#include "qemu/bitmap.h"
#include "cpu.h"
#include "exec/translator.h"
#include "tcg/tcg-op.h"
#include "internal.h"

typedef struct DisasContext {
    DisasContextBase base;
    uint32_t mem_idx;
    int reg_log[REG_WRITES_MAX];
    int reg_log_idx;
    DECLARE_BITMAP(regs_written, TOTAL_PER_THREAD_REGS);
    int preg_log[PRED_WRITES_MAX];
    int preg_log_idx;
    uint8_t store_width[STORES_MAX];
    uint8_t s1_store_processed;
    uint32_t npc;
} DisasContext;

static inline void ctx_log_reg_write(DisasContext *ctx, int rnum)
{
#if HEX_DEBUG
    if (test_bit(rnum, ctx->regs_written)) {
        HEX_DEBUG_LOG("WARNING: Multiple writes to r%d\n", rnum);
    }
#endif
    ctx->reg_log[ctx->reg_log_idx] = rnum;
    ctx->reg_log_idx++;
    set_bit(rnum, ctx->regs_written);
}

static inline void ctx_log_reg_write_pair(DisasContext *ctx, int rnum)
{
    ctx_log_reg_write(ctx, rnum);
    ctx_log_reg_write(ctx, rnum + 1);
}

static inline void ctx_log_pred_write(DisasContext *ctx, int pnum)
{
    ctx->preg_log[ctx->preg_log_idx] = pnum;
    ctx->preg_log_idx++;
}

static inline bool is_preloaded(DisasContext *ctx, int num)
{
    return test_bit(num, ctx->regs_written);
}

extern TCGv hex_gpr[TOTAL_PER_THREAD_REGS];
extern TCGv hex_pred[NUM_PREGS];
extern TCGv hex_next_PC;
extern TCGv hex_this_PC;
extern TCGv hex_slot_cancelled;
extern TCGv hex_branch_taken;
extern TCGv hex_new_value[TOTAL_PER_THREAD_REGS];
extern TCGv hex_reg_written[TOTAL_PER_THREAD_REGS];
extern TCGv hex_new_pred_value[NUM_PREGS];
extern TCGv hex_pred_written;
extern TCGv hex_store_addr[STORES_MAX];
extern TCGv hex_store_width[STORES_MAX];
extern TCGv hex_store_val32[STORES_MAX];
extern TCGv_i64 hex_store_val64[STORES_MAX];
extern TCGv hex_dczero_addr;
extern TCGv hex_llsc_addr;
extern TCGv hex_llsc_val;
extern TCGv_i64 hex_llsc_val_i64;

extern void gen_exception(int excp);
extern void gen_exception_debug(void);

extern void process_store(DisasContext *ctx, int slot_num);
#endif
