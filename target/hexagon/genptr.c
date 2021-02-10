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

#define QEMU_GENERATE
#include "qemu/osdep.h"
#include "qemu/log.h"
#include "cpu.h"
#include "internal.h"
#include "tcg/tcg-op.h"
#include "insn.h"
#include "opcodes.h"
#include "translate.h"
#include "macros.h"
#include "gen_tcg.h"
#include "genptr.h"

TCGv gen_read_reg(TCGv result, int num)
{
    tcg_gen_mov_tl(result, hex_gpr[num]);
    return result;
}

TCGv gen_read_preg(TCGv pred, uint8_t num)
{
    tcg_gen_mov_tl(pred, hex_pred[num]);
    return pred;
}

static inline void gen_log_predicated_reg_write(int rnum, TCGv val, int slot)
{
    TCGv one = tcg_const_tl(1);
    TCGv zero = tcg_const_tl(0);
    TCGv slot_mask = tcg_temp_new();

    tcg_gen_andi_tl(slot_mask, hex_slot_cancelled, 1 << slot);
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_new_value[rnum], slot_mask, zero,
                           val, hex_new_value[rnum]);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_reg_written[rnum], slot_mask, zero,
                       one, hex_reg_written[rnum]);
#endif

    tcg_temp_free(one);
    tcg_temp_free(zero);
    tcg_temp_free(slot_mask);
}

void gen_log_reg_write(int rnum, TCGv val)
{
    tcg_gen_mov_tl(hex_new_value[rnum], val);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movi_tl(hex_reg_written[rnum], 1);
#endif
}

static void gen_log_predicated_reg_write_pair(int rnum, TCGv_i64 val, int slot)
{
    TCGv val32 = tcg_temp_new();
    TCGv one = tcg_const_tl(1);
    TCGv zero = tcg_const_tl(0);
    TCGv slot_mask = tcg_temp_new();

    tcg_gen_andi_tl(slot_mask, hex_slot_cancelled, 1 << slot);
    /* Low word */
    tcg_gen_extrl_i64_i32(val32, val);
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_new_value[rnum], slot_mask, zero,
                       val32, hex_new_value[rnum]);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_reg_written[rnum],
                       slot_mask, zero,
                       one, hex_reg_written[rnum]);
#endif

    /* High word */
    tcg_gen_extrh_i64_i32(val32, val);
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_new_value[rnum + 1],
                       slot_mask, zero,
                       val32, hex_new_value[rnum + 1]);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_reg_written[rnum + 1],
                       slot_mask, zero,
                       one, hex_reg_written[rnum + 1]);
#endif

    tcg_temp_free(val32);
    tcg_temp_free(one);
    tcg_temp_free(zero);
    tcg_temp_free(slot_mask);
}

static void gen_log_reg_write_pair(int rnum, TCGv_i64 val)
{
    /* Low word */
    tcg_gen_extrl_i64_i32(hex_new_value[rnum], val);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movi_tl(hex_reg_written[rnum], 1);
#endif

    /* High word */
    tcg_gen_extrh_i64_i32(hex_new_value[rnum + 1], val);
#if HEX_DEBUG
    /* Do this so HELPER(debug_commit_end) will know */
    tcg_gen_movi_tl(hex_reg_written[rnum + 1], 1);
#endif
}

void gen_log_pred_write(int pnum, TCGv val)
{
    TCGv zero = tcg_const_tl(0);
    TCGv base_val = tcg_temp_new();
    TCGv and_val = tcg_temp_new();
    TCGv pred_written = tcg_temp_new();

    /* Multiple writes to the same preg are and'ed together */
    tcg_gen_andi_tl(base_val, val, 0xff);
    tcg_gen_and_tl(and_val, base_val, hex_new_pred_value[pnum]);
    tcg_gen_andi_tl(pred_written, hex_pred_written, 1 << pnum);
    tcg_gen_movcond_tl(TCG_COND_NE, hex_new_pred_value[pnum],
                       pred_written, zero,
                       and_val, base_val);
    tcg_gen_ori_tl(hex_pred_written, hex_pred_written, 1 << pnum);

    tcg_temp_free(zero);
    tcg_temp_free(base_val);
    tcg_temp_free(and_val);
    tcg_temp_free(pred_written);
}

static inline void gen_read_p3_0(TCGv control_reg)
{
    tcg_gen_movi_tl(control_reg, 0);
    for (int i = 0; i < NUM_PREGS; i++) {
        tcg_gen_deposit_tl(control_reg, control_reg, hex_pred[i], i * 8, 8);
    }
}

static inline void gen_write_p3_0(TCGv control_reg)
{
    for (int i = 0; i < NUM_PREGS; i++) {
        tcg_gen_extract_tl(hex_pred[i], control_reg, i * 8, 8);
    }
}

static inline void gen_load_locked4u(TCGv dest, TCGv vaddr, int mem_index)
{
    tcg_gen_qemu_ld32u(dest, vaddr, mem_index);
    tcg_gen_mov_tl(hex_llsc_addr, vaddr);
    tcg_gen_mov_tl(hex_llsc_val, dest);
}

static inline void gen_load_locked8u(TCGv_i64 dest, TCGv vaddr, int mem_index)
{
    tcg_gen_qemu_ld64(dest, vaddr, mem_index);
    tcg_gen_mov_tl(hex_llsc_addr, vaddr);
    tcg_gen_mov_i64(hex_llsc_val_i64, dest);
}

static inline void gen_store_conditional4(CPUHexagonState *env,
                                          DisasContext *ctx, int prednum,
                                          TCGv pred, TCGv vaddr, TCGv src)
{
    TCGLabel *fail = gen_new_label();
    TCGLabel *done = gen_new_label();
    TCGv one, zero, tmp;

    tcg_gen_brcond_tl(TCG_COND_NE, vaddr, hex_llsc_addr, fail);

    one = tcg_const_tl(0xff);
    zero = tcg_const_tl(0);
    tmp = tcg_temp_new();
    tcg_gen_atomic_cmpxchg_tl(tmp, hex_llsc_addr, hex_llsc_val, src,
                              ctx->mem_idx, MO_32);
    tcg_gen_movcond_tl(TCG_COND_EQ, hex_pred[prednum], tmp, hex_llsc_val,
                       one, zero);
    tcg_temp_free(one);
    tcg_temp_free(zero);
    tcg_temp_free(tmp);
    tcg_gen_br(done);

    gen_set_label(fail);
    tcg_gen_movi_tl(pred, 0);

    gen_set_label(done);
    tcg_gen_movi_tl(hex_llsc_addr, ~0);
}

static inline void gen_store_conditional8(CPUHexagonState *env,
                                          DisasContext *ctx, int prednum,
                                          TCGv pred, TCGv vaddr, TCGv_i64 src)
{
    TCGLabel *fail = gen_new_label();
    TCGLabel *done = gen_new_label();
    TCGv_i64 one, zero, tmp;

    tcg_gen_brcond_tl(TCG_COND_NE, vaddr, hex_llsc_addr, fail);

    one = tcg_const_i64(0xff);
    zero = tcg_const_i64(0);
    tmp = tcg_temp_new_i64();
    tcg_gen_atomic_cmpxchg_i64(tmp, hex_llsc_addr, hex_llsc_val_i64, src,
                               ctx->mem_idx, MO_64);
    tcg_gen_movcond_i64(TCG_COND_EQ, tmp, tmp, hex_llsc_val_i64,
                        one, zero);
    tcg_gen_extrl_i64_i32(hex_pred[prednum], tmp);
    tcg_temp_free_i64(one);
    tcg_temp_free_i64(zero);
    tcg_temp_free_i64(tmp);
    tcg_gen_br(done);

    gen_set_label(fail);
    tcg_gen_movi_tl(pred, 0);

    gen_set_label(done);
    tcg_gen_movi_tl(hex_llsc_addr, ~0);
}

void gen_fbrev(TCGv result, TCGv src)
{
    TCGv lo = tcg_temp_new();
    TCGv tmp1 = tcg_temp_new();
    TCGv tmp2 = tcg_temp_new();

    /* Bit reversal of low 16 bits */
    tcg_gen_andi_tl(lo, src, 0xffff);
    tcg_gen_andi_tl(tmp1, lo, 0xaaaa);
    tcg_gen_shri_tl(tmp1, tmp1, 1);
    tcg_gen_andi_tl(tmp2, lo, 0x5555);
    tcg_gen_shli_tl(tmp2, tmp2, 1);
    tcg_gen_or_tl(lo, tmp1, tmp2);
    tcg_gen_andi_tl(tmp1, lo, 0xcccc);
    tcg_gen_shri_tl(tmp1, tmp1, 2);
    tcg_gen_andi_tl(tmp2, lo, 0x3333);
    tcg_gen_shli_tl(tmp2, tmp2, 2);
    tcg_gen_or_tl(lo, tmp1, tmp2);
    tcg_gen_andi_tl(tmp1, lo, 0xf0f0);
    tcg_gen_shri_tl(tmp1, tmp1, 4);
    tcg_gen_andi_tl(tmp2, lo, 0x0f0f);
    tcg_gen_shli_tl(tmp2, tmp2, 4);
    tcg_gen_or_tl(lo, tmp1, tmp2);
    tcg_gen_bswap16_tl(lo, lo);

    /* Final tweaks */
    tcg_gen_andi_tl(result, src, 0xffff0000);
    tcg_gen_ori_tl(result, lo, 8);

    tcg_temp_free(lo);
    tcg_temp_free(tmp1);
    tcg_temp_free(tmp2);
}

TCGv gen_set_bit(int i, TCGv result, TCGv src)
{
    TCGv mask = tcg_const_tl(~(1 << i));
    TCGv bit = tcg_temp_new();
    tcg_gen_shli_tl(bit, src, i);
    tcg_gen_and_tl(result, result, mask);
    tcg_gen_or_tl(result, result, bit);
    tcg_temp_free(mask);
    tcg_temp_free(bit);

    return result;
}

void gen_cancel(TCGv slot)
{
    TCGv one = tcg_const_tl(1);
    TCGv mask = tcg_temp_new();
    tcg_gen_shl_tl(mask, one, slot);
    tcg_gen_or_tl(hex_slot_cancelled, hex_slot_cancelled, mask);
    tcg_temp_free(one);
    tcg_temp_free(mask);
}

void gen_store32(TCGv vaddr, TCGv src, int width, int slot)
{
    tcg_gen_mov_tl(hex_store_addr[slot], vaddr);
    tcg_gen_movi_tl(hex_store_width[slot], width);
    tcg_gen_mov_tl(hex_store_val32[slot], src);
}

void gen_store1(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot)
{
    gen_store32(vaddr, src, 1, slot);
    ctx->store_width[slot] = 1;
}

void gen_store2(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot)
{
    gen_store32(vaddr, src, 2, slot);
    ctx->store_width[slot] = 2;
}

void gen_store4(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot)
{
    gen_store32(vaddr, src, 4, slot);
    ctx->store_width[slot] = 4;
}


void gen_store8(TCGv_env cpu_env, TCGv vaddr, TCGv_i64 src, DisasContext *ctx,
                int slot)
{
    tcg_gen_mov_tl(hex_store_addr[slot], vaddr);
    tcg_gen_movi_tl(hex_store_width[slot], 8);
    tcg_gen_mov_i64(hex_store_val64[slot], src);
    ctx->store_width[slot] = 8;
}

void gen_set_usr_field(int field, TCGv val)
{
    tcg_gen_deposit_tl(hex_gpr[HEX_REG_USR], hex_gpr[HEX_REG_USR], val,
                       reg_field_info[field].offset,
                       reg_field_info[field].width);
}

void gen_set_usr_fieldi(int field, int x)
{
    TCGv val = tcg_const_tl(x);
    gen_set_usr_field(field, val);
    tcg_temp_free(val);
}

void gen_write_new_pc(TCGv addr)
{
    /* If there are multiple branches in a packet, ignore the second one */
    TCGv zero = tcg_const_tl(0);
    tcg_gen_movcond_tl(TCG_COND_NE, hex_next_PC, hex_branch_taken, zero,
                       hex_next_PC, addr);
    tcg_gen_movi_tl(hex_branch_taken, 1);
    tcg_temp_free(zero);
}

void gen_sat_i32(TCGv dest, TCGv source, int width, bool set_overflow)
{
    TCGv max_val = tcg_const_i32((1 << (width - 1)) - 1);
    TCGv min_val = tcg_const_i32(-(1 << (width - 1)));
    tcg_gen_movcond_i32(TCG_COND_GT, dest, source, max_val, max_val, source);
    tcg_gen_movcond_i32(TCG_COND_LT, dest, source, min_val, min_val, dest);
    /* Set Overflow Bit */
    if (set_overflow) {
        TCGv ovf = tcg_temp_new();
        TCGv one = tcg_const_i32(1);
        GET_USR_FIELD(USR_OVF, ovf);
        tcg_gen_movcond_i32(TCG_COND_GT, ovf, source, max_val, one, ovf);
        tcg_gen_movcond_i32(TCG_COND_LT, ovf, source, min_val, one, ovf);
        SET_USR_FIELD(USR_OVF, ovf);
        tcg_temp_free_i32(ovf);
        tcg_temp_free_i32(one);
    }
    tcg_temp_free_i32(max_val);
    tcg_temp_free_i32(min_val);
}

void gen_satu_i32(TCGv dest, TCGv source, int width, bool set_overflow)
{
    TCGv max_val = tcg_const_i32((1 << width) - 1);
    tcg_gen_movcond_i32(TCG_COND_GTU, dest, source, max_val, max_val, source);
    TCGv_i32 zero = tcg_const_i32(0);
    tcg_gen_movcond_i32(TCG_COND_LT, dest, source, zero, zero, dest);
    /* Set Overflow Bit */
    if (set_overflow) {
        TCGv ovf = tcg_temp_new();
        TCGv one = tcg_const_i32(1);
        GET_USR_FIELD(USR_OVF, ovf);
        tcg_gen_movcond_i32(TCG_COND_GTU, ovf, source, max_val, one, ovf);
        SET_USR_FIELD(USR_OVF, ovf);
        tcg_temp_free_i32(ovf);
        tcg_temp_free_i32(one);
    }
    tcg_temp_free_i32(max_val);
    tcg_temp_free_i32(zero);
}

void gen_sat_i64(TCGv_i64 dest, TCGv_i64 source, int width, bool set_overflow)
{
    TCGv_i64 max_val = tcg_const_i64((1 << (width - 1)) - 1);
    TCGv_i64 min_val = tcg_const_i64(-(1 << (width - 1)));
    tcg_gen_movcond_i64(TCG_COND_GT, dest, source, max_val, max_val, source);
    tcg_gen_movcond_i64(TCG_COND_LT, dest, source, min_val, min_val, dest);
    /* Set Overflow Bit */
    if (set_overflow) {
        TCGv ovf = tcg_temp_new();
        TCGv_i64 ovf_ext = tcg_temp_new_i64();
        TCGv_i64 one = tcg_const_i64(1);
        GET_USR_FIELD(USR_OVF, ovf);
        tcg_gen_ext_i32_i64(ovf_ext, ovf);
        tcg_gen_movcond_i64(TCG_COND_GT,
                            ovf_ext,
                            source,
                            max_val,
                            one,
                            ovf_ext);
        tcg_gen_movcond_i64(TCG_COND_LT,
                            ovf_ext,
                            source,
                            min_val,
                            one,
                            ovf_ext);
        tcg_gen_trunc_i64_tl(ovf, ovf_ext);
        SET_USR_FIELD(USR_OVF, ovf);
        tcg_temp_free_i32(ovf);
        tcg_temp_free_i64(ovf_ext);
        tcg_temp_free_i64(one);
    }
    tcg_temp_free_i64(max_val);
    tcg_temp_free_i64(min_val);
}

void gen_satu_i64(TCGv_i64 dest, TCGv_i64 source, int width, bool set_overflow)
{
    TCGv_i64 max_val = tcg_const_i64((1 << width) - 1);
    tcg_gen_movcond_i64(TCG_COND_GTU, dest, source, max_val, max_val, source);
    TCGv_i64 zero = tcg_const_i64(0);
    tcg_gen_movcond_i64(TCG_COND_LT, dest, source, zero, zero, dest);
    /* Set Overflow Bit */
    if (set_overflow) {
        TCGv ovf = tcg_temp_new();
        TCGv_i64 ovf_ext = tcg_temp_new_i64();
        TCGv_i64 one = tcg_const_i64(1);
        GET_USR_FIELD(USR_OVF, ovf);
        tcg_gen_ext_i32_i64(ovf_ext, ovf);
        tcg_gen_movcond_i64(TCG_COND_GTU,
                            ovf_ext,
                            source,
                            max_val,
                            one,
                            ovf_ext);
        tcg_gen_trunc_i64_tl(ovf, ovf_ext);
        SET_USR_FIELD(USR_OVF, ovf);
        tcg_temp_free_i32(ovf);
        tcg_temp_free_i64(ovf_ext);
        tcg_temp_free_i64(one);
    }
    tcg_temp_free_i64(max_val);
    tcg_temp_free_i64(zero);
}

#include "tcg_funcs_generated.h"
#include "tcg_func_table_generated.h"
