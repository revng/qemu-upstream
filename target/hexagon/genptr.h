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

#ifndef HEXAGON_GENPTR_H
#define HEXAGON_GENPTR_H

#include "insn.h"
#include "tcg/tcg.h"
#include "translate.h"

extern const SemanticInsn opcode_genptr[];

TCGv gen_read_reg(TCGv result, int num);
TCGv gen_read_preg(TCGv pred, uint8_t num);
void gen_log_reg_write(int rnum, TCGv val);
void gen_log_pred_write(int pnum, TCGv val);
void gen_fbrev(TCGv result, TCGv src);
void gen_cancel(TCGv slot);
TCGv gen_set_bit(int i, TCGv result, TCGv src);
void gen_store32(TCGv vaddr, TCGv src, int width, int slot);
void gen_store1(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot);
void gen_store2(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot);
void gen_store4(TCGv_env cpu_env, TCGv vaddr, TCGv src, DisasContext *ctx,
                int slot);
void gen_store8(TCGv_env cpu_env, TCGv vaddr, TCGv_i64 src, DisasContext *ctx,
                int slot);
void gen_set_usr_field(int field, TCGv val);
void gen_set_usr_fieldi(int field, int x);
void gen_write_new_pc(TCGv addr);

#endif
