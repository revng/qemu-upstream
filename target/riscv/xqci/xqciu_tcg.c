#include "qemu/osdep.h"
#include "qemu/log.h"
#include "cpu.h"
#include "tcg/tcg-op.h"
#include "tcg/tcg-op-gvec.h"
#include "tcg/tcg.h"
#include "exec/exec-all.h"
#include "exec/helper-gen.h"

// void _ZN12CPUArchState9qc_addsatEhhh
static void emit_qc_addsat(DisasContext *ctx, TCGv_env env, int8_t vi_25, int8_t vi_23, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp22 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp21 = xqci_get_gpr(ctx, vi_23);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_add_i32(temp8, temp21, temp22);
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_23);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xor_i32(temp6, temp18, temp19);
tcg_gen_mov_i32(temp0, temp8);
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LE, temp6, -1, label29);
gen_set_label(label30);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_25);
tcg_gen_xor_i32(temp6, temp16, temp8);
tcg_gen_mov_i32(temp0, temp8);
TCGLabel * label31 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GT, temp6, -1, label29);
gen_set_label(label31);
temp8 = xqci_get_gpr(ctx, vi_25);
tcg_gen_movcond_i32(TCG_COND_LT, temp6, temp8, tcg_constant_i32(0), tcg_constant_i32(INT32_MIN), tcg_constant_i32(2147483647));
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label29);
gen_set_label(label29);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_addusatEhhh
static void emit_qc_addusat(DisasContext *ctx, TCGv_env env, int8_t vi_22, int8_t vi_20, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_22);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_add_i32(temp9, temp18, temp19);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_22);
TCGLabel * label26 = gen_new_label();
TCGLabel * label27 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp16, 0, label26);
gen_set_label(label27);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_LT, temp11, temp12, 0);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_GT, temp10, temp9, -1);
tcg_gen_and_i32(temp10, temp10, temp11);
tcg_gen_mov_i32(temp0, temp9);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp10, 0, label28);
tcg_gen_br(label29);
gen_set_label(label26);
tcg_gen_mov_i32(temp0, temp9);
tcg_gen_brcondi_i32(TCG_COND_LE, temp9, -1, label29);
gen_set_label(label28);
tcg_gen_movi_i32(temp0, -1);
tcg_gen_br(label29);
gen_set_label(label29);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_beqiEthh
static void emit_qc_beqi(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState7qc_bgeiEthh
static void emit_qc_bgei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState8qc_bgeuiEthh
static void emit_qc_bgeui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_10, int8_t vi_13) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_13);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp8, vi_10, label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState7qc_bltiEthh
static void emit_qc_blti(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState8qc_bltuiEthh
static void emit_qc_bltui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_10, int8_t vi_13) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_13);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp8, vi_10, label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState7qc_bneiEthh
static void emit_qc_bnei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState10qc_c_bextiEhh
static void emit_qc_c_bexti(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_10) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, ((vi_10 & 7) | 8));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp6, (vi_13 & 31));
tcg_gen_andi_i32(temp0, temp0, 1);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) ((vi_10 & 7) | 8))], temp0);
}

// void _ZN12CPUArchState10qc_c_bsetiEhh
static void emit_qc_c_bseti(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_10) {
TCGv_i32 temp5 = xqci_get_gpr(ctx, ((vi_10 & 7) | 8));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp0, temp5, (1 << (vi_13 & 31)));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) ((vi_10 & 7) | 8))], temp0);
}

// void _ZN12CPUArchState11qc_c_clrintEh
static void emit_qc_c_clrint(DisasContext *ctx, TCGv_env env, int8_t vi_10) {
TCGv_i32 temp3 = xqci_csrr(ctx, env, (((uint8_t) vi_10 >> 5) | 2032));
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp3, ((1 << (vi_10 & 31)) ^ -1));
xqci_csrw(ctx, env, (((uint8_t) vi_10 >> 5) | 2032), temp2);
}

// void _ZN12CPUArchState10qc_c_delayEh
static void emit_qc_c_delay(DisasContext *ctx, TCGv_env env, int8_t vi_1) {
}

// void _ZN12CPUArchState7qc_c_diEv
static void emit_qc_c_di(DisasContext *ctx, TCGv_env env) {
xqci_csrw_field(ctx, env, 768, 8, tcg_constant_i32(0));
TCGv_i32 temp3 = xqci_csrr(ctx, env, 1993);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp3, -67108865);
xqci_csrw(ctx, env, 1993, temp2);
}

// void _ZN12CPUArchState8qc_c_dirEh
static void emit_qc_c_dir(DisasContext *ctx, TCGv_env env, int8_t vi_4) {
TCGv_i32 temp0 = xqci_csrr(ctx, env, 768);
xqci_csrw_field(ctx, env, 768, 8, tcg_constant_i32(0));
TCGv_i32 temp9 = xqci_csrr(ctx, env, 1993);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp9, -67108865);
xqci_csrw(ctx, env, 1993, temp8);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_c_eiEv
static void emit_qc_c_ei(DisasContext *ctx, TCGv_env env) {
xqci_csrw_field(ctx, env, 768, 8, tcg_constant_i32(1));
TCGv_i32 temp3 = xqci_csrr(ctx, env, 1993);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp2, temp3, 67108864);
xqci_csrw(ctx, env, 1993, temp2);
}

// void _ZN12CPUArchState8qc_c_eirEh
static void emit_qc_c_eir(DisasContext *ctx, TCGv_env env, int8_t vi_11) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp3 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp3, temp8, 3);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp3, 1);
xqci_csrw_field(ctx, env, 768, 8, temp2);
temp3 = xqci_csrr(ctx, env, 1993);
tcg_gen_shli_i32(temp2, temp2, 26);
tcg_gen_or_i32(temp2, temp2, temp3);
xqci_csrw(ctx, env, 1993, temp2);
}

// void _ZN12CPUArchState9qc_c_extuEhh
static void emit_qc_c_extu(DisasContext *ctx, TCGv_env env, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp6, ((-2ll << ((uint64_t) (uint32_t) vi_10)) ^ -1));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_c_mnretEv
static void emit_qc_c_mnret(DisasContext *ctx, TCGv_env env) {
TCGv_i32 temp26 = xqci_csrr(ctx, env, 1993);
TCGv_i32 temp22 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp22, temp26, -1425076225);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp26, 28);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp19, temp0, 1);
xqci_csrw_field(ctx, env, 768, 8, temp19);
xqci_csrw_field(ctx, env, 1860, 8, tcg_constant_i32(1));
tcg_gen_ori_i32(temp0, temp22, 268435456);
tcg_gen_shli_i32(temp22, temp19, 26);
tcg_gen_or_i32(temp0, temp22, temp0);
tcg_gen_shri_i32(temp19, temp26, 8);
tcg_gen_andi_i32(temp22, temp19, 61440);
tcg_gen_or_i32(temp22, temp0, temp22);
tcg_gen_ori_i32(temp0, temp22, 1089470464);
xqci_csrw(ctx, env, 1993, temp0);
temp22 = xqci_csrr(ctx, env, 1860);
TCGLabel * label32 = gen_new_label();
TCGLabel * label33 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp22, 3, label32);
gen_set_label(label33);
xqci_csrw_field(ctx, env, 768, 131072, tcg_constant_i32(0));
temp19 = xqci_implemented_Smdbltrp(ctx);
TCGLabel * label34 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp19, 0, label34);
tcg_gen_br(label32);
gen_set_label(label34);
xqci_csrw_field(ctx, env, 784, 1024, tcg_constant_i32(0));
tcg_gen_br(label32);
gen_set_label(label32);
TCGv_i32 temp16 = xqci_csrr(ctx, env, 1860);
TCGLabel * label36 = gen_new_label();
TCGLabel * label37 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label36);
gen_set_label(label37);
TCGv_i32 temp14 = xqci_csrr(ctx, env, 1860);
TCGLabel * label38 = gen_new_label();
TCGLabel * label39 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp14, 1, label38);
gen_set_label(label39);
TCGv_i32 temp12 = xqci_csrr(ctx, env, 1860);
TCGLabel * label40 = gen_new_label();
TCGLabel * label41 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp12, 3, label40);
gen_set_label(label41);
xqci_set_mode_M(ctx);
tcg_gen_br(label40);
gen_set_label(label38);
xqci_set_mode_S(ctx);
tcg_gen_br(label40);
gen_set_label(label36);
xqci_set_mode_U(ctx);
tcg_gen_br(label40);
gen_set_label(label40);
TCGv_i32 temp7 = xqci_implemented_U(ctx);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp7, tcg_constant_i32(0), tcg_constant_i32(0), tcg_constant_i32(3));
xqci_csrw_field(ctx, env, 1860, 6144, temp0);
temp0 = xqci_csrr(ctx, env, 1857);
tcg_gen_mov_i32(cpu_pc, temp0);
}

// void _ZN12CPUArchState9qc_c_mretEv
static void emit_qc_c_mret(DisasContext *ctx, TCGv_env env) {
TCGv_i32 temp22 = xqci_csrr(ctx, env, 1993);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp16, temp22, -739241985);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp22, 27);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp14, temp0, 1);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp22, 29);
tcg_gen_andi_i32(temp0, temp18, 1);
xqci_csrw_field(ctx, env, 768, 8, temp14);
xqci_csrw_field(ctx, env, 768, 128, tcg_constant_i32(1));
TCGv_i32 temp28 = xqci_implemented_Smdbltrp(ctx);
TCGLabel * label34 = gen_new_label();
TCGLabel * label35 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp28, 0, label34);
tcg_gen_br(label35);
gen_set_label(label34);
xqci_csrw_field(ctx, env, 784, 1024, temp0);
tcg_gen_br(label35);
gen_set_label(label35);
tcg_gen_ori_i32(temp18, temp16, 134217728);
tcg_gen_shli_i32(temp16, temp14, 26);
tcg_gen_or_i32(temp18, temp16, temp18);
tcg_gen_shri_i32(temp14, temp22, 4);
tcg_gen_andi_i32(temp16, temp14, 61440);
tcg_gen_or_i32(temp16, temp18, temp16);
tcg_gen_ori_i32(temp18, temp16, 983040);
xqci_csrw(ctx, env, 1993, temp18);
TCGLabel * label37 = gen_new_label();
TCGLabel * label38 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp0, 0, label37);
gen_set_label(label38);
temp18 = xqci_csrr(ctx, env, 768);
TCGLabel * label39 = gen_new_label();
TCGLabel * label40 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp18, 3, label39);
gen_set_label(label40);
xqci_csrw_field(ctx, env, 768, 131072, tcg_constant_i32(0));
tcg_gen_br(label39);
gen_set_label(label39);
temp16 = xqci_csrr(ctx, env, 768);
TCGLabel * label41 = gen_new_label();
TCGLabel * label42 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label41);
gen_set_label(label42);
temp14 = xqci_csrr(ctx, env, 768);
TCGLabel * label43 = gen_new_label();
TCGLabel * label44 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp14, 1, label43);
gen_set_label(label44);
TCGv_i32 temp12 = xqci_csrr(ctx, env, 768);
TCGLabel * label45 = gen_new_label();
TCGLabel * label46 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp12, 3, label45);
gen_set_label(label46);
xqci_set_mode_M(ctx);
tcg_gen_br(label45);
gen_set_label(label43);
xqci_set_mode_S(ctx);
tcg_gen_br(label45);
gen_set_label(label41);
xqci_set_mode_U(ctx);
tcg_gen_br(label45);
gen_set_label(label45);
TCGv_i32 temp7 = xqci_implemented_U(ctx);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp7, tcg_constant_i32(0), tcg_constant_i32(0), tcg_constant_i32(3));
xqci_csrw_field(ctx, env, 768, 6144, temp0);
tcg_gen_br(label37);
gen_set_label(label37);
temp0 = xqci_csrr(ctx, env, 833);
tcg_gen_mov_i32(cpu_pc, temp0);
}

// void _ZN12CPUArchState12qc_c_muliaddEhhh
static void emit_qc_c_muliadd(DisasContext *ctx, TCGv_env env, int8_t vi_8, int8_t vi_13, int8_t vi_16) {
TCGv_i32 temp5 = xqci_get_gpr(ctx, ((vi_16 & 7) | 8));
TCGv_i32 temp6 = xqci_get_gpr(ctx, ((vi_13 & 7) | 8));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_muli_i32(temp0, temp6, vi_8);
tcg_gen_add_i32(temp0, temp0, temp5);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) ((vi_16 & 7) | 8))], temp0);
}

// void _ZN12CPUArchState10qc_c_mveqzEhh
static void emit_qc_c_mveqz(DisasContext *ctx, TCGv_env env, int8_t vi_8, int8_t vi_16) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp13 = xqci_get_gpr(ctx, ((vi_16 & 7) | 8));
tcg_gen_mov_i32(temp0, temp13);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp13, 0, label22);
gen_set_label(label23);
TCGv_i32 temp5 = xqci_get_gpr(ctx, ((vi_8 & 7) | 8));
tcg_gen_mov_i32(temp0, temp5);
tcg_gen_br(label22);
gen_set_label(label22);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) ((vi_16 & 7) | 8))], temp0);
}

// void _ZN12CPUArchState11qc_c_ptraceEv
static void emit_qc_c_ptrace(DisasContext *ctx, TCGv_env env) {
xqci_syscall(ctx, 9, tcg_constant_i32(0));
}

// void _ZN12CPUArchState11qc_c_setintEh
static void emit_qc_c_setint(DisasContext *ctx, TCGv_env env, int8_t vi_9) {
TCGv_i32 temp3 = xqci_csrr(ctx, env, (((uint8_t) vi_9 >> 5) | 2032));
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp2, temp3, (1 << (vi_9 & 31)));
xqci_csrw(ctx, env, (((uint8_t) vi_9 >> 5) | 2032), temp2);
}

// void _ZN12CPUArchState6qc_cloEhh
static void emit_qc_clo(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp6, temp8, -1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp0, temp6, 32);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_clrintiEt
static void emit_qc_clrinti(DisasContext *ctx, TCGv_env env, int16_t vi_10) {
TCGv_i32 temp3 = xqci_csrr(ctx, env, (((uint16_t) vi_10 >> 5) + 2032));
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp3, ((1 << (vi_10 & 31)) ^ -1));
xqci_csrw(ctx, env, (((uint16_t) vi_10 >> 5) + 2032), temp2);
}

// void _ZN12CPUArchState12qc_compress2Ehh
static void emit_qc_compress2(DisasContext *ctx, TCGv_env env, int8_t vi_57, int8_t vi_4) {
TCGv_i32 temp45 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp47 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp49 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp51 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp52 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp54 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp55 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp56 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp56, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp55, 1);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp9, 2);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp54, 2);
tcg_gen_andi_i32(temp9, temp10, 4);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp52, 3);
tcg_gen_andi_i32(temp10, temp11, 8);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp51, 4);
tcg_gen_andi_i32(temp11, temp12, 16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp49, 5);
tcg_gen_andi_i32(temp12, temp0, 32);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp47, 6);
tcg_gen_andi_i32(temp0, temp13, 64);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp45, 7);
tcg_gen_andi_i32(temp13, temp15, 128);
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp26 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp29 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp32 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp34 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp37 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp40 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp43 = xqci_get_gpr(ctx, vi_57);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp43, 16);
tcg_gen_andi_i32(temp15, temp16, 1);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp17, temp40, 17);
tcg_gen_andi_i32(temp16, temp17, 2);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp37, 18);
tcg_gen_andi_i32(temp17, temp18, 4);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp19, temp34, 19);
tcg_gen_andi_i32(temp18, temp19, 8);
TCGv_i32 temp20 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp20, temp32, 20);
tcg_gen_andi_i32(temp19, temp20, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp29, 21);
tcg_gen_andi_i32(temp20, temp6, 32);
TCGv_i32 temp21 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp21, temp26, 22);
tcg_gen_andi_i32(temp6, temp21, 64);
tcg_gen_shri_i32(temp21, temp23, 23);
tcg_gen_andi_i32(temp21, temp21, 128);
tcg_gen_or_i32(temp6, temp6, temp21);
tcg_gen_or_i32(temp6, temp6, temp20);
tcg_gen_or_i32(temp6, temp6, temp19);
tcg_gen_or_i32(temp6, temp6, temp18);
tcg_gen_or_i32(temp6, temp6, temp17);
tcg_gen_or_i32(temp6, temp6, temp16);
tcg_gen_or_i32(temp6, temp6, temp15);
tcg_gen_shli_i32(temp6, temp6, 8);
tcg_gen_or_i32(temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp0, temp12);
tcg_gen_or_i32(temp0, temp0, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_or_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_compress3Ehh
static void emit_qc_compress3(DisasContext *ctx, TCGv_env env, int8_t vi_42, int8_t vi_4) {
TCGv_i32 temp27 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp30 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp33 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp36 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp37 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp39 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp40 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp41 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp41, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp40, 2);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp9, 2);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp39, 4);
tcg_gen_andi_i32(temp9, temp10, 4);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp37, 6);
tcg_gen_andi_i32(temp10, temp11, 8);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp36, 8);
tcg_gen_andi_i32(temp11, temp12, 16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp33, 10);
tcg_gen_andi_i32(temp12, temp0, 32);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp30, 12);
tcg_gen_andi_i32(temp0, temp13, 64);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp27, 14);
tcg_gen_andi_i32(temp13, temp6, 128);
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_42);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp23, 24);
tcg_gen_andi_i32(temp6, temp15, 1);
tcg_gen_shri_i32(temp15, temp20, 26);
tcg_gen_andi_i32(temp15, temp15, 2);
tcg_gen_or_i32(temp6, temp15, temp6);
tcg_gen_shri_i32(temp15, temp17, 28);
tcg_gen_andi_i32(temp15, temp15, 4);
tcg_gen_or_i32(temp6, temp6, temp15);
tcg_gen_shli_i32(temp6, temp6, 8);
tcg_gen_or_i32(temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp0, temp12);
tcg_gen_or_i32(temp0, temp0, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_or_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_ctoEhh
static void emit_qc_cto(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_12);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp0, temp9, -1);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_ctzi_i32(temp6, temp0, 32);
tcg_gen_addi_i32(temp0, temp6, 1);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_e_addaiEjh
static void emit_qc_e_addai(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp6, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_addiEjhh
static void emit_qc_e_addi(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp6, ((int32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_e_andaiEjh
static void emit_qc_e_andai(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp6, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_andiEjhh
static void emit_qc_e_andi(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp6, ((int32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_beqiEtth
static void emit_qc_e_beqi(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState9qc_e_bgeiEtth
static void emit_qc_e_bgei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState10qc_e_bgeuiEtth
static void emit_qc_e_bgeui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_10, int8_t vi_13) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_13);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp8, vi_10, label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState9qc_e_bltiEtth
static void emit_qc_e_blti(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState10qc_e_bltuiEtth
static void emit_qc_e_bltui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_10, int8_t vi_13) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_13);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp8, vi_10, label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState9qc_e_bneiEtth
static void emit_qc_e_bnei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_12) {
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_12);
TCGLabel * label16 = gen_new_label();
TCGLabel * label17 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp8, ((int32_t) (int32_t) vi_9), label16);
gen_set_label(label17);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, ((int32_t) (int32_t) vi_2));
tcg_gen_br(label16);
gen_set_label(label16);
}

// void _ZN12CPUArchState6qc_e_jEj
static void emit_qc_e_j(DisasContext *ctx, TCGv_env env, int32_t vi_1) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_1);
}

// void _ZN12CPUArchState8qc_e_jalEj
static void emit_qc_e_jal(DisasContext *ctx, TCGv_env env, int32_t vi_5) {
TCGv_i32 temp4 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp4, cpu_pc);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp4, 6);
xqci_jump_pcrel(ctx, temp4, vi_5);
tcg_gen_mov_i32(cpu_gpr[1ull], temp0);
}

// void _ZN12CPUArchState7qc_e_lbEjhh
static void emit_qc_e_lb(DisasContext *ctx, TCGv_env env, int32_t vi_11, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_14);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp10, vi_11);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_qemu_ld_i32(temp7, temp0, ctx->mem_idx, MO_UB);
tcg_gen_shli_i32(temp0, temp7, 24);
tcg_gen_sari_i32(temp0, temp0, 24);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_e_lbuEjhh
static void emit_qc_e_lbu(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp6, temp9, vi_10);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_UB);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_lhEjhh
static void emit_qc_e_lh(DisasContext *ctx, TCGv_env env, int32_t vi_12, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp11, vi_12);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_qemu_ld_i32(temp7, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_shli_i32(temp0, temp7, 16);
tcg_gen_sari_i32(temp0, temp0, 16);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_e_lhuEjhh
static void emit_qc_e_lhu(DisasContext *ctx, TCGv_env env, int32_t vi_11, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_14);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp6, temp10, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_LEUW);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_liEjh
static void emit_qc_e_li(DisasContext *ctx, TCGv_env env, int32_t vi_0, int8_t vi_5) {
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_5)], vi_0);
}

// void _ZN12CPUArchState7qc_e_lwEjhh
static void emit_qc_e_lw(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp6, temp9, vi_10);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_LESL);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_oraiEjh
static void emit_qc_e_orai(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp0, temp6, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_e_oriEjhh
static void emit_qc_e_ori(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp0, temp6, ((int32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_sbEjhh
static void emit_qc_e_sb(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_7, int8_t vi_13) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp9, vi_10);
TCGv_i32 temp4 = xqci_get_gpr(ctx, vi_7);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp1, temp4, 255);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_UB);
}

// void _ZN12CPUArchState7qc_e_shEjhh
static void emit_qc_e_sh(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_7, int8_t vi_13) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp9, vi_10);
TCGv_i32 temp4 = xqci_get_gpr(ctx, vi_7);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp1, temp4, 65535);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUW);
}

// void _ZN12CPUArchState7qc_e_swEjhh
static void emit_qc_e_sw(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_5, int8_t vi_11) {
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp7, vi_8);
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_5);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUL);
}

// void _ZN12CPUArchState10qc_e_xoraiEjh
static void emit_qc_e_xorai(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp0, temp6, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_xoriEjhh
static void emit_qc_e_xori(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp0, temp6, ((int32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_expand2Ehh
static void emit_qc_expand2(DisasContext *ctx, TCGv_env env, int8_t vi_87, int8_t vi_4) {
TCGv_i32 temp79 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp80 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp81 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp82 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp83 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp84 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp85 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp86 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp10, temp86, 1);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp11, temp85, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp9, temp11, 2);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp12, temp84, 1);
tcg_gen_andi_i32(temp11, temp12, 4);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp13, temp83, 2);
tcg_gen_andi_i32(temp12, temp13, 8);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp14, temp82, 2);
tcg_gen_andi_i32(temp13, temp14, 16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp81, 3);
tcg_gen_andi_i32(temp14, temp0, 32);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp15, temp80, 3);
tcg_gen_andi_i32(temp0, temp15, 64);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp19, temp79, 4);
tcg_gen_andi_i32(temp15, temp19, 128);
TCGv_i32 temp78 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp25 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp25, temp78, 128);
TCGv_i32 temp70 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp71 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp72 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp73 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp74 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp76 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp77 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp20 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp20, temp77, 4);
tcg_gen_andi_i32(temp19, temp20, 1);
TCGv_i32 temp21 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp21, temp76, 3);
tcg_gen_andi_i32(temp20, temp21, 2);
TCGv_i32 temp22 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp22, temp74, 3);
tcg_gen_andi_i32(temp21, temp22, 4);
TCGv_i32 temp23 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp23, temp73, 2);
tcg_gen_andi_i32(temp22, temp23, 8);
TCGv_i32 temp24 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp24, temp72, 2);
tcg_gen_andi_i32(temp23, temp24, 16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp71, 1);
tcg_gen_andi_i32(temp24, temp8, 32);
TCGv_i32 temp26 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp26, temp70, 1);
tcg_gen_andi_i32(temp8, temp26, 64);
TCGv_i32 temp59 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp60 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp62 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp63 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp65 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp66 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp68 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp69 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp27 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp27, temp69, 8);
tcg_gen_andi_i32(temp26, temp27, 1);
TCGv_i32 temp28 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp28, temp68, 7);
tcg_gen_andi_i32(temp27, temp28, 2);
TCGv_i32 temp29 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp29, temp66, 7);
tcg_gen_andi_i32(temp28, temp29, 4);
TCGv_i32 temp30 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp30, temp65, 6);
tcg_gen_andi_i32(temp29, temp30, 8);
TCGv_i32 temp31 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp31, temp63, 6);
tcg_gen_andi_i32(temp30, temp31, 16);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp62, 5);
tcg_gen_andi_i32(temp31, temp7, 32);
TCGv_i32 temp32 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp32, temp60, 5);
tcg_gen_andi_i32(temp7, temp32, 64);
TCGv_i32 temp33 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp33, temp59, 4);
tcg_gen_andi_i32(temp32, temp33, 128);
TCGv_i32 temp41 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp43 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp46 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp47 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp49 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp51 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp54 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp56 = xqci_get_gpr(ctx, vi_87);
TCGv_i32 temp34 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp34, temp56, 12);
tcg_gen_andi_i32(temp33, temp34, 1);
TCGv_i32 temp35 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp35, temp54, 11);
tcg_gen_andi_i32(temp34, temp35, 2);
TCGv_i32 temp36 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp36, temp51, 11);
tcg_gen_andi_i32(temp35, temp36, 4);
TCGv_i32 temp37 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp37, temp49, 10);
tcg_gen_andi_i32(temp36, temp37, 8);
TCGv_i32 temp38 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp38, temp47, 10);
tcg_gen_andi_i32(temp37, temp38, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp46, 9);
tcg_gen_andi_i32(temp38, temp6, 32);
TCGv_i32 temp39 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp39, temp43, 9);
tcg_gen_andi_i32(temp6, temp39, 64);
tcg_gen_shri_i32(temp39, temp41, 8);
tcg_gen_andi_i32(temp39, temp39, 128);
tcg_gen_or_i32(temp6, temp6, temp39);
tcg_gen_or_i32(temp6, temp6, temp38);
tcg_gen_or_i32(temp6, temp6, temp37);
tcg_gen_or_i32(temp6, temp6, temp36);
tcg_gen_or_i32(temp6, temp6, temp35);
tcg_gen_or_i32(temp6, temp6, temp34);
tcg_gen_or_i32(temp6, temp6, temp33);
tcg_gen_or_i32(temp7, temp7, temp32);
tcg_gen_or_i32(temp7, temp7, temp31);
tcg_gen_or_i32(temp7, temp7, temp30);
tcg_gen_or_i32(temp7, temp7, temp29);
tcg_gen_or_i32(temp7, temp7, temp28);
tcg_gen_or_i32(temp7, temp7, temp27);
tcg_gen_or_i32(temp7, temp7, temp26);
tcg_gen_or_i32(temp8, temp8, temp25);
tcg_gen_or_i32(temp8, temp8, temp24);
tcg_gen_or_i32(temp8, temp8, temp23);
tcg_gen_or_i32(temp8, temp8, temp22);
tcg_gen_or_i32(temp8, temp8, temp21);
tcg_gen_or_i32(temp8, temp8, temp20);
tcg_gen_or_i32(temp8, temp8, temp19);
tcg_gen_shli_i32(temp8, temp8, 8);
tcg_gen_shli_i32(temp7, temp7, 16);
tcg_gen_shli_i32(temp6, temp6, 24);
tcg_gen_or_i32(temp0, temp0, temp15);
tcg_gen_or_i32(temp0, temp0, temp14);
tcg_gen_or_i32(temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp0, temp12);
tcg_gen_or_i32(temp0, temp0, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_or_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_expand3Ehh
static void emit_qc_expand3(DisasContext *ctx, TCGv_env env, int8_t vi_83, int8_t vi_4) {
TCGv_i32 temp75 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp76 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp77 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp78 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp79 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp80 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp81 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp82 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp10, temp82, 1);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp11, temp81, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp9, temp11, 2);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp12, temp80, 2);
tcg_gen_andi_i32(temp11, temp12, 4);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp13, temp79, 2);
tcg_gen_andi_i32(temp12, temp13, 8);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp14, temp78, 3);
tcg_gen_andi_i32(temp13, temp14, 16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp77, 4);
tcg_gen_andi_i32(temp14, temp0, 32);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp15, temp76, 4);
tcg_gen_andi_i32(temp0, temp15, 64);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp19, temp75, 5);
tcg_gen_andi_i32(temp15, temp19, 128);
TCGv_i32 temp67 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp68 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp69 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp74 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp24 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp24, temp74, 16);
TCGv_i32 temp73 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp22 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp22, temp73, 8);
TCGv_i32 temp70 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp71 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp72 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp20 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp20, temp72, 2);
tcg_gen_andi_i32(temp19, temp20, 1);
TCGv_i32 temp21 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp21, temp71, 2);
tcg_gen_andi_i32(temp20, temp21, 2);
TCGv_i32 temp23 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp23, temp70, 1);
tcg_gen_andi_i32(temp21, temp23, 4);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp8, temp69, 1);
tcg_gen_andi_i32(temp23, temp8, 32);
TCGv_i32 temp25 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp25, temp68, 2);
tcg_gen_andi_i32(temp8, temp25, 64);
TCGv_i32 temp26 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp26, temp67, 2);
tcg_gen_andi_i32(temp25, temp26, 128);
TCGv_i32 temp66 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp32 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp32, temp66, 128);
TCGv_i32 temp59 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp60 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp61 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp62 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp63 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp64 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp65 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp27 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp27, temp65, 5);
tcg_gen_andi_i32(temp26, temp27, 1);
TCGv_i32 temp28 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp28, temp64, 4);
tcg_gen_andi_i32(temp27, temp28, 2);
TCGv_i32 temp29 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp29, temp63, 4);
tcg_gen_andi_i32(temp28, temp29, 4);
TCGv_i32 temp30 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp30, temp62, 3);
tcg_gen_andi_i32(temp29, temp30, 8);
TCGv_i32 temp31 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp31, temp61, 2);
tcg_gen_andi_i32(temp30, temp31, 16);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp60, 2);
tcg_gen_andi_i32(temp31, temp7, 32);
TCGv_i32 temp33 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp33, temp59, 1);
tcg_gen_andi_i32(temp7, temp33, 64);
TCGv_i32 temp41 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp44 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp47 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp48 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp50 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp52 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp54 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp57 = xqci_get_gpr(ctx, vi_83);
TCGv_i32 temp34 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp34, temp57, 8);
tcg_gen_andi_i32(temp33, temp34, 1);
TCGv_i32 temp35 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp35, temp54, 7);
tcg_gen_andi_i32(temp34, temp35, 2);
TCGv_i32 temp36 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp36, temp52, 6);
tcg_gen_andi_i32(temp35, temp36, 4);
TCGv_i32 temp37 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp37, temp50, 6);
tcg_gen_andi_i32(temp36, temp37, 8);
TCGv_i32 temp38 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp38, temp48, 5);
tcg_gen_andi_i32(temp37, temp38, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp47, 4);
tcg_gen_andi_i32(temp38, temp6, 32);
TCGv_i32 temp39 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp39, temp44, 4);
tcg_gen_andi_i32(temp6, temp39, 64);
tcg_gen_shri_i32(temp39, temp41, 3);
tcg_gen_andi_i32(temp39, temp39, 128);
tcg_gen_or_i32(temp6, temp6, temp39);
tcg_gen_or_i32(temp6, temp6, temp38);
tcg_gen_or_i32(temp6, temp6, temp37);
tcg_gen_or_i32(temp6, temp6, temp36);
tcg_gen_or_i32(temp6, temp6, temp35);
tcg_gen_or_i32(temp6, temp6, temp34);
tcg_gen_or_i32(temp6, temp6, temp33);
tcg_gen_or_i32(temp7, temp7, temp32);
tcg_gen_or_i32(temp7, temp7, temp31);
tcg_gen_or_i32(temp7, temp7, temp30);
tcg_gen_or_i32(temp7, temp7, temp29);
tcg_gen_or_i32(temp7, temp7, temp28);
tcg_gen_or_i32(temp7, temp7, temp27);
tcg_gen_or_i32(temp7, temp7, temp26);
tcg_gen_or_i32(temp8, temp8, temp25);
tcg_gen_or_i32(temp8, temp8, temp24);
tcg_gen_or_i32(temp8, temp8, temp23);
tcg_gen_or_i32(temp8, temp8, temp22);
tcg_gen_or_i32(temp8, temp8, temp21);
tcg_gen_or_i32(temp8, temp8, temp20);
tcg_gen_or_i32(temp8, temp8, temp19);
tcg_gen_shli_i32(temp8, temp8, 8);
tcg_gen_shli_i32(temp7, temp7, 16);
tcg_gen_shli_i32(temp6, temp6, 24);
tcg_gen_or_i32(temp0, temp0, temp15);
tcg_gen_or_i32(temp0, temp0, temp14);
tcg_gen_or_i32(temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp0, temp12);
tcg_gen_or_i32(temp0, temp0, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_or_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_extEhhhh
static void emit_qc_ext(DisasContext *ctx, TCGv_env env, int8_t vi_22, int8_t vi_16, int8_t vi_19, int8_t vi_4) {
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_19);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp14, vi_16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp0, ((-1ll << ((uint64_t) (uint32_t) (vi_22 + 1))) ^ -1));
tcg_gen_shli_i32(temp0, temp8, (31 - vi_22));
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_sari_i32(temp9, temp0, (31 - vi_22));
tcg_gen_movcond_i32(TCG_COND_EQ, temp0, tcg_constant_i32((vi_22 + 1)), tcg_constant_i32(32), temp8, temp9);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_extdEhhhh
static void emit_qc_extd(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_23, int8_t vi_4) {
TCGv_i32 temp22 = xqci_get_gpr(ctx, (vi_23 + 1));
TCGv_i64 temp12 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp12, temp22);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_shli_i64(temp3, temp12, 32ull);
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_23);
tcg_gen_extu_i32_i64(temp12, temp19);
tcg_gen_or_i64(temp12, temp3, temp12);
tcg_gen_shri_i64(temp3, temp12, ((uint64_t) (uint32_t) vi_14));
tcg_gen_andi_i64(temp3, temp3, ((-1ll << ((uint64_t) (uint32_t) (vi_17 + 1))) ^ -1ll));
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_extrl_i64_i32(temp8, temp3);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp8, (31 - vi_17));
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_sari_i32(temp9, temp0, (31 - vi_17));
tcg_gen_movcond_i32(TCG_COND_EQ, temp0, tcg_constant_i32((vi_17 + 1)), tcg_constant_i32(32), temp8, temp9);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_extdprEhhh
static void emit_qc_extdpr(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_28, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_28 + 1));
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_28);
TCGv_i32 temp22 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp22, 8);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp6, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_24);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label32 = gen_new_label();
TCGLabel * label33 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label32);
gen_set_label(label33);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp11, temp17, 63);
tcg_gen_shr_i32(temp6, temp16, temp11);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp13);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp11, temp3);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp10, temp11, -1);
tcg_gen_and_i32(temp10, temp6, temp10);
tcg_gen_sub_i32(temp11, tcg_constant_i32(32), temp13);
tcg_gen_shl_i32(temp6, temp10, temp11);
tcg_gen_sar_i32(temp11, temp6, temp11);
tcg_gen_movcond_i32(TCG_COND_GTU, temp6, temp8, tcg_constant_i32(31), temp10, temp11);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label32);
gen_set_label(label32);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_extdprhEhhh
static void emit_qc_extdprh(DisasContext *ctx, TCGv_env env, int8_t vi_25, int8_t vi_29, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_29 + 1));
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_29);
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp23, 24);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp11, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_25);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label33 = gen_new_label();
TCGLabel * label34 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label33);
gen_set_label(label34);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp18, 16);
tcg_gen_andi_i32(temp11, temp6, 63);
tcg_gen_shr_i32(temp6, temp16, temp11);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp13);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp11, temp3);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp10, temp11, -1);
tcg_gen_and_i32(temp10, temp6, temp10);
tcg_gen_sub_i32(temp11, tcg_constant_i32(32), temp13);
tcg_gen_shl_i32(temp6, temp10, temp11);
tcg_gen_sar_i32(temp11, temp6, temp11);
tcg_gen_movcond_i32(TCG_COND_GTU, temp6, temp8, tcg_constant_i32(31), temp10, temp11);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label33);
gen_set_label(label33);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_extdrEhhh
static void emit_qc_extdr(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_28, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_28 + 1));
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_28);
TCGv_i32 temp22 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp22, 16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp6, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_24);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label32 = gen_new_label();
TCGLabel * label33 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label32);
gen_set_label(label33);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp11, temp17, 63);
tcg_gen_shr_i32(temp6, temp16, temp11);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp13);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp11, temp3);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp10, temp11, -1);
tcg_gen_and_i32(temp10, temp6, temp10);
tcg_gen_sub_i32(temp11, tcg_constant_i32(32), temp13);
tcg_gen_shl_i32(temp6, temp10, temp11);
tcg_gen_sar_i32(temp11, temp6, temp11);
tcg_gen_movcond_i32(TCG_COND_GTU, temp6, temp8, tcg_constant_i32(31), temp10, temp11);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label32);
gen_set_label(label32);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_extduEhhhh
static void emit_qc_extdu(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_10, int8_t vi_19, int8_t vi_4) {
TCGv_i32 temp17 = xqci_get_gpr(ctx, (vi_19 + 1));
TCGv_i64 temp6 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp6, temp17);
TCGv_i64 temp9 = tcg_temp_new_i64();
tcg_gen_shli_i64(temp9, temp6, 32ull);
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_19);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp14);
tcg_gen_or_i64(temp3, temp9, temp3);
tcg_gen_shri_i64(temp3, temp3, ((uint64_t) (uint32_t) vi_10));
tcg_gen_andi_i64(temp3, temp3, ((-2ll << ((uint64_t) (uint32_t) vi_12)) ^ -1ll));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_extrl_i64_i32(temp0, temp3);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_extduprEhhh
static void emit_qc_extdupr(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_24, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_24 + 1));
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp18, 8);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp12, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp6, tcg_constant_i32(32));
temp12 = xqci_get_gpr(ctx, vi_20);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label28);
gen_set_label(label29);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp12, 63);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label28);
gen_set_label(label28);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState11qc_extduprhEhhh
static void emit_qc_extduprh(DisasContext *ctx, TCGv_env env, int8_t vi_21, int8_t vi_25, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_25 + 1));
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_21);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp19, 24);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp13, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp7, tcg_constant_i32(32));
temp13 = xqci_get_gpr(ctx, vi_21);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label29);
gen_set_label(label30);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp13, 16);
tcg_gen_andi_i32(temp7, temp6, 63);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label29);
gen_set_label(label29);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_extdurEhhh
static void emit_qc_extdur(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_24, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
xqci_get_gpr(ctx, (vi_24 + 1));
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp18, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp12, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp6, tcg_constant_i32(32));
temp12 = xqci_get_gpr(ctx, vi_20);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label28);
gen_set_label(label29);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp12, 63);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label28);
gen_set_label(label28);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_extuEhhhh
static void emit_qc_extu(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_11, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_14);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp9, vi_11);
tcg_gen_andi_i32(temp0, temp0, ((-2ll << ((uint64_t) (uint32_t) vi_16)) ^ -1));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_insbEhhhh
static void emit_qc_insb(DisasContext *ctx, TCGv_env env, int8_t vi_21, int8_t vi_17, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp13, ((((-2ll << ((uint64_t) (uint32_t) vi_21)) ^ -1ll) << ((uint64_t) (uint32_t) vi_17)) ^ -1));
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp8, vi_17);
tcg_gen_andi_i32(temp0, temp0, (((-2ll << ((uint64_t) (uint32_t) vi_21)) ^ -1ll) << ((uint64_t) (uint32_t) vi_17)));
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_insbhEhhhh
static void emit_qc_insbh(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_20, int8_t vi_13, int8_t vi_4) {
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LEU, tcg_constant_i32(((vi_24 + 1) + vi_20)), 32, label29);
gen_set_label(label30);
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp9, (32 - vi_20));
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp8, (-1 << (((vi_24 + 1) + vi_20) + -32)));
tcg_gen_andi_i32(temp0, temp0, ((-1 << (((vi_24 + 1) + vi_20) + -32)) ^ -1));
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label29);
gen_set_label(label29);
}

// void _ZN12CPUArchState9qc_insbhrEhhh
static void emit_qc_insbhr(DisasContext *ctx, TCGv_env env, int8_t vi_25, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp19, temp23, 16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp19, 63);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp17, temp8, tcg_constant_i32(32));
temp19 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp19, 31);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp17, temp0);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_GTU, temp9, temp6, 32);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_NE, temp7, temp17, 0);
tcg_gen_and_i32(temp9, temp7, temp9);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp9, 0, label28);
tcg_gen_br(label29);
gen_set_label(label28);
tcg_gen_addi_i32(temp9, temp6, -32);
tcg_gen_shl_i32(temp6, tcg_constant_i32(-1), temp9);
tcg_gen_xori_i32(temp7, temp6, -1);
temp8 = xqci_get_gpr(ctx, vi_4);
temp9 = xqci_get_gpr(ctx, vi_12);
tcg_gen_sub_i32(temp0, tcg_constant_i32(32), temp0);
tcg_gen_shr_i32(temp0, temp9, temp0);
tcg_gen_and_i32(temp6, temp8, temp6);
tcg_gen_and_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label29);
gen_set_label(label29);
}

// void _ZN12CPUArchState8qc_insbiEhhhh
static void emit_qc_insbi(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_16, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp12, ((((-2ll << ((uint64_t) (uint32_t) vi_20)) ^ -1ll) << ((uint64_t) (uint32_t) vi_16)) ^ -1));
tcg_gen_ori_i32(temp0, temp0, ((vi_10 << vi_16) & (((-2ll << ((uint64_t) (uint32_t) vi_20)) ^ -1ll) << ((uint64_t) (uint32_t) vi_16))));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_insbprEhhh
static void emit_qc_insbpr(DisasContext *ctx, TCGv_env env, int8_t vi_26, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp24 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp20 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp20, temp24, 8);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp20, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp7, tcg_constant_i32(32));
temp20 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp20, 31);
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label29);
gen_set_label(label30);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp16);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp6, temp7, -1);
tcg_gen_shl_i32(temp7, temp6, temp0);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp8, temp7, -1);
tcg_gen_and_i32(temp6, temp12, temp8);
temp8 = xqci_get_gpr(ctx, vi_10);
tcg_gen_shl_i32(temp0, temp8, temp0);
tcg_gen_and_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label29);
gen_set_label(label29);
}

// void _ZN12CPUArchState10qc_insbprhEhhh
static void emit_qc_insbprh(DisasContext *ctx, TCGv_env env, int8_t vi_27, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp25 = xqci_get_gpr(ctx, vi_27);
TCGv_i32 temp21 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp21, temp25, 24);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp21, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp0, tcg_constant_i32(32));
temp21 = xqci_get_gpr(ctx, vi_27);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp21, 16);
tcg_gen_andi_i32(temp0, temp7, 31);
TCGLabel * label30 = gen_new_label();
TCGLabel * label31 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label30);
gen_set_label(label31);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp16);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp6, temp7, -1);
tcg_gen_shl_i32(temp7, temp6, temp0);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp8, temp7, -1);
tcg_gen_and_i32(temp6, temp12, temp8);
temp8 = xqci_get_gpr(ctx, vi_10);
tcg_gen_shl_i32(temp0, temp8, temp0);
tcg_gen_and_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label30);
gen_set_label(label30);
}

// void _ZN12CPUArchState8qc_insbrEhhh
static void emit_qc_insbr(DisasContext *ctx, TCGv_env env, int8_t vi_26, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp24 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp20 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp20, temp24, 16);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp20, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp7, tcg_constant_i32(32));
temp20 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp20, 31);
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label29);
gen_set_label(label30);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp16);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp6, temp7, -1);
tcg_gen_shl_i32(temp7, temp6, temp0);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp8, temp7, -1);
tcg_gen_and_i32(temp6, temp12, temp8);
temp8 = xqci_get_gpr(ctx, vi_10);
tcg_gen_shl_i32(temp0, temp8, temp0);
tcg_gen_and_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label29);
gen_set_label(label29);
}

// void _ZN12CPUArchState9qc_insbriEthh
static void emit_qc_insbri(DisasContext *ctx, TCGv_env env, int16_t vi_9, int8_t vi_25, int8_t vi_4) {
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp19, temp23, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp19, 63);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp15, temp6, tcg_constant_i32(32));
temp19 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp19, 31);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp15, 0, label28);
gen_set_label(label29);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp15);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp6, temp3);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp0, temp6, -1);
tcg_gen_shl_i32(temp6, temp0, temp7);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_4);
tcg_gen_xori_i32(temp0, temp6, -1);
tcg_gen_and_i32(temp0, temp11, temp0);
tcg_gen_shl_i32(temp7, tcg_constant_i32(vi_9), temp7);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label28);
gen_set_label(label28);
}

// void _ZN12CPUArchState5qc_liEjh
static void emit_qc_li(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_7 << 12) >> 12));
}

// void _ZN12CPUArchState7qc_lieqEhhhh
static void emit_qc_lieq(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_NE, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState8qc_lieqiEhhhh
static void emit_qc_lieqi(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp12, ((int32_t) (int32_t) vi_13), label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_ligeEhhhh
static void emit_qc_lige(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LT, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState8qc_ligeiEhhhh
static void emit_qc_ligei(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp12, ((int32_t) (int32_t) vi_13), label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_ligeuEhhhh
static void emit_qc_ligeu(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LTU, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState9qc_ligeuiEhhhh
static void emit_qc_ligeui(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_17);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp12, vi_14, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_liltEhhhh
static void emit_qc_lilt(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GE, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState8qc_liltiEhhhh
static void emit_qc_lilti(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp12, ((int32_t) (int32_t) vi_13), label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_liltuEhhhh
static void emit_qc_liltu(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GEU, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState9qc_liltuiEhhhh
static void emit_qc_liltui(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_17);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp12, vi_14, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_lineEhhhh
static void emit_qc_line(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_15, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp12, temp13, label21);
gen_set_label(label22);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label21);
gen_set_label(label21);
}

// void _ZN12CPUArchState8qc_lineiEhhhh
static void emit_qc_linei(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp12, ((int32_t) (int32_t) vi_13), label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((int32_t) (vi_8 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState6qc_lrbEhhhh
static void emit_qc_lrb(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_19, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_19);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp7, temp11, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp7, temp10);
tcg_gen_qemu_ld_i32(temp7, temp0, ctx->mem_idx, MO_UB);
tcg_gen_shli_i32(temp0, temp7, 24);
tcg_gen_sari_i32(temp0, temp0, 24);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_lrbuEhhhh
static void emit_qc_lrbu(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_18, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp10, vi_12);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp0, temp9);
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_UB);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_lrhEhhhh
static void emit_qc_lrh(DisasContext *ctx, TCGv_env env, int8_t vi_14, int8_t vi_20, int8_t vi_17, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp7, temp12, vi_14);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp7, temp11);
tcg_gen_qemu_ld_i32(temp7, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_shli_i32(temp0, temp7, 16);
tcg_gen_sari_i32(temp0, temp0, 16);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_lrhuEhhhh
static void emit_qc_lrhu(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_19, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_19);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp11, vi_13);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp0, temp10);
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_LEUW);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_lrwEhhhh
static void emit_qc_lrw(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_18, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp10, vi_12);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp0, temp9);
tcg_gen_qemu_ld_i32(temp0, temp6, ctx->mem_idx, MO_LESL);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_muliaddEthh
static void emit_qc_muliadd(DisasContext *ctx, TCGv_env env, int16_t vi_10, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_muli_i32(temp0, temp7, ((int32_t) (vi_10 << 20) >> 20));
tcg_gen_add_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_mveqEhhhh
static void emit_qc_mveq(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_NE, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mveqiEhhhh
static void emit_qc_mveqi(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_12, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp11, ((int32_t) (int32_t) vi_12), label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState7qc_mvgeEhhhh
static void emit_qc_mvge(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LT, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mvgeiEhhhh
static void emit_qc_mvgei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_12, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp11, ((int32_t) (int32_t) vi_12), label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState8qc_mvgeuEhhhh
static void emit_qc_mvgeu(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LTU, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState9qc_mvgeuiEhhhh
static void emit_qc_mvgeui(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp11, vi_13, label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState7qc_mvltEhhhh
static void emit_qc_mvlt(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GE, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mvltiEhhhh
static void emit_qc_mvlti(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_12, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp11, ((int32_t) (int32_t) vi_12), label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState8qc_mvltuEhhhh
static void emit_qc_mvltu(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GEU, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState9qc_mvltuiEhhhh
static void emit_qc_mvltui(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp11, vi_13, label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState7qc_mvneEhhhh
static void emit_qc_mvne(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_14);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp11, temp12, label20);
gen_set_label(label21);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mvneiEhhhh
static void emit_qc_mvnei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_12, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGLabel * label19 = gen_new_label();
TCGLabel * label20 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp11, ((int32_t) (int32_t) vi_12), label19);
gen_set_label(label20);
TCGv_i32 temp0 = xqci_get_gpr(ctx, vi_7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
tcg_gen_br(label19);
gen_set_label(label19);
}

// void _ZN12CPUArchState7qc_normEhh
static void emit_qc_norm(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_4) {
TCGv_i32 temp23 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp15, temp23, 32);
TCGv_i32 temp22 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp6, temp22, -1);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp14, temp6, 32);
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp18, temp20, tcg_constant_i32(0), temp14, temp15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp18, -1);
tcg_gen_shl_i32(temp6, temp17, temp0);
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp10, temp12, tcg_constant_i32(0), temp14, temp15);
tcg_gen_sub_i32(temp0, tcg_constant_i32(1), temp10);
tcg_gen_andi_i32(temp6, temp6, -256);
tcg_gen_andi_i32(temp0, temp0, 255);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_normeuEhh
static void emit_qc_normeu(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp12, temp14, 32);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp12, 30);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shl_i32(temp0, temp10, temp6);
tcg_gen_sub_i32(temp6, tcg_constant_i32(0), temp6);
tcg_gen_andi_i32(temp0, temp0, -256);
tcg_gen_andi_i32(temp6, temp6, 254);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_normuEhh
static void emit_qc_normu(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp10, temp13, 32);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shl_i32(temp0, temp11, temp10);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp6, tcg_constant_i32(0), temp10);
tcg_gen_andi_i32(temp0, temp0, -256);
tcg_gen_andi_i32(temp6, temp6, 255);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_pcoredumpEv
static void emit_qc_pcoredump(DisasContext *ctx, TCGv_env env) {
xqci_syscall(ctx, 8, tcg_constant_i32(0));
}

// void _ZN12CPUArchState8qc_pexitEh
static void emit_qc_pexit(DisasContext *ctx, TCGv_env env, int8_t vi_3) {
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_3);
xqci_syscall(ctx, 12, temp1);
}

// void _ZN12CPUArchState8qc_ppregEh
static void emit_qc_ppreg(DisasContext *ctx, TCGv_env env, int8_t vi_3) {
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_3);
xqci_syscall(ctx, 2, temp1);
}

// void _ZN12CPUArchState9qc_ppregsEv
static void emit_qc_ppregs(DisasContext *ctx, TCGv_env env) {
xqci_syscall(ctx, 3, tcg_constant_i32(0));
}

// void _ZN12CPUArchState8qc_pputcEh
static void emit_qc_pputc(DisasContext *ctx, TCGv_env env, int8_t vi_3) {
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_3);
xqci_syscall(ctx, 4, temp1);
}

// void _ZN12CPUArchState9qc_pputciEh
static void emit_qc_pputci(DisasContext *ctx, TCGv_env env, int8_t vi_2) {
xqci_syscall(ctx, 5, tcg_constant_i32(vi_2));
}

// void _ZN12CPUArchState8qc_pputsEh
static void emit_qc_pputs(DisasContext *ctx, TCGv_env env, int8_t vi_3) {
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_3);
xqci_syscall(ctx, 6, temp1);
}

// void _ZN12CPUArchState11qc_psyscallEh
static void emit_qc_psyscall(DisasContext *ctx, TCGv_env env, int8_t vi_3) {
TCGv_i32 temp1 = xqci_get_gpr(ctx, vi_3);
xqci_syscall(ctx, 10, temp1);
}

// void _ZN12CPUArchState12qc_psyscalliEt
static void emit_qc_psyscalli(DisasContext *ctx, TCGv_env env, int16_t vi_2) {
xqci_syscall(ctx, 11, tcg_constant_i32(vi_2));
}

// void _ZN12CPUArchState12qc_selecteqiEhhhh
static void emit_qc_selecteqi(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_12, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_EQ, temp0, temp14, ((int32_t) (int32_t) vi_15));
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_12);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_10);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_selectieqEhhhh
static void emit_qc_selectieq(DisasContext *ctx, TCGv_env env, int8_t vi_19, int8_t vi_8, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_19);
TCGLabel * label24 = gen_new_label();
TCGLabel * label25 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp16, temp17, label24);
gen_set_label(label25);
tcg_gen_movi_i32(temp0, ((int32_t) (vi_12 << 27) >> 27));
TCGLabel * label28 = gen_new_label();
tcg_gen_br(label28);
gen_set_label(label24);
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_8);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label28);
gen_set_label(label28);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState13qc_selectieqiEhhhh
static void emit_qc_selectieqi(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_8, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_4);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, ((int32_t) (int32_t) vi_17), label23);
gen_set_label(label24);
tcg_gen_movi_i32(temp0, ((int32_t) (vi_12 << 27) >> 27));
TCGLabel * label27 = gen_new_label();
tcg_gen_br(label27);
gen_set_label(label23);
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_8);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label27);
gen_set_label(label27);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState13qc_selectiieqEhhhh
static void emit_qc_selectiieq(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_setcond_i32(TCG_COND_EQ, temp0, temp12, temp13);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), tcg_constant_i32(vi_8), tcg_constant_i32(vi_10));
tcg_gen_shli_i32(temp0, temp0, 27);
tcg_gen_sari_i32(temp0, temp0, 27);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState13qc_selectiineEhhhh
static void emit_qc_selectiine(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_10, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp12 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_setcond_i32(TCG_COND_EQ, temp0, temp12, temp13);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), tcg_constant_i32(vi_8), tcg_constant_i32(vi_10));
tcg_gen_shli_i32(temp0, temp0, 27);
tcg_gen_sari_i32(temp0, temp0, 27);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_selectineEhhhh
static void emit_qc_selectine(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_13, int8_t vi_9, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_20);
TCGLabel * label25 = gen_new_label();
TCGLabel * label26 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp17, temp18, label25);
gen_set_label(label26);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_13);
tcg_gen_mov_i32(temp0, temp11);
TCGLabel * label27 = gen_new_label();
tcg_gen_br(label27);
gen_set_label(label25);
tcg_gen_movi_i32(temp0, ((int32_t) (vi_9 << 27) >> 27));
tcg_gen_br(label27);
gen_set_label(label27);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState13qc_selectineiEhhhh
static void emit_qc_selectinei(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_13, int8_t vi_9, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_4);
TCGLabel * label24 = gen_new_label();
TCGLabel * label25 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp17, ((int32_t) (int32_t) vi_18), label24);
gen_set_label(label25);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_13);
tcg_gen_mov_i32(temp0, temp11);
TCGLabel * label26 = gen_new_label();
tcg_gen_br(label26);
gen_set_label(label24);
tcg_gen_movi_i32(temp0, ((int32_t) (vi_9 << 27) >> 27));
tcg_gen_br(label26);
gen_set_label(label26);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_selectneiEhhhh
static void emit_qc_selectnei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_10, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_4);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_EQ, temp0, temp14, ((int32_t) (int32_t) vi_15));
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_12);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_10);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_setintiEt
static void emit_qc_setinti(DisasContext *ctx, TCGv_env env, int16_t vi_9) {
TCGv_i32 temp3 = xqci_csrr(ctx, env, (((uint16_t) vi_9 >> 5) + 2032));
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_ori_i32(temp2, temp3, (1 << (vi_9 & 31)));
xqci_csrw(ctx, env, (((uint16_t) vi_9 >> 5) + 2032), temp2);
}

// void _ZN12CPUArchState9qc_shladdEhhhh
static void emit_qc_shladd(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_8, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp10, vi_12);
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_8);
tcg_gen_add_i32(temp0, temp6, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_shlsatEhhh
static void emit_qc_shlsat(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_17);
TCGv_i64 temp6 = tcg_temp_new_i64();
tcg_gen_ext_i32_i64(temp6, temp15);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp10, 31);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp0);
tcg_gen_shl_i64(temp3, temp6, temp3);
TCGv_i64 temp7 = tcg_temp_new_i64();
tcg_gen_smin_i64(temp7, temp3, tcg_constant_i64(2147483647ull));
tcg_gen_smax_i64(temp6, temp7, tcg_constant_i64(-2147483648ll));
tcg_gen_extrl_i64_i32(temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_shlusatEhhh
static void emit_qc_shlusat(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_10);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp7, 31);
tcg_gen_shl_i32(temp0, temp6, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_srbEhhhh
static void emit_qc_srb(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_18, int8_t vi_15, int8_t vi_7) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp1, temp10, vi_12);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp1, temp9);
TCGv_i32 temp4 = xqci_get_gpr(ctx, vi_7);
tcg_gen_andi_i32(temp1, temp4, 255);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_UB);
}

// void _ZN12CPUArchState6qc_srhEhhhh
static void emit_qc_srh(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_18, int8_t vi_15, int8_t vi_7) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp1, temp10, vi_12);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp1, temp9);
TCGv_i32 temp4 = xqci_get_gpr(ctx, vi_7);
tcg_gen_andi_i32(temp1, temp4, 65535);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUW);
}

// void _ZN12CPUArchState6qc_srwEhhhh
static void emit_qc_srw(DisasContext *ctx, TCGv_env env, int8_t vi_10, int8_t vi_16, int8_t vi_13, int8_t vi_5) {
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp1, temp8, vi_10);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp1, temp7);
temp1 = xqci_get_gpr(ctx, vi_5);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUL);
}

// void _ZN12CPUArchState9qc_subsatEhhh
static void emit_qc_subsat(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_22, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp21 = xqci_get_gpr(ctx, vi_22);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp8, temp20, temp21);
TCGv_i32 temp18 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_22);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xor_i32(temp6, temp17, temp18);
tcg_gen_mov_i32(temp0, temp8);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GT, temp6, -1, label28);
gen_set_label(label29);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_24);
tcg_gen_xor_i32(temp6, temp16, temp8);
tcg_gen_mov_i32(temp0, temp8);
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GT, temp6, -1, label28);
gen_set_label(label30);
temp8 = xqci_get_gpr(ctx, vi_24);
tcg_gen_movcond_i32(TCG_COND_LT, temp6, temp8, tcg_constant_i32(0), tcg_constant_i32(INT32_MIN), tcg_constant_i32(2147483647));
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label28);
gen_set_label(label28);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_subusatEhhh
static void emit_qc_subusat(DisasContext *ctx, TCGv_env env, int8_t vi_18, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp14 = xqci_get_gpr(ctx, vi_16);
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LTU, temp13, temp14, label22);
gen_set_label(label23);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_18);
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp6, temp7, temp8);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label22);
gen_set_label(label22);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_wrapEhhh
static void emit_qc_wrap(DisasContext *ctx, TCGv_env env, int8_t vi_19, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp8 = xqci_get_gpr(ctx, vi_19);
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_16);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LT, temp8, temp15, label23);
gen_set_label(label24);
TCGv_i32 temp13 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp6, temp8, temp13);
tcg_gen_mov_i32(temp0, temp6);
TCGLabel * label25 = gen_new_label();
tcg_gen_br(label25);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, temp8);
TCGLabel * label26 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp8, 0, label25);
gen_set_label(label26);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_16);
tcg_gen_add_i32(temp6, temp7, temp8);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label25);
gen_set_label(label25);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_wrapiEthh
static void emit_qc_wrapi(DisasContext *ctx, TCGv_env env, int16_t vi_12, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp10, temp7, tcg_constant_i32(0), tcg_constant_i32(vi_12), tcg_constant_i32(0));
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp6, temp7, tcg_constant_i32(vi_12), temp10, tcg_constant_i32((0 - vi_12)));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_add_i32(temp0, temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

