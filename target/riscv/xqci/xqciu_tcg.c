#include "qemu/osdep.h"
#include "qemu/log.h"
#include "cpu.h"
#include "tcg/tcg-op.h"
#include "tcg/tcg-op-gvec.h"
#include "tcg/tcg.h"
#include "exec/exec-all.h"
#include "exec/helper-gen.h"

// void _ZN12CPUArchState9qc_addsatEhhh
static void emit_qc_addsat(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_17, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp15, temp9);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp9, 31);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp15, 31);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_setcond_i32(TCG_COND_NE, temp7, temp13, temp0);
tcg_gen_shri_i32(temp0, temp6, 31);
tcg_gen_setcond_i32(TCG_COND_EQ, temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp7, temp0);
tcg_gen_movcond_i32(TCG_COND_LT, temp7, temp9, tcg_constant_i32(0), tcg_constant_i32(INT32_MIN), tcg_constant_i32(2147483647));
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_addusatEhhh
static void emit_qc_addusat(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_add_i32(temp7, temp10, temp11);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_or_i32(temp8, temp10, temp11);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_LT, temp0, temp8, 0);
tcg_gen_setcondi_i32(TCG_COND_GT, temp8, temp7, -1);
tcg_gen_and_i32(temp0, temp0, temp8);
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), tcg_constant_i32(-1), temp7);
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
temp22 = xqci_csrr_field(ctx, env, 1860, 6144);
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
TCGv_i32 temp16 = xqci_csrr_field(ctx, env, 1860, 6144);
TCGLabel * label36 = gen_new_label();
TCGLabel * label37 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label36);
gen_set_label(label37);
TCGv_i32 temp14 = xqci_csrr_field(ctx, env, 1860, 6144);
TCGLabel * label38 = gen_new_label();
TCGLabel * label39 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp14, 1, label38);
gen_set_label(label39);
TCGv_i32 temp12 = xqci_csrr_field(ctx, env, 1860, 6144);
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
temp18 = xqci_csrr_field(ctx, env, 768, 6144);
TCGLabel * label39 = gen_new_label();
TCGLabel * label40 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp18, 3, label39);
gen_set_label(label40);
xqci_csrw_field(ctx, env, 768, 131072, tcg_constant_i32(0));
tcg_gen_br(label39);
gen_set_label(label39);
temp16 = xqci_csrr_field(ctx, env, 768, 6144);
TCGLabel * label41 = gen_new_label();
TCGLabel * label42 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label41);
gen_set_label(label42);
temp14 = xqci_csrr_field(ctx, env, 768, 6144);
TCGLabel * label43 = gen_new_label();
TCGLabel * label44 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp14, 1, label43);
gen_set_label(label44);
TCGv_i32 temp12 = xqci_csrr_field(ctx, env, 768, 6144);
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
static void emit_qc_compress2(DisasContext *ctx, TCGv_env env, int8_t vi_38, int8_t vi_4) {
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_38);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp17, temp19, 1);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp19, 1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp16, 2);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp19, 2);
tcg_gen_andi_i32(temp16, temp15, 4);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp14, temp19, 3);
tcg_gen_andi_i32(temp15, temp14, 8);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp19, 4);
tcg_gen_andi_i32(temp14, temp10, 16);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp19, 5);
tcg_gen_andi_i32(temp10, temp9, 32);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp19, 6);
tcg_gen_andi_i32(temp9, temp8, 64);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp19, 7);
tcg_gen_andi_i32(temp8, temp13, 128);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp19, 16);
tcg_gen_andi_i32(temp13, temp6, 1);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp19, 17);
tcg_gen_andi_i32(temp6, temp12, 2);
tcg_gen_or_i32(temp13, temp6, temp13);
tcg_gen_shri_i32(temp12, temp19, 18);
tcg_gen_andi_i32(temp6, temp12, 4);
tcg_gen_or_i32(temp6, temp13, temp6);
tcg_gen_shri_i32(temp12, temp19, 19);
tcg_gen_andi_i32(temp13, temp12, 8);
tcg_gen_or_i32(temp13, temp6, temp13);
tcg_gen_shri_i32(temp12, temp19, 20);
tcg_gen_andi_i32(temp6, temp12, 16);
tcg_gen_or_i32(temp6, temp13, temp6);
tcg_gen_shri_i32(temp12, temp19, 21);
tcg_gen_andi_i32(temp13, temp12, 32);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp19, 22);
tcg_gen_andi_i32(temp12, temp11, 64);
tcg_gen_shri_i32(temp11, temp19, 23);
tcg_gen_andi_i32(temp11, temp11, 128);
tcg_gen_or_i32(temp0, temp0, temp17);
tcg_gen_or_i32(temp0, temp0, temp16);
tcg_gen_or_i32(temp0, temp0, temp15);
tcg_gen_or_i32(temp0, temp0, temp14);
tcg_gen_or_i32(temp6, temp6, temp13);
tcg_gen_or_i32(temp6, temp6, temp12);
tcg_gen_or_i32(temp6, temp6, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_shli_i32(temp6, temp6, 8);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_compress3Ehh
static void emit_qc_compress3(DisasContext *ctx, TCGv_env env, int8_t vi_32, int8_t vi_4) {
TCGv_i32 temp17 = xqci_get_gpr(ctx, vi_32);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp14, temp17, 1);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp17, 2);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp13, 2);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp17, 4);
tcg_gen_andi_i32(temp13, temp12, 4);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp17, 6);
tcg_gen_andi_i32(temp12, temp11, 8);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp17, 8);
tcg_gen_andi_i32(temp11, temp10, 16);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp17, 10);
tcg_gen_andi_i32(temp10, temp9, 32);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp17, 12);
tcg_gen_andi_i32(temp9, temp8, 64);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp17, 14);
tcg_gen_andi_i32(temp8, temp6, 128);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp17, 24);
tcg_gen_andi_i32(temp6, temp15, 1);
tcg_gen_shri_i32(temp15, temp17, 26);
tcg_gen_andi_i32(temp15, temp15, 2);
tcg_gen_or_i32(temp6, temp15, temp6);
tcg_gen_shri_i32(temp15, temp17, 28);
tcg_gen_andi_i32(temp15, temp15, 4);
tcg_gen_or_i32(temp6, temp6, temp15);
tcg_gen_or_i32(temp0, temp0, temp14);
tcg_gen_or_i32(temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp0, temp12);
tcg_gen_or_i32(temp0, temp0, temp11);
tcg_gen_or_i32(temp0, temp0, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_shli_i32(temp6, temp6, 8);
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
static void emit_qc_expand2(DisasContext *ctx, TCGv_env env, int8_t vi_58, int8_t vi_4) {
TCGv_i32 temp46 = xqci_get_gpr(ctx, vi_58);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp46, 3);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp10, temp13, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp46, 2);
tcg_gen_andi_i32(temp13, temp9, 1);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp46, 1);
tcg_gen_andi_i32(temp9, temp8, 1);
TCGv_i32 temp30 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp30, temp46, 1);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp14, temp9, 2);
tcg_gen_shli_i32(temp8, temp9, 3);
TCGv_i32 temp28 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp28, temp13, 4);
tcg_gen_shli_i32(temp9, temp13, 5);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp7, temp10, 6);
tcg_gen_shli_i32(temp13, temp10, 7);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp46, 7);
tcg_gen_andi_i32(temp10, temp15, 1);
TCGv_i32 temp39 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp39, temp46, 6);
tcg_gen_andi_i32(temp15, temp39, 1);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp46, 5);
tcg_gen_andi_i32(temp39, temp6, 1);
TCGv_i32 temp29 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp29, temp46, 4);
tcg_gen_andi_i32(temp6, temp29, 1);
tcg_gen_muli_i32(temp29, temp30, 3);
tcg_gen_or_i32(temp29, temp14, temp29);
tcg_gen_or_i32(temp8, temp29, temp8);
tcg_gen_shri_i32(temp14, temp46, 11);
tcg_gen_shri_i32(temp30, temp46, 10);
tcg_gen_andi_i32(temp29, temp30, 1);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp46, 9);
tcg_gen_andi_i32(temp30, temp16, 1);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp17, temp46, 8);
tcg_gen_andi_i32(temp16, temp17, 1);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp46, 15);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp46, 14);
tcg_gen_andi_i32(temp17, temp18, 1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp46, 13);
tcg_gen_andi_i32(temp18, temp0, 1);
tcg_gen_shri_i32(temp0, temp46, 12);
tcg_gen_andi_i32(temp0, temp0, 1);
tcg_gen_or_i32(temp28, temp8, temp28);
tcg_gen_or_i32(temp9, temp28, temp9);
tcg_gen_shli_i32(temp8, temp15, 13);
tcg_gen_shli_i32(temp28, temp15, 12);
tcg_gen_shli_i32(temp15, temp39, 11);
tcg_gen_shli_i32(temp39, temp39, 10);
tcg_gen_muli_i32(temp6, temp6, 768);
tcg_gen_or_i32(temp6, temp6, temp39);
tcg_gen_or_i32(temp15, temp6, temp15);
tcg_gen_or_i32(temp28, temp15, temp28);
tcg_gen_or_i32(temp8, temp28, temp8);
tcg_gen_shli_i32(temp15, temp14, 23);
tcg_gen_shli_i32(temp28, temp14, 22);
tcg_gen_andi_i32(temp14, temp28, 4194304);
tcg_gen_shli_i32(temp6, temp29, 21);
tcg_gen_shli_i32(temp28, temp29, 20);
tcg_gen_shli_i32(temp29, temp30, 19);
tcg_gen_shli_i32(temp30, temp30, 18);
tcg_gen_muli_i32(temp16, temp16, 196608);
tcg_gen_or_i32(temp16, temp16, temp30);
tcg_gen_or_i32(temp16, temp16, temp29);
tcg_gen_or_i32(temp16, temp16, temp28);
tcg_gen_or_i32(temp6, temp16, temp6);
tcg_gen_andi_i32(temp16, temp15, 8388608);
tcg_gen_or_i32(temp16, temp14, temp16);
tcg_gen_or_i32(temp6, temp16, temp6);
tcg_gen_shli_i32(temp14, temp11, 31);
tcg_gen_shli_i32(temp16, temp11, 30);
tcg_gen_andi_i32(temp11, temp16, 1073741824);
tcg_gen_shli_i32(temp15, temp17, 29);
tcg_gen_shli_i32(temp16, temp17, 28);
tcg_gen_shli_i32(temp17, temp18, 27);
tcg_gen_shli_i32(temp18, temp18, 26);
tcg_gen_muli_i32(temp0, temp0, 50331648);
tcg_gen_or_i32(temp0, temp0, temp18);
tcg_gen_or_i32(temp0, temp0, temp17);
tcg_gen_or_i32(temp0, temp0, temp16);
tcg_gen_or_i32(temp0, temp0, temp15);
tcg_gen_or_i32(temp7, temp7, temp14);
tcg_gen_or_i32(temp7, temp7, temp13);
tcg_gen_muli_i32(temp10, temp10, 49152);
tcg_gen_or_i32(temp7, temp7, temp11);
tcg_gen_or_i32(temp7, temp7, temp10);
tcg_gen_or_i32(temp0, temp0, temp9);
tcg_gen_or_i32(temp0, temp0, temp8);
tcg_gen_or_i32(temp0, temp0, temp7);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_expand3Ehh
static void emit_qc_expand3(DisasContext *ctx, TCGv_env env, int8_t vi_53, int8_t vi_4) {
TCGv_i32 temp36 = xqci_get_gpr(ctx, vi_53);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp36, 2);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp17, temp15, 1);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp36, 1);
tcg_gen_andi_i32(temp15, temp10, 1);
TCGv_i32 temp27 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp27, temp36, 1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp15, 3);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp16, temp15, 4);
tcg_gen_shli_i32(temp10, temp15, 5);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp6, temp17, 6);
tcg_gen_shli_i32(temp15, temp17, 7);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp36, 5);
tcg_gen_andi_i32(temp17, temp18, 1);
TCGv_i32 temp34 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp34, temp36, 4);
tcg_gen_andi_i32(temp18, temp34, 1);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp36, 3);
tcg_gen_andi_i32(temp34, temp12, 1);
tcg_gen_muli_i32(temp12, temp27, 7);
tcg_gen_or_i32(temp0, temp12, temp0);
tcg_gen_shri_i32(temp27, temp36, 7);
tcg_gen_andi_i32(temp12, temp27, 1);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp19, temp36, 6);
tcg_gen_andi_i32(temp27, temp19, 1);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp36, 10);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp36, 9);
tcg_gen_andi_i32(temp19, temp9, 1);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp36, 8);
tcg_gen_andi_i32(temp9, temp11, 1);
tcg_gen_or_i32(temp16, temp0, temp16);
tcg_gen_or_i32(temp10, temp16, temp10);
tcg_gen_shli_i32(temp0, temp17, 15);
tcg_gen_shli_i32(temp11, temp18, 14);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp7, temp18, 13);
tcg_gen_shli_i32(temp16, temp18, 12);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp8, temp34, 11);
TCGv_i32 temp33 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp33, temp34, 10);
tcg_gen_shli_i32(temp18, temp34, 9);
tcg_gen_shli_i32(temp34, temp36, 6);
tcg_gen_andi_i32(temp34, temp34, 256);
tcg_gen_or_i32(temp18, temp18, temp34);
tcg_gen_or_i32(temp18, temp18, temp33);
tcg_gen_or_i32(temp8, temp18, temp8);
tcg_gen_or_i32(temp16, temp8, temp16);
tcg_gen_or_i32(temp7, temp16, temp7);
tcg_gen_shli_i32(temp8, temp12, 21);
tcg_gen_shli_i32(temp18, temp27, 20);
tcg_gen_shli_i32(temp16, temp27, 19);
tcg_gen_shli_i32(temp27, temp27, 18);
tcg_gen_muli_i32(temp17, temp17, 196608);
tcg_gen_or_i32(temp17, temp17, temp27);
tcg_gen_or_i32(temp16, temp17, temp16);
tcg_gen_or_i32(temp18, temp16, temp18);
tcg_gen_or_i32(temp8, temp18, temp8);
tcg_gen_shli_i32(temp16, temp13, 31);
tcg_gen_shli_i32(temp18, temp13, 30);
tcg_gen_andi_i32(temp13, temp18, 1073741824);
tcg_gen_shli_i32(temp17, temp19, 29);
tcg_gen_shli_i32(temp18, temp19, 28);
tcg_gen_shli_i32(temp19, temp19, 27);
tcg_gen_muli_i32(temp9, temp9, 117440512);
tcg_gen_or_i32(temp9, temp9, temp19);
tcg_gen_or_i32(temp9, temp9, temp18);
tcg_gen_or_i32(temp9, temp9, temp17);
tcg_gen_or_i32(temp6, temp6, temp16);
tcg_gen_or_i32(temp6, temp6, temp15);
tcg_gen_muli_i32(temp12, temp12, 12582912);
tcg_gen_or_i32(temp6, temp6, temp13);
tcg_gen_or_i32(temp6, temp6, temp12);
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
static void emit_qc_extdpr(DisasContext *ctx, TCGv_env env, int8_t vi_26, int8_t vi_18, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp20, 8);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp6, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label30 = gen_new_label();
TCGLabel * label31 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label30);
gen_set_label(label31);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp11, temp20, 63);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_18);
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
tcg_gen_br(label30);
gen_set_label(label30);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_extdprhEhhh
static void emit_qc_extdprh(DisasContext *ctx, TCGv_env env, int8_t vi_27, int8_t vi_18, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp21 = xqci_get_gpr(ctx, vi_27);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp21, 24);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp11, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label31 = gen_new_label();
TCGLabel * label32 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label31);
gen_set_label(label32);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp21, 16);
tcg_gen_andi_i32(temp11, temp6, 63);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_18);
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
tcg_gen_br(label31);
gen_set_label(label31);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_extdrEhhh
static void emit_qc_extdr(DisasContext *ctx, TCGv_env env, int8_t vi_26, int8_t vi_18, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp20, 16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp6, 63);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp13, temp8, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label30 = gen_new_label();
TCGLabel * label31 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, 0, label30);
gen_set_label(label31);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp11, temp20, 63);
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_18);
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
tcg_gen_br(label30);
gen_set_label(label30);
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
static void emit_qc_extdupr(DisasContext *ctx, TCGv_env env, int8_t vi_22, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_22);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp15, 8);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp7, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp6, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label26 = gen_new_label();
TCGLabel * label27 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label26);
gen_set_label(label27);
tcg_gen_andi_i32(temp7, temp15, 63);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_13);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label26);
gen_set_label(label26);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState11qc_extduprhEhhh
static void emit_qc_extduprh(DisasContext *ctx, TCGv_env env, int8_t vi_23, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp16 = xqci_get_gpr(ctx, vi_23);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp16, 24);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp6, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp7, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label27 = gen_new_label();
TCGLabel * label28 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label27);
gen_set_label(label28);
tcg_gen_shri_i32(temp6, temp16, 16);
tcg_gen_andi_i32(temp7, temp6, 63);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_13);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label27);
gen_set_label(label27);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_extdurEhhh
static void emit_qc_extdur(DisasContext *ctx, TCGv_env env, int8_t vi_22, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_22);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp15, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp7, 63);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp10, temp6, tcg_constant_i32(32));
tcg_gen_movi_i32(temp0, 0);
TCGLabel * label26 = gen_new_label();
TCGLabel * label27 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp10, 0, label26);
gen_set_label(label27);
tcg_gen_andi_i32(temp7, temp15, 63);
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_13);
tcg_gen_shr_i32(temp6, temp11, temp7);
TCGv_i64 temp3 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp3, temp10);
tcg_gen_shl_i64(temp3, tcg_constant_i64(-1ll), temp3);
tcg_gen_extrl_i64_i32(temp7, temp3);
tcg_gen_xori_i32(temp7, temp7, -1);
tcg_gen_and_i32(temp6, temp6, temp7);
tcg_gen_mov_i32(temp0, temp6);
tcg_gen_br(label26);
gen_set_label(label26);
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
static void emit_qc_insbhr(DisasContext *ctx, TCGv_env env, int8_t vi_24, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp19, 16);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp8, temp0, 63);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp17, temp8, tcg_constant_i32(32));
tcg_gen_andi_i32(temp0, temp19, 31);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_add_i32(temp6, temp17, temp0);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_GTU, temp9, temp6, 32);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_setcondi_i32(TCG_COND_NE, temp7, temp17, 0);
tcg_gen_and_i32(temp9, temp7, temp9);
TCGLabel * label27 = gen_new_label();
TCGLabel * label28 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp9, 0, label27);
tcg_gen_br(label28);
gen_set_label(label27);
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
tcg_gen_br(label28);
gen_set_label(label28);
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
static void emit_qc_insbpr(DisasContext *ctx, TCGv_env env, int8_t vi_25, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp20, 8);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp0, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp7, tcg_constant_i32(32));
tcg_gen_andi_i32(temp0, temp20, 31);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label28);
gen_set_label(label29);
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
tcg_gen_br(label28);
gen_set_label(label28);
}

// void _ZN12CPUArchState10qc_insbprhEhhh
static void emit_qc_insbprh(DisasContext *ctx, TCGv_env env, int8_t vi_26, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp21 = xqci_get_gpr(ctx, vi_26);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp21, 24);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp7, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp0, tcg_constant_i32(32));
tcg_gen_shri_i32(temp7, temp21, 16);
tcg_gen_andi_i32(temp0, temp7, 31);
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

// void _ZN12CPUArchState8qc_insbrEhhh
static void emit_qc_insbr(DisasContext *ctx, TCGv_env env, int8_t vi_25, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp20 = xqci_get_gpr(ctx, vi_25);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp20, 16);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp7, temp0, 63);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp16, temp7, tcg_constant_i32(32));
tcg_gen_andi_i32(temp0, temp20, 31);
TCGLabel * label28 = gen_new_label();
TCGLabel * label29 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label28);
gen_set_label(label29);
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
tcg_gen_br(label28);
gen_set_label(label28);
}

// void _ZN12CPUArchState9qc_insbriEthh
static void emit_qc_insbri(DisasContext *ctx, TCGv_env env, int16_t vi_9, int8_t vi_24, int8_t vi_4) {
TCGv_i32 temp19 = xqci_get_gpr(ctx, vi_24);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp19, 16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp7, 63);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_umin_i32(temp15, temp6, tcg_constant_i32(32));
tcg_gen_andi_i32(temp7, temp19, 31);
TCGLabel * label27 = gen_new_label();
TCGLabel * label28 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp15, 0, label27);
gen_set_label(label28);
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
tcg_gen_br(label27);
gen_set_label(label27);
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
static void emit_qc_norm(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_sari_i32(temp6, temp9, 31);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_xor_i32(temp0, temp6, temp9);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp10, temp0, 32);
tcg_gen_sub_i32(temp6, tcg_constant_i32(1), temp10);
tcg_gen_addi_i32(temp0, temp10, -1);
tcg_gen_shl_i32(temp0, temp9, temp0);
tcg_gen_andi_i32(temp0, temp0, -256);
tcg_gen_andi_i32(temp6, temp6, 255);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_normeuEhh
static void emit_qc_normeu(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_4) {
TCGv_i32 temp10 = xqci_get_gpr(ctx, vi_15);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp11, temp10, 32);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp11, 30);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shl_i32(temp0, temp10, temp6);
tcg_gen_sub_i32(temp6, tcg_constant_i32(0), temp6);
tcg_gen_andi_i32(temp0, temp0, -256);
tcg_gen_andi_i32(temp6, temp6, 254);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_normuEhh
static void emit_qc_normu(DisasContext *ctx, TCGv_env env, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp11 = xqci_get_gpr(ctx, vi_14);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp10, temp11, 32);
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
static void emit_qc_subsat(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_17, int8_t vi_4) {
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_20);
TCGv_i32 temp15 = xqci_get_gpr(ctx, vi_17);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp6, temp9, temp15);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp9, 31);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp15, 31);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_setcond_i32(TCG_COND_EQ, temp7, temp13, temp0);
tcg_gen_shri_i32(temp0, temp6, 31);
tcg_gen_setcond_i32(TCG_COND_EQ, temp0, temp0, temp13);
tcg_gen_or_i32(temp0, temp7, temp0);
tcg_gen_movcond_i32(TCG_COND_LT, temp7, temp9, tcg_constant_i32(0), tcg_constant_i32(INT32_MIN), tcg_constant_i32(2147483647));
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp0, tcg_constant_i32(0), temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_subusatEhhh
static void emit_qc_subusat(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_9, int8_t vi_4) {
TCGv_i32 temp6 = xqci_get_gpr(ctx, vi_12);
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_9);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ussub_i32(temp0, temp6, temp7);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_wrapEhhh
static void emit_qc_wrap(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp7 = xqci_get_gpr(ctx, vi_16);
TCGv_i32 temp9 = xqci_get_gpr(ctx, vi_13);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp10, temp7, tcg_constant_i32(0), temp9, tcg_constant_i32(0));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp0, tcg_constant_i32(0), temp9);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp6, temp7, temp9, temp10, temp0);
tcg_gen_add_i32(temp0, temp6, temp7);
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

