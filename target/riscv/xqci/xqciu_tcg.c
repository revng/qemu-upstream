#include "qemu/osdep.h"
#include "qemu/log.h"
#include "cpu.h"
#include "tcg/tcg-op.h"
#include "tcg/tcg-op-gvec.h"
#include "tcg/tcg.h"
#include "exec/exec-all.h"
#include "exec/helper-gen.h"

#include "xqciu_tcg.h"

// void _ZN12CPUArchState7qc_beqiEthh
void emit_qc_beqi(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState7qc_bgeiEthh
void emit_qc_bgei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState8qc_bgeuiEthh
void emit_qc_bgeui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_11, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp0, vi_11, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState7qc_bltiEthh
void emit_qc_blti(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState8qc_bltuiEthh
void emit_qc_bltui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_11, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp0, vi_11, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState7qc_bneiEthh
void emit_qc_bnei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int8_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState10qc_c_bextiEhh
void emit_qc_c_bexti(DisasContext *ctx, TCGv_env env, int8_t vi_9, int8_t vi_7) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[(((uint64_t) (uint32_t) vi_7) + 8ull)]);
tcg_gen_shri_i32(temp0, temp0, (vi_9 & 31));
tcg_gen_andi_i32(temp0, temp0, 1);
tcg_gen_mov_i32(cpu_gpr[(((uint64_t) (uint32_t) vi_7) + 8ull)], temp0);
}

// void _ZN12CPUArchState10qc_c_bsetiEhh
void emit_qc_c_bseti(DisasContext *ctx, TCGv_env env, int8_t vi_9, int8_t vi_7) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[(((uint64_t) (uint32_t) vi_7) + 8ull)]);
tcg_gen_ori_i32(temp0, temp0, (1 << (vi_9 & 31)));
tcg_gen_mov_i32(cpu_gpr[(((uint64_t) (uint32_t) vi_7) + 8ull)], temp0);
}

// void _ZN12CPUArchState11qc_c_clrintEh
void emit_qc_c_clrint(DisasContext *ctx, TCGv_env env, int8_t vi_13) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 324ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_addi_i32(temp0, temp0, ((int8_t) vi_13 >> 5));
TCGv_i64 temp6 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp6, temp0);
tcg_gen_muli_i64(temp6, temp6, 24ull);
tcg_gen_addi_i64(temp6, temp6, 132ull);
tcg_gen_addi_i64(temp6, temp6, 0ull);
TCGv_ptr ptr18 = tcg_temp_new_ptr();
tcg_gen_trunc_i64_ptr(ptr18, temp6);
tcg_gen_add_ptr(ptr1, env, ptr18);
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_andi_i32(temp0, temp0, ((1 << (vi_13 & 31)) ^ -1));
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState7qc_c_diEv
void emit_qc_c_di(DisasContext *ctx, TCGv_env env) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 516ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_andi_i32(temp0, temp0, -9);
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState8qc_c_dirEh
void emit_qc_c_dir(DisasContext *ctx, TCGv_env env, int8_t vi_4) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 516ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp6, temp0, -9);
tcg_gen_st_i32(temp6, ptr1, 0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_c_eiEv
void emit_qc_c_ei(DisasContext *ctx, TCGv_env env) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 516ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_ori_i32(temp0, temp0, 8);
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState8qc_c_eirEh
void emit_qc_c_eir(DisasContext *ctx, TCGv_env env, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 516ull);
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState9qc_c_extuEhh
void emit_qc_c_extu(DisasContext *ctx, TCGv_env env, int8_t vi_9, int8_t vi_7) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_7)]);
tcg_gen_andi_i32(temp0, temp0, ((2 << vi_9) + -1));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_7)], temp0);
}

// void _ZN12CPUArchState11qc_c_setintEh
void emit_qc_c_setint(DisasContext *ctx, TCGv_env env, int8_t vi_12) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 324ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_addi_i32(temp0, temp0, ((int8_t) vi_12 >> 5));
TCGv_i64 temp5 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp5, temp0);
tcg_gen_muli_i64(temp5, temp5, 24ull);
tcg_gen_addi_i64(temp5, temp5, 132ull);
tcg_gen_addi_i64(temp5, temp5, 0ull);
TCGv_ptr ptr17 = tcg_temp_new_ptr();
tcg_gen_trunc_i64_ptr(ptr17, temp5);
tcg_gen_add_ptr(ptr1, env, ptr17);
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_ori_i32(temp0, temp0, (1 << (vi_12 & 31)));
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState6qc_cloEhh
void emit_qc_clo(DisasContext *ctx, TCGv_env env, int8_t vi_8, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_xori_i32(temp0, temp0, -1);
tcg_gen_clzi_i32(temp0, temp0, 32);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_clrintiEt
void emit_qc_clrinti(DisasContext *ctx, TCGv_env env, int16_t vi_13) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 324ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_addi_i32(temp0, temp0, ((int16_t) vi_13 >> 5));
TCGv_i64 temp6 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp6, temp0);
tcg_gen_muli_i64(temp6, temp6, 24ull);
tcg_gen_addi_i64(temp6, temp6, 132ull);
tcg_gen_addi_i64(temp6, temp6, 0ull);
TCGv_ptr ptr18 = tcg_temp_new_ptr();
tcg_gen_trunc_i64_ptr(ptr18, temp6);
tcg_gen_add_ptr(ptr1, env, ptr18);
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_andi_i32(temp0, temp0, ((1 << (vi_13 & 31)) ^ -1));
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState12qc_compress2Ehh
void emit_qc_compress2(DisasContext *ctx, TCGv_env env, int8_t vi_36, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_36)]);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp17, temp11, 1);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp11, 1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp16, 2);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp11, 2);
tcg_gen_andi_i32(temp16, temp15, 4);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp14, temp11, 3);
tcg_gen_andi_i32(temp15, temp14, 8);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp11, 4);
tcg_gen_andi_i32(temp14, temp10, 16);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp11, 5);
tcg_gen_andi_i32(temp10, temp9, 32);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp11, 6);
tcg_gen_andi_i32(temp9, temp8, 64);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp11, 7);
tcg_gen_andi_i32(temp8, temp13, 128);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp11, 16);
tcg_gen_andi_i32(temp13, temp6, 1);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp11, 17);
tcg_gen_andi_i32(temp6, temp12, 2);
tcg_gen_or_i32(temp13, temp6, temp13);
tcg_gen_shri_i32(temp12, temp11, 18);
tcg_gen_andi_i32(temp6, temp12, 4);
tcg_gen_or_i32(temp6, temp13, temp6);
tcg_gen_shri_i32(temp12, temp11, 19);
tcg_gen_andi_i32(temp13, temp12, 8);
tcg_gen_or_i32(temp13, temp6, temp13);
tcg_gen_shri_i32(temp12, temp11, 20);
tcg_gen_andi_i32(temp6, temp12, 16);
tcg_gen_or_i32(temp6, temp13, temp6);
tcg_gen_shri_i32(temp12, temp11, 21);
tcg_gen_andi_i32(temp13, temp12, 32);
tcg_gen_shri_i32(temp12, temp11, 22);
tcg_gen_andi_i32(temp12, temp12, 64);
tcg_gen_shri_i32(temp11, temp11, 23);
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
void emit_qc_compress3(DisasContext *ctx, TCGv_env env, int8_t vi_31, int8_t vi_4) {
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp15, cpu_gpr[((uint64_t) (uint32_t) vi_31)]);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp14, temp15, 1);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp13, temp15, 2);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp0, temp13, 2);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp12, temp15, 4);
tcg_gen_andi_i32(temp13, temp12, 4);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp15, 6);
tcg_gen_andi_i32(temp12, temp11, 8);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp15, 8);
tcg_gen_andi_i32(temp11, temp10, 16);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp15, 10);
tcg_gen_andi_i32(temp10, temp9, 32);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp15, 12);
tcg_gen_andi_i32(temp9, temp8, 64);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp15, 14);
tcg_gen_andi_i32(temp8, temp18, 128);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp15, 24);
tcg_gen_andi_i32(temp18, temp6, 1);
tcg_gen_shri_i32(temp6, temp15, 26);
tcg_gen_andi_i32(temp6, temp6, 2);
tcg_gen_or_i32(temp6, temp6, temp18);
tcg_gen_shri_i32(temp15, temp15, 28);
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
void emit_qc_cto(DisasContext *ctx, TCGv_env env, int8_t vi_9, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_9)]);
tcg_gen_xori_i32(temp0, temp0, -1);
tcg_gen_ctzi_i32(temp0, temp0, 32);
tcg_gen_sub_i32(temp0, tcg_constant_i32(30), temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_e_addaiEjh
void emit_qc_e_addai(DisasContext *ctx, TCGv_env env, int32_t vi_2, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_addi_i32(temp0, temp0, vi_2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
}

// void _ZN12CPUArchState9qc_e_addiEjhh
void emit_qc_e_addi(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_addi_i32(temp0, temp0, ((uint32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_e_andaiEjh
void emit_qc_e_andai(DisasContext *ctx, TCGv_env env, int32_t vi_2, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_andi_i32(temp0, temp0, vi_2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
}

// void _ZN12CPUArchState9qc_e_andiEjhh
void emit_qc_e_andi(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_andi_i32(temp0, temp0, ((uint32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_beqiEtth
void emit_qc_e_beqi(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState9qc_e_bgeiEtth
void emit_qc_e_bgei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState10qc_e_bgeuiEtth
void emit_qc_e_bgeui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_11, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp0, vi_11, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState9qc_e_bltiEtth
void emit_qc_e_blti(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState10qc_e_bltuiEtth
void emit_qc_e_bltui(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_11, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp0, vi_11, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState9qc_e_bneiEtth
void emit_qc_e_bnei(DisasContext *ctx, TCGv_env env, int16_t vi_2, int16_t vi_9, int8_t vi_13) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label18 = gen_new_label();
TCGLabel * label19 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp0, vi_9, label18);
gen_set_label(label19);
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_2);
tcg_gen_br(label18);
gen_set_label(label18);
}

// void _ZN12CPUArchState6qc_e_jEj
void emit_qc_e_j(DisasContext *ctx, TCGv_env env, int32_t vi_1) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_pc);
xqci_jump_pcrel(ctx, temp0, vi_1);
}

// void _ZN12CPUArchState8qc_e_jalEj
void emit_qc_e_jal(DisasContext *ctx, TCGv_env env, int32_t vi_5) {
TCGv_i32 temp4 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp4, cpu_pc);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp4, 6);
xqci_jump_pcrel(ctx, temp4, vi_5);
tcg_gen_mov_i32(cpu_gpr[1ull], temp0);
}

// void _ZN12CPUArchState7qc_e_lbEjhh
void emit_qc_e_lb(DisasContext *ctx, TCGv_env env, int32_t vi_9, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
tcg_gen_addi_i32(temp0, temp0, vi_9);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_UB);
tcg_gen_shli_i32(temp0, temp0, 25);
tcg_gen_sari_i32(temp0, temp0, 25);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_e_lbuEjhh
void emit_qc_e_lbu(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_addi_i32(temp0, temp0, vi_8);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_UB);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_lhEjhh
void emit_qc_e_lh(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_addi_i32(temp0, temp0, vi_10);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_shli_i32(temp0, temp0, 17);
tcg_gen_sari_i32(temp0, temp0, 17);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_e_lhuEjhh
void emit_qc_e_lhu(DisasContext *ctx, TCGv_env env, int32_t vi_9, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
tcg_gen_addi_i32(temp0, temp0, vi_9);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_liEjh
void emit_qc_e_li(DisasContext *ctx, TCGv_env env, int32_t vi_0, int8_t vi_5) {
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_5)], vi_0);
}

// void _ZN12CPUArchState7qc_e_lwEjhh
void emit_qc_e_lw(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_addi_i32(temp0, temp0, vi_8);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LESL);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_e_oraiEjh
void emit_qc_e_orai(DisasContext *ctx, TCGv_env env, int32_t vi_2, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_ori_i32(temp0, temp0, vi_2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
}

// void _ZN12CPUArchState8qc_e_oriEjhh
void emit_qc_e_ori(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_ori_i32(temp0, temp0, ((uint32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_e_sbEjhh
void emit_qc_e_sb(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_8, int8_t vi_12) {
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp1, vi_10);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_andi_i32(temp1, temp1, 255);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_UB);
}

// void _ZN12CPUArchState7qc_e_shEjhh
void emit_qc_e_sh(DisasContext *ctx, TCGv_env env, int32_t vi_10, int8_t vi_8, int8_t vi_12) {
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp1, vi_10);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_andi_i32(temp1, temp1, 65535);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUW);
}

// void _ZN12CPUArchState7qc_e_swEjhh
void emit_qc_e_sw(DisasContext *ctx, TCGv_env env, int32_t vi_9, int8_t vi_7, int8_t vi_11) {
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp0, temp1, vi_9);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_7)]);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUL);
}

// void _ZN12CPUArchState10qc_e_xoraiEjh
void emit_qc_e_xorai(DisasContext *ctx, TCGv_env env, int32_t vi_2, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_xori_i32(temp0, temp0, vi_2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
}

// void _ZN12CPUArchState9qc_e_xoriEjhh
void emit_qc_e_xori(DisasContext *ctx, TCGv_env env, int32_t vi_8, int8_t vi_10, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_xori_i32(temp0, temp0, ((uint32_t) (vi_8 << 6) >> 6));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState10qc_expand2Ehh
void emit_qc_expand2(DisasContext *ctx, TCGv_env env, int8_t vi_56, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_56)]);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp0, 3);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp10, temp11, 1);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp9, temp0, 2);
tcg_gen_andi_i32(temp11, temp9, 1);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp8, temp0, 1);
tcg_gen_andi_i32(temp9, temp8, 1);
TCGv_i32 temp30 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp30, temp0, 1);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp14, temp9, 2);
tcg_gen_shli_i32(temp8, temp9, 3);
TCGv_i32 temp28 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp28, temp11, 4);
tcg_gen_shli_i32(temp9, temp11, 5);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp13, temp10, 6);
tcg_gen_shli_i32(temp11, temp10, 7);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp15, temp0, 7);
tcg_gen_andi_i32(temp10, temp15, 1);
TCGv_i32 temp39 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp39, temp0, 6);
tcg_gen_andi_i32(temp15, temp39, 1);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp0, 5);
tcg_gen_andi_i32(temp39, temp6, 1);
TCGv_i32 temp29 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp29, temp0, 4);
tcg_gen_andi_i32(temp6, temp29, 1);
tcg_gen_muli_i32(temp29, temp30, 3);
tcg_gen_or_i32(temp29, temp14, temp29);
tcg_gen_or_i32(temp8, temp29, temp8);
tcg_gen_shri_i32(temp14, temp0, 11);
tcg_gen_shri_i32(temp30, temp0, 10);
tcg_gen_andi_i32(temp29, temp30, 1);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp0, 9);
tcg_gen_andi_i32(temp30, temp16, 1);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp17, temp0, 8);
tcg_gen_andi_i32(temp16, temp17, 1);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp7, temp0, 15);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp0, 14);
tcg_gen_andi_i32(temp17, temp18, 1);
tcg_gen_shri_i32(temp18, temp0, 13);
tcg_gen_andi_i32(temp18, temp18, 1);
tcg_gen_shri_i32(temp0, temp0, 12);
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
tcg_gen_shli_i32(temp14, temp7, 31);
tcg_gen_shli_i32(temp16, temp7, 30);
tcg_gen_andi_i32(temp7, temp16, 1073741824);
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
void emit_qc_expand3(DisasContext *ctx, TCGv_env env, int8_t vi_51, int8_t vi_4) {
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp9, cpu_gpr[((uint64_t) (uint32_t) vi_51)]);
TCGv_i32 temp17 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp17, temp9, 2);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp12, temp17, 1);
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp10, temp9, 1);
tcg_gen_andi_i32(temp17, temp10, 1);
TCGv_i32 temp27 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp27, temp9, 1);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp17, 3);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp8, temp17, 4);
tcg_gen_shli_i32(temp10, temp17, 5);
TCGv_i32 temp18 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp18, temp9, 5);
tcg_gen_andi_i32(temp17, temp18, 1);
TCGv_i32 temp35 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp35, temp9, 4);
tcg_gen_andi_i32(temp18, temp35, 1);
TCGv_i32 temp16 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp16, temp9, 3);
tcg_gen_andi_i32(temp35, temp16, 1);
tcg_gen_muli_i32(temp16, temp27, 7);
tcg_gen_or_i32(temp0, temp16, temp0);
tcg_gen_shri_i32(temp27, temp9, 7);
tcg_gen_andi_i32(temp16, temp27, 1);
TCGv_i32 temp19 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp19, temp9, 6);
tcg_gen_andi_i32(temp27, temp19, 1);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp6, temp9, 10);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp11, temp9, 9);
tcg_gen_andi_i32(temp19, temp11, 1);
tcg_gen_shri_i32(temp11, temp9, 8);
tcg_gen_andi_i32(temp9, temp11, 1);
tcg_gen_or_i32(temp8, temp0, temp8);
tcg_gen_or_i32(temp10, temp8, temp10);
tcg_gen_shli_i32(temp0, temp17, 15);
tcg_gen_shli_i32(temp11, temp18, 14);
TCGv_i32 temp7 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp7, temp18, 13);
tcg_gen_shli_i32(temp8, temp18, 12);
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp13, temp35, 11);
TCGv_i32 temp15 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp15, temp35, 10);
tcg_gen_shli_i32(temp18, temp35, 9);
tcg_gen_shli_i32(temp35, temp12, 8);
tcg_gen_or_i32(temp18, temp18, temp35);
tcg_gen_or_i32(temp15, temp18, temp15);
tcg_gen_or_i32(temp13, temp15, temp13);
tcg_gen_or_i32(temp8, temp13, temp8);
tcg_gen_or_i32(temp7, temp8, temp7);
tcg_gen_shli_i32(temp13, temp16, 23);
tcg_gen_shli_i32(temp15, temp16, 22);
tcg_gen_shli_i32(temp8, temp16, 21);
tcg_gen_shli_i32(temp18, temp27, 20);
tcg_gen_shli_i32(temp16, temp27, 19);
tcg_gen_shli_i32(temp27, temp27, 18);
tcg_gen_muli_i32(temp17, temp17, 196608);
tcg_gen_or_i32(temp17, temp17, temp27);
tcg_gen_or_i32(temp16, temp17, temp16);
tcg_gen_or_i32(temp18, temp16, temp18);
tcg_gen_or_i32(temp8, temp18, temp8);
tcg_gen_shli_i32(temp16, temp6, 31);
tcg_gen_shli_i32(temp18, temp6, 30);
tcg_gen_andi_i32(temp6, temp18, 1073741824);
tcg_gen_shli_i32(temp17, temp19, 29);
tcg_gen_shli_i32(temp18, temp19, 28);
tcg_gen_shli_i32(temp19, temp19, 27);
tcg_gen_muli_i32(temp9, temp9, 117440512);
tcg_gen_or_i32(temp9, temp9, temp19);
tcg_gen_or_i32(temp9, temp9, temp18);
tcg_gen_or_i32(temp9, temp9, temp17);
tcg_gen_or_i32(temp6, temp6, temp16);
tcg_gen_or_i32(temp6, temp6, temp15);
tcg_gen_muli_i32(temp12, temp12, 192);
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
void emit_qc_ext(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_11, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp8, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp8, vi_11);
tcg_gen_andi_i32(temp0, temp0, ((2 << vi_15) + -1));
tcg_gen_shli_i32(temp0, temp0, (32 - vi_15));
tcg_gen_sari_i32(temp0, temp0, (32 - vi_15));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_extuEhhhh
void emit_qc_extu(DisasContext *ctx, TCGv_env env, int8_t vi_14, int8_t vi_10, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_shri_i32(temp0, temp0, vi_10);
tcg_gen_andi_i32(temp0, temp0, ((2 << vi_14) + -1));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_insbEhhhh
void emit_qc_insb(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_8, int8_t vi_11) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp0, ((((2 << vi_16) + -1) << vi_13) ^ -1));
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_shli_i32(temp0, temp0, vi_13);
tcg_gen_andi_i32(temp0, temp0, (((2 << vi_16) + -1) << vi_13));
tcg_gen_or_i32(temp0, temp0, temp2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_11)], temp0);
}

// void _ZN12CPUArchState8qc_insbhEhhhh
void emit_qc_insbh(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_17, int8_t vi_9, int8_t vi_11) {
TCGLabel * label25 = gen_new_label();
TCGLabel * label26 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LEU, tcg_constant_i32(((vi_20 + 1) + vi_17)), 32, label25);
gen_set_label(label26);
TCGv_i32 temp4 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp4, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp4, (-1 << (((vi_20 + 1) + vi_17) + -32)));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_9)]);
tcg_gen_shri_i32(temp0, temp0, (32 - vi_17));
tcg_gen_andi_i32(temp0, temp0, ((-1 << (((vi_20 + 1) + vi_17) + -32)) ^ -1));
tcg_gen_or_i32(temp0, temp0, temp2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_11)], temp0);
tcg_gen_br(label25);
gen_set_label(label25);
}

// void _ZN12CPUArchState9qc_insbhrEhhh
void emit_qc_insbhr(DisasContext *ctx, TCGv_env env, int8_t vi_20, int8_t vi_9, int8_t vi_11) {
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp2, cpu_gpr[((uint64_t) (uint32_t) vi_20)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp0, temp2, 16);
TCGv_i32 temp4 = tcg_temp_new_i32();
tcg_gen_addi_i32(temp4, temp0, 1);
tcg_gen_andi_i32(temp0, temp2, 65535);
tcg_gen_add_i32(temp2, temp4, temp0);
TCGLabel * label25 = gen_new_label();
TCGLabel * label26 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LEU, temp2, 32, label25);
gen_set_label(label26);
tcg_gen_addi_i32(temp4, temp2, -32);
tcg_gen_shl_i32(temp2, tcg_constant_i32(-1), temp4);
TCGv_i32 temp3 = tcg_temp_new_i32();
tcg_gen_xori_i32(temp3, temp2, -1);
tcg_gen_mov_i32(temp4, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
tcg_gen_and_i32(temp2, temp4, temp2);
tcg_gen_sub_i32(temp4, tcg_constant_i32(32), temp0);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_9)]);
tcg_gen_shr_i32(temp0, temp0, temp4);
tcg_gen_and_i32(temp0, temp0, temp3);
tcg_gen_or_i32(temp0, temp0, temp2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_11)], temp0);
tcg_gen_br(label25);
gen_set_label(label25);
}

// void _ZN12CPUArchState8qc_insbiEhhhh
void emit_qc_insbi(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_13, int8_t vi_5, int8_t vi_11) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
tcg_gen_andi_i32(temp0, temp0, ((((2 << vi_16) + -1) << vi_13) ^ -1));
tcg_gen_ori_i32(temp0, temp0, ((((2 << vi_16) + -1) << vi_13) & (vi_5 << vi_13)));
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_11)], temp0);
}

// void _ZN12CPUArchState9qc_insbprEhhh
void emit_qc_insbpr(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_8, int8_t vi_11) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_16)]);
TCGv_i32 temp4 = tcg_temp_new_i32();
tcg_gen_shri_i32(temp4, temp0, 8);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_andi_i32(temp2, temp4, 255);
tcg_gen_andi_i32(temp4, temp0, 255);
tcg_gen_shl_i32(temp0, tcg_constant_i32(2), temp2);
tcg_gen_addi_i32(temp2, temp0, -1);
tcg_gen_shl_i32(temp0, temp2, temp4);
TCGv_i32 temp3 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp3, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
tcg_gen_xori_i32(temp2, temp0, -1);
tcg_gen_and_i32(temp2, temp3, temp2);
tcg_gen_mov_i32(temp3, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_shl_i32(temp3, temp3, temp4);
tcg_gen_and_i32(temp0, temp0, temp3);
tcg_gen_or_i32(temp0, temp0, temp2);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_11)], temp0);
}

// void _ZN12CPUArchState5qc_liEjh
void emit_qc_li(DisasContext *ctx, TCGv_env env, int32_t vi_7, int8_t vi_4) {
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 12) >> 12));
}

// void _ZN12CPUArchState7qc_lieqEhhhh
void emit_qc_lieq(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_6, int8_t vi_4) {
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp10, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_NE, temp10, temp11, label22);
gen_set_label(label23);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], vi_6);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_lieqiEhhhh
void emit_qc_lieqi(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_ligeEhhhh
void emit_qc_lige(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LT, temp11, temp12, label22);
gen_set_label(label23);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_ligeiEhhhh
void emit_qc_ligei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_ligeuEhhhh
void emit_qc_ligeu(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_16)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LTU, temp11, temp12, label23);
gen_set_label(label24);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label23);
gen_set_label(label23);
}

// void _ZN12CPUArchState9qc_ligeuiEhhhh
void emit_qc_ligeui(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_liltEhhhh
void emit_qc_lilt(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GE, temp11, temp12, label22);
gen_set_label(label23);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_liltiEhhhh
void emit_qc_lilti(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_liltuEhhhh
void emit_qc_liltu(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_14, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_16)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GEU, temp11, temp12, label23);
gen_set_label(label24);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label23);
gen_set_label(label23);
}

// void _ZN12CPUArchState9qc_liltuiEhhhh
void emit_qc_liltui(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_lineEhhhh
void emit_qc_line(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp11, temp12, label22);
gen_set_label(label23);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_lineiEhhhh
void emit_qc_linei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_7, int8_t vi_4) {
TCGv_i32 temp11 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp11, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp11, vi_13, label20);
gen_set_label(label21);
tcg_gen_movi_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], ((uint32_t) (vi_7 << 27) >> 27));
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState6qc_lrbEhhhh
void emit_qc_lrb(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_15, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp9, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
tcg_gen_shli_i32(temp0, temp0, vi_11);
tcg_gen_add_i32(temp0, temp0, temp9);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_UB);
tcg_gen_shli_i32(temp0, temp0, 24);
tcg_gen_sari_i32(temp0, temp0, 24);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_lrbuEhhhh
void emit_qc_lrbu(DisasContext *ctx, TCGv_env env, int8_t vi_10, int8_t vi_14, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp8, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_shli_i32(temp0, temp0, vi_10);
tcg_gen_add_i32(temp0, temp0, temp8);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_UB);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_lrhEhhhh
void emit_qc_lrh(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_16, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp10 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp10, cpu_gpr[((uint64_t) (uint32_t) vi_16)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
tcg_gen_shli_i32(temp0, temp0, vi_12);
tcg_gen_add_i32(temp0, temp0, temp10);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_shli_i32(temp0, temp0, 16);
tcg_gen_sari_i32(temp0, temp0, 16);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_lrhuEhhhh
void emit_qc_lrhu(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_15, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp9, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
tcg_gen_shli_i32(temp0, temp0, vi_11);
tcg_gen_add_i32(temp0, temp0, temp9);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LEUW);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_lrwEhhhh
void emit_qc_lrw(DisasContext *ctx, TCGv_env env, int8_t vi_10, int8_t vi_14, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp8, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_shli_i32(temp0, temp0, vi_10);
tcg_gen_add_i32(temp0, temp0, temp8);
tcg_gen_qemu_ld_i32(temp0, temp0, ctx->mem_idx, MO_LESL);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState7qc_mveqEhhhh
void emit_qc_mveq(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_NE, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_mveqiEhhhh
void emit_qc_mveqi(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_mvgeEhhhh
void emit_qc_mvge(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LT, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_mvgeiEhhhh
void emit_qc_mvgei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LT, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mvgeuEhhhh
void emit_qc_mvgeu(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_LTU, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState9qc_mvgeuiEhhhh
void emit_qc_mvgeui(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_LTU, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_mvltEhhhh
void emit_qc_mvlt(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GE, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_mvltiEhhhh
void emit_qc_mvlti(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GE, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState8qc_mvltuEhhhh
void emit_qc_mvltu(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_GEU, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState9qc_mvltuiEhhhh
void emit_qc_mvltui(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_GEU, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_mvneEhhhh
void emit_qc_mvne(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp12 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp12, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGLabel * label22 = gen_new_label();
TCGLabel * label23 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp0, temp12, label22);
gen_set_label(label23);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label22);
gen_set_label(label22);
}

// void _ZN12CPUArchState8qc_mvneiEhhhh
void emit_qc_mvnei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_13, int8_t vi_8, int8_t vi_6) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label20 = gen_new_label();
TCGLabel * label21 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp0, vi_13, label20);
gen_set_label(label21);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_6)], temp0);
tcg_gen_br(label20);
gen_set_label(label20);
}

// void _ZN12CPUArchState7qc_normEhh
void emit_qc_norm(DisasContext *ctx, TCGv_env env, int8_t vi_14, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_sari_i32(temp9, temp0, 31);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_xor_i32(temp6, temp9, temp0);
tcg_gen_clzi_i32(temp9, temp6, 32);
tcg_gen_sub_i32(temp6, tcg_constant_i32(1), temp9);
tcg_gen_addi_i32(temp9, temp9, -1);
tcg_gen_shl_i32(temp0, temp0, temp9);
tcg_gen_andi_i32(temp6, temp6, 255);
tcg_gen_shli_i32(temp0, temp0, 8);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState9qc_normeuEhh
void emit_qc_normeu(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp6, temp0, 32);
tcg_gen_andi_i32(temp6, temp6, 30);
tcg_gen_shl_i32(temp0, temp0, temp6);
tcg_gen_sub_i32(temp6, tcg_constant_i32(0), temp6);
tcg_gen_andi_i32(temp6, temp6, 254);
tcg_gen_shli_i32(temp0, temp0, 8);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_normuEhh
void emit_qc_normu(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_clzi_i32(temp6, temp0, 32);
tcg_gen_shl_i32(temp0, temp0, temp6);
tcg_gen_sub_i32(temp6, tcg_constant_i32(0), temp6);
tcg_gen_andi_i32(temp6, temp6, 255);
tcg_gen_shli_i32(temp0, temp0, 8);
tcg_gen_or_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState12qc_selectieqEhhhh
void emit_qc_selectieq(DisasContext *ctx, TCGv_env env, int8_t vi_16, int8_t vi_6, int8_t vi_9, int8_t vi_18) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp13, cpu_gpr[((uint64_t) (uint32_t) vi_18)]);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp14, cpu_gpr[((uint64_t) (uint32_t) vi_16)]);
TCGLabel * label26 = gen_new_label();
TCGLabel * label27 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp13, temp14, label26);
gen_set_label(label27);
tcg_gen_movi_i32(temp0, ((uint32_t) (vi_9 << 27) >> 27));
TCGLabel * label30 = gen_new_label();
tcg_gen_br(label30);
gen_set_label(label26);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp2, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_mov_i32(temp0, temp2);
tcg_gen_br(label30);
gen_set_label(label30);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_18)], temp0);
}

// void _ZN12CPUArchState13qc_selectieqiEhhhh
void emit_qc_selectieqi(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_6, int8_t vi_9, int8_t vi_17) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp13 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp13, cpu_gpr[((uint64_t) (uint32_t) vi_17)]);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp13, vi_15, label23);
gen_set_label(label24);
tcg_gen_movi_i32(temp0, ((uint32_t) (vi_9 << 27) >> 27));
TCGLabel * label27 = gen_new_label();
tcg_gen_br(label27);
gen_set_label(label23);
TCGv_i32 temp2 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp2, cpu_gpr[((uint64_t) (uint32_t) vi_6)]);
tcg_gen_mov_i32(temp0, temp2);
tcg_gen_br(label27);
gen_set_label(label27);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_17)], temp0);
}

// void _ZN12CPUArchState13qc_selectiieqEhhhh
void emit_qc_selectiieq(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_4, int8_t vi_6, int8_t vi_14) {
TCGv_i32 temp3 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp3, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp8, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_setcond_i32(TCG_COND_EQ, temp3, temp3, temp8);
tcg_gen_movcond_i32(TCG_COND_NE, temp3, temp3, tcg_constant_i32(0), tcg_constant_i32(vi_4), tcg_constant_i32(vi_6));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp3, 27);
tcg_gen_sari_i32(temp0, temp0, 27);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_14)], temp0);
}

// void _ZN12CPUArchState13qc_selectiineEhhhh
void emit_qc_selectiine(DisasContext *ctx, TCGv_env env, int8_t vi_12, int8_t vi_6, int8_t vi_4, int8_t vi_14) {
TCGv_i32 temp3 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp3, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp8, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_setcond_i32(TCG_COND_EQ, temp3, temp3, temp8);
tcg_gen_movcond_i32(TCG_COND_NE, temp3, temp3, tcg_constant_i32(0), tcg_constant_i32(vi_4), tcg_constant_i32(vi_6));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp0, temp3, 27);
tcg_gen_sari_i32(temp0, temp0, 27);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_14)], temp0);
}

// void _ZN12CPUArchState12qc_selectineEhhhh
void emit_qc_selectine(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_10, int8_t vi_4, int8_t vi_17) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_17)]);
TCGv_i32 temp14 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp14, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGLabel * label25 = gen_new_label();
TCGLabel * label26 = gen_new_label();
tcg_gen_brcond_i32(TCG_COND_EQ, temp6, temp14, label25);
gen_set_label(label26);
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_mov_i32(temp0, temp6);
TCGLabel * label29 = gen_new_label();
tcg_gen_br(label29);
gen_set_label(label25);
tcg_gen_movi_i32(temp0, ((uint32_t) (vi_4 << 27) >> 27));
tcg_gen_br(label29);
gen_set_label(label29);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_17)], temp0);
}

// void _ZN12CPUArchState13qc_selectineiEhhhh
void emit_qc_selectinei(DisasContext *ctx, TCGv_env env, int8_t vi_15, int8_t vi_10, int8_t vi_4, int8_t vi_17) {
TCGv_i32 temp0 = tcg_temp_new_i32();
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_17)]);
TCGLabel * label23 = gen_new_label();
TCGLabel * label24 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp6, vi_15, label23);
gen_set_label(label24);
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_10)]);
tcg_gen_mov_i32(temp0, temp6);
TCGLabel * label27 = gen_new_label();
tcg_gen_br(label27);
gen_set_label(label23);
tcg_gen_movi_i32(temp0, ((uint32_t) (vi_4 << 27) >> 27));
tcg_gen_br(label27);
gen_set_label(label27);
tcg_gen_mov_i32(temp0, temp0);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_17)], temp0);
}

// void _ZN12CPUArchState10qc_setintiEt
void emit_qc_setinti(DisasContext *ctx, TCGv_env env, int16_t vi_12) {
TCGv_ptr ptr1 = tcg_temp_new_ptr();
tcg_gen_addi_ptr(ptr1, env, 324ull);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_addi_i32(temp0, temp0, ((int16_t) vi_12 >> 5));
TCGv_i64 temp5 = tcg_temp_new_i64();
tcg_gen_extu_i32_i64(temp5, temp0);
tcg_gen_muli_i64(temp5, temp5, 24ull);
tcg_gen_addi_i64(temp5, temp5, 132ull);
tcg_gen_addi_i64(temp5, temp5, 0ull);
TCGv_ptr ptr17 = tcg_temp_new_ptr();
tcg_gen_trunc_i64_ptr(ptr17, temp5);
tcg_gen_add_ptr(ptr1, env, ptr17);
tcg_gen_ld_i32(temp0, ptr1, 0);
tcg_gen_ori_i32(temp0, temp0, (1 << (vi_12 & 31)));
tcg_gen_st_i32(temp0, ptr1, 0);
}

// void _ZN12CPUArchState9qc_shladdEhhhh
void emit_qc_shladd(DisasContext *ctx, TCGv_env env, int8_t vi_9, int8_t vi_7, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_shli_i32(temp6, temp0, vi_9);
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_7)]);
tcg_gen_add_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState6qc_srbEhhhh
void emit_qc_srb(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_15, int8_t vi_13, int8_t vi_8) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
tcg_gen_shli_i32(temp1, temp1, vi_11);
tcg_gen_add_i32(temp0, temp1, temp0);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_andi_i32(temp1, temp1, 255);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_UB);
}

// void _ZN12CPUArchState6qc_srhEhhhh
void emit_qc_srh(DisasContext *ctx, TCGv_env env, int8_t vi_11, int8_t vi_15, int8_t vi_13, int8_t vi_8) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_15)]);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
tcg_gen_shli_i32(temp1, temp1, vi_11);
tcg_gen_add_i32(temp0, temp1, temp0);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_8)]);
tcg_gen_andi_i32(temp1, temp1, 65535);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUW);
}

// void _ZN12CPUArchState6qc_srwEhhhh
void emit_qc_srw(DisasContext *ctx, TCGv_env env, int8_t vi_10, int8_t vi_14, int8_t vi_12, int8_t vi_7) {
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_14)]);
TCGv_i32 temp1 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_12)]);
tcg_gen_shli_i32(temp1, temp1, vi_10);
tcg_gen_add_i32(temp0, temp1, temp0);
tcg_gen_mov_i32(temp1, cpu_gpr[((uint64_t) (uint32_t) vi_7)]);
tcg_gen_qemu_st_i32(temp1, temp0, ctx->mem_idx, MO_LEUL);
}

// void _ZN12CPUArchState7qc_wrapEhhh
void emit_qc_wrap(DisasContext *ctx, TCGv_env env, int8_t vi_13, int8_t vi_11, int8_t vi_4) {
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp0, cpu_gpr[((uint64_t) (uint32_t) vi_11)]);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp8, temp6, tcg_constant_i32(0), temp0, tcg_constant_i32(0));
TCGv_i32 temp9 = tcg_temp_new_i32();
tcg_gen_sub_i32(temp9, tcg_constant_i32(0), temp0);
tcg_gen_movcond_i32(TCG_COND_LT, temp0, temp6, temp0, temp8, temp9);
tcg_gen_add_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}

// void _ZN12CPUArchState8qc_wrapiEthh
void emit_qc_wrapi(DisasContext *ctx, TCGv_env env, int16_t vi_11, int8_t vi_13, int8_t vi_4) {
TCGv_i32 temp6 = tcg_temp_new_i32();
tcg_gen_mov_i32(temp6, cpu_gpr[((uint64_t) (uint32_t) vi_13)]);
TCGv_i32 temp8 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp8, temp6, tcg_constant_i32(0), tcg_constant_i32(vi_11), tcg_constant_i32(0));
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_LT, temp0, temp6, tcg_constant_i32(vi_11), temp8, tcg_constant_i32((0 - vi_11)));
tcg_gen_add_i32(temp0, temp0, temp6);
tcg_gen_mov_i32(cpu_gpr[((uint64_t) (uint32_t) vi_4)], temp0);
}


#include "exec/helper-proto.h"
int helper_to_tcg_dispatcher(void *func, TCGTemp *ret_temp, int nargs, TCGTemp **args) {
    return 0;
}
