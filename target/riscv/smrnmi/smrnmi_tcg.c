#include "qemu/osdep.h"
#include "qemu/log.h"
#include "cpu.h"
#include "tcg/tcg-op.h"
#include "tcg/tcg-op-gvec.h"
#include "tcg/tcg.h"
#include "exec/exec-all.h"
#include "exec/helper-gen.h"

// void _ZN12CPUArchState5mnretEv
static void emit_mnret(DisasContext *ctx, TCGv_env env) {
TCGv_i32 temp20 = xqci_implemented_S(ctx);
TCGLabel * label21 = gen_new_label();
TCGLabel * label22 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp20, 0, label21);
tcg_gen_br(label22);
gen_set_label(label21);
TCGv_i32 temp19 = xqci_csrr(ctx, env, 768);
TCGLabel * label24 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp19, 3, label22);
gen_set_label(label24);
xqci_csrw_field(ctx, env, 768, 131072, tcg_constant_i32(0));
tcg_gen_br(label22);
gen_set_label(label22);
TCGv_i32 temp16 = xqci_csrr(ctx, env, 1860);
TCGLabel * label25 = gen_new_label();
TCGLabel * label26 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp16, 0, label25);
gen_set_label(label26);
TCGv_i32 temp14 = xqci_csrr(ctx, env, 1860);
TCGLabel * label27 = gen_new_label();
TCGLabel * label28 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_EQ, temp14, 1, label27);
gen_set_label(label28);
TCGv_i32 temp12 = xqci_csrr(ctx, env, 1860);
TCGLabel * label29 = gen_new_label();
TCGLabel * label30 = gen_new_label();
tcg_gen_brcondi_i32(TCG_COND_NE, temp12, 3, label29);
gen_set_label(label30);
xqci_set_mode_M(ctx);
tcg_gen_br(label29);
gen_set_label(label27);
xqci_set_mode_S(ctx);
tcg_gen_br(label29);
gen_set_label(label25);
xqci_set_mode_U(ctx);
tcg_gen_br(label29);
gen_set_label(label29);
TCGv_i32 temp7 = xqci_implemented_U(ctx);
TCGv_i32 temp0 = tcg_temp_new_i32();
tcg_gen_movcond_i32(TCG_COND_NE, temp0, temp7, tcg_constant_i32(0), tcg_constant_i32(0), tcg_constant_i32(3));
xqci_csrw_field(ctx, env, 1860, 6144, temp0);
temp0 = xqci_csrr(ctx, env, 1857);
tcg_gen_mov_i32(cpu_pc, temp0);
}

