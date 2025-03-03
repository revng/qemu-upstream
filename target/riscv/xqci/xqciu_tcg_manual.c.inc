void HELPER(xqci_swm)(CPUArchState *env, target_ulong vaddr, target_ulong length, int rs3)
{
    int mmu_idx = riscv_env_mmu_index(env, false);

    if (rs3 + length > 32) {
        riscv_raise_exception(env, RISCV_EXCP_ILLEGAL_INST, GETPC());
    }

    for (int i = 0; i < length; ++i) {
        cpu_stl_mmuidx_ra(env, vaddr, env->gpr[rs3 + i], mmu_idx, GETPC());
        vaddr += 4;
    }
}

void HELPER(xqci_lwm)(CPUArchState *env, target_ulong vaddr, target_ulong length, int rd)
{
    int mmu_idx = riscv_env_mmu_index(env, false);

    if (rd + length > 32) {
        riscv_raise_exception(env, RISCV_EXCP_ILLEGAL_INST, GETPC());
    }

    for (int i = 0; i < length; ++i) {
        env->gpr[rd + i] = cpu_ldl_mmuidx_ra(env, vaddr, mmu_idx, GETPC());
        vaddr += 4;
    }
}

void HELPER(xqci_setwm)(CPUArchState *env, target_ulong vaddr, target_ulong length, target_ulong value)
{
    int mmu_idx = riscv_env_mmu_index(env, false);

    for (int i = 0; i < length; ++i) {
        cpu_stl_mmuidx_ra(env, vaddr, value, mmu_idx, GETPC());
        vaddr += 4;
    }
}

static TCGv xqci_ea(DisasContext *ctx, int rs1, int imm)
{
    TCGv vaddr = tcg_temp_new();
    tcg_gen_addi_tl(vaddr, get_gpr(ctx, rs1, EXT_NONE), imm);
    return vaddr;
}

static void emit_qc_brev32(DisasContext *ctx, TCGv_env tcg_env, int rs1, int rd)
{
    TCGv dst = dest_gpr(ctx, rd);
    TCGv src = get_gpr(ctx, rs1, EXT_NONE);
    gen_helper_brev32(dst, src);
}

static void emit_qc_swm(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int rs2, int rs3)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    TCGv length = get_gpr(ctx, rs2, EXT_NONE);
    gen_helper_xqci_swm(tcg_env, vaddr, length, tcg_constant_i32(rs3));
}

static void emit_qc_swmi(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int length, int rs3)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    gen_helper_xqci_swm(tcg_env, vaddr, tcg_constant_tl(length), tcg_constant_i32(rs3));
}

static void emit_qc_lwm(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int rs2, int rd)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    TCGv length = get_gpr(ctx, rs2, EXT_NONE);
    gen_helper_xqci_lwm(tcg_env, vaddr, length, tcg_constant_i32(rd));
}

static void emit_qc_lwmi(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int length, int rd)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    gen_helper_xqci_lwm(tcg_env, vaddr, tcg_constant_tl(length), tcg_constant_i32(rd));
}

static void emit_qc_setwm(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int rs2, int rs3)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    TCGv length = get_gpr(ctx, rs2, EXT_NONE);
    TCGv value = get_gpr(ctx, rs3, EXT_NONE);
    gen_helper_xqci_setwm(tcg_env, vaddr, length, value);
}

static void emit_qc_setwmi(DisasContext *ctx, TCGv_env tcg_env, int imm, int rs1, int length, int rs3)
{
    TCGv vaddr = xqci_ea(ctx, rs1, imm);
    TCGv value = get_gpr(ctx, rs3, EXT_NONE);
    gen_helper_xqci_setwm(tcg_env, vaddr, tcg_constant_tl(length), value);
}
