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

#ifndef CONFIG_USER_ONLY
static void mienter_copy_cpustate(CPUArchState *env, uintptr_t ra)
{
    target_ulong vaddr = env->gpr[2];
    target_ulong mepc;
    target_ulong mnepc;
    target_ulong mcause;
    target_ulong reserved_val = 0;
    int mmu_idx = riscv_env_get_mmu_index(env, false);
    target_ulong mask = (1ull << 32) - 1;
    RISCVException ret;

    ret = riscv_csrr(env, CSR_MEPC, &mepc);
    if (ret != RISCV_EXCP_NONE) {
        riscv_raise_exception(env, ret, ra);
    }

    ret = riscv_csrr(env, CSR_QC_MNEPC, &mnepc);
    if (ret != RISCV_EXCP_NONE) {
        riscv_raise_exception(env, ret, ra);
    }

    ret = riscv_csrr(env, CSR_MCAUSE, &mcause);
    if (ret != RISCV_EXCP_NONE) {
        riscv_raise_exception(env, ret, ra);
    }

    if (get_field(mcause, MCAUSE_NMI) != 1) {
        cpu_stl_mmuidx_ra(env, vaddr - 4, mepc, mmu_idx, ra);
    } else {
        cpu_stl_mmuidx_ra(env, vaddr - 4, mnepc, mmu_idx, ra);
    }

    cpu_stl_mmuidx_ra(env, vaddr - 8,  env->gpr[8] & mask,  mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 12, mcause,              mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 16, env->gpr[1] & mask,  mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 20, reserved_val,        mmu_idx, ra);

    cpu_stl_mmuidx_ra(env, vaddr - 24, env->gpr[5] & mask,  mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 28, env->gpr[6] & mask,  mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 32, env->gpr[7] & mask,  mmu_idx, ra);

    cpu_stl_mmuidx_ra(env, vaddr - 36, env->gpr[10] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 40, env->gpr[11] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 44, env->gpr[12] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 48, env->gpr[13] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 52, env->gpr[14] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 56, env->gpr[15] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 60, env->gpr[16] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 64, env->gpr[17] & mask, mmu_idx, ra);

    cpu_stl_mmuidx_ra(env, vaddr - 68, env->gpr[28] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 72, env->gpr[29] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 76, env->gpr[30] & mask, mmu_idx, ra);
    cpu_stl_mmuidx_ra(env, vaddr - 80, env->gpr[31] & mask, mmu_idx, ra);

    env->gpr[2] -= 96;
}

void HELPER(xqci_mienter)(CPUArchState *env)
{
    mienter_copy_cpustate(env, GETPC());
}

void HELPER(xqci_mienter_nest)(CPUArchState *env)
{
    mienter_copy_cpustate(env, GETPC());
    env->mstatus = set_field(env->mstatus, MSTATUS_MIE, 1);
}

void HELPER(xqci_mileaveret)(CPUArchState *env)
{

    target_ulong vaddr = env->gpr[2] + 96;
    target_ulong prev_retpc;
    target_ulong curr_retpc;
    int mcause_nmi = get_field(env->mcause, MCAUSE_NMI);
    int mmu_idx = riscv_env_get_mmu_index(env, false);
    target_ulong mask = (1ull << 32) - 1;
    RISCVException ret;

    prev_retpc = cpu_ldl_mmuidx_ra(env, vaddr - 4, mmu_idx, GETPC());

    if (mcause_nmi != 1) {
        ret = riscv_csrr(env, CSR_MNEPC, &curr_retpc);
        if (ret != RISCV_EXCP_NONE) {
            riscv_raise_exception(env, ret, GETPC());
        }
        ret = riscv_csrrw(env, CSR_MNEPC, NULL, prev_retpc, -1);
        if (ret != RISCV_EXCP_NONE) {
            riscv_raise_exception(env, ret, GETPC());
        }
    } else {
        ret = riscv_csrr(env, CSR_QC_MNEPC, &curr_retpc);
        if (ret != RISCV_EXCP_NONE) {
            riscv_raise_exception(env, ret, GETPC());
        }
        ret = riscv_csrrw(env, CSR_QC_MNEPC, NULL, prev_retpc, -1);
        if (ret != RISCV_EXCP_NONE) {
            riscv_raise_exception(env, ret, GETPC());
        }
    }

    env->gpr[8] = cpu_ldl_mmuidx_ra(env, vaddr - 8, mmu_idx, GETPC());
    env->mcause = cpu_ldl_mmuidx_ra(env, vaddr - 12, mmu_idx, GETPC());
    env->gpr[1] = cpu_ldl_mmuidx_ra(env, vaddr - 16, mmu_idx, GETPC());

    env->gpr[5] = cpu_ldl_mmuidx_ra(env, vaddr - 24, mmu_idx, GETPC());
    env->gpr[6] = cpu_ldl_mmuidx_ra(env, vaddr - 28, mmu_idx, GETPC());
    env->gpr[7] = cpu_ldl_mmuidx_ra(env, vaddr - 32, mmu_idx, GETPC());

    env->gpr[10] = cpu_ldl_mmuidx_ra(env, vaddr - 36, mmu_idx, GETPC());
    env->gpr[11] = cpu_ldl_mmuidx_ra(env, vaddr - 40, mmu_idx, GETPC());
    env->gpr[12] = cpu_ldl_mmuidx_ra(env, vaddr - 44, mmu_idx, GETPC());
    env->gpr[13] = cpu_ldl_mmuidx_ra(env, vaddr - 48, mmu_idx, GETPC());
    env->gpr[14] = cpu_ldl_mmuidx_ra(env, vaddr - 52, mmu_idx, GETPC());
    env->gpr[15] = cpu_ldl_mmuidx_ra(env, vaddr - 56, mmu_idx, GETPC());
    env->gpr[16] = cpu_ldl_mmuidx_ra(env, vaddr - 60, mmu_idx, GETPC());
    env->gpr[17] = cpu_ldl_mmuidx_ra(env, vaddr - 64, mmu_idx, GETPC());

    env->gpr[28] = cpu_ldl_mmuidx_ra(env, vaddr - 68, mmu_idx, GETPC());
    env->gpr[29] = cpu_ldl_mmuidx_ra(env, vaddr - 72, mmu_idx, GETPC());
    env->gpr[30] = cpu_ldl_mmuidx_ra(env, vaddr - 76, mmu_idx, GETPC());
    env->gpr[31] = cpu_ldl_mmuidx_ra(env, vaddr - 80, mmu_idx, GETPC());

    env->gpr[2] += 96;
    env->pc = curr_retpc;
}

static void emit_qc_c_mienter(DisasContext *ctx, TCGv_env tcg_env)
{
    gen_helper_xqci_mienter(tcg_env);
}

static void emit_qc_c_mienter_nest(DisasContext *ctx, TCGv_env tcg_env)
{
    gen_helper_xqci_mienter_nest(tcg_env);
}

static void emit_qc_c_mileaveret(DisasContext *ctx, TCGv_env tcg_env)
{
    gen_helper_xqci_mileaveret(tcg_env);
}
#endif

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

static void emit_qc_c_mveqz(DisasContext *ctx, TCGv_env tcg_env, int rs1, int rd)
{
    TCGv dst = dest_gpr(ctx, rd + 8);
    TCGv src = get_gpr(ctx, rs1 + 8, EXT_NONE);
    TCGv zero = tcg_constant_i32(0);
    tcg_gen_movcond_i32(TCG_COND_EQ, dst, dst, zero, src, dst);
}
