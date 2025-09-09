#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu_vendorid.h"
#include "smrnmi-csr.h"

static RISCVException rmw_mncause(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->mncause;
    }
    env->mncause = (env->mncause & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_mncause(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_smrnmi)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_mnepc(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->mnepc;
    }
    env->mnepc = (env->mnepc & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_mnepc(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_smrnmi)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_mnscratch(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->mnscratch;
    }
    env->mnscratch = (env->mnscratch & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_mnscratch(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_smrnmi)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_mnstatus(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->mnstatus;
    }
    env->mnstatus = (env->mnstatus & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_mnstatus(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_smrnmi)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
void smrnmi_register_custom_csrs(RISCVCPU *cpu)
{
    riscv_set_csr_ops(CSR_MNCAUSE, &(riscv_csr_operations){"mncause", pred_mncause, NULL, NULL, rmw_mncause});
    riscv_set_csr_ops(CSR_MNEPC, &(riscv_csr_operations){"mnepc", pred_mnepc, NULL, NULL, rmw_mnepc});
    riscv_set_csr_ops(CSR_MNSCRATCH, &(riscv_csr_operations){"mnscratch", pred_mnscratch, NULL, NULL, rmw_mnscratch});
    riscv_set_csr_ops(CSR_MNSTATUS, &(riscv_csr_operations){"mnstatus", pred_mnstatus, NULL, NULL, rmw_mnstatus});
}
