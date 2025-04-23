#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu_vendorid.h"
#include "smrnmi_csr.h"

static RISCVException any(CPURISCVState *env, int csrno)
{
    return RISCV_EXCP_NONE;
}

static RISCVException read_mncause(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->mncause;
    return RISCV_EXCP_NONE;
}
static RISCVException write_mncause(CPURISCVState *env, int csrno, target_ulong val)
{
    env->mncause = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_mnepc(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->mnepc;
    return RISCV_EXCP_NONE;
}
static RISCVException write_mnepc(CPURISCVState *env, int csrno, target_ulong val)
{
    env->mnepc = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_mnscratch(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->mnscratch;
    return RISCV_EXCP_NONE;
}
static RISCVException write_mnscratch(CPURISCVState *env, int csrno, target_ulong val)
{
    env->mnscratch = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_mnstatus(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->mnstatus;
    return RISCV_EXCP_NONE;
}
static RISCVException write_mnstatus(CPURISCVState *env, int csrno, target_ulong val)
{
    env->mnstatus = val;
    return RISCV_EXCP_NONE;
}
void smrnmi_register_custom_csrs(RISCVCPU *cpu)
{
    riscv_set_csr_ops(CSR_MNCAUSE, &(riscv_csr_operations){"mncause", any, read_mncause, write_mncause});
    riscv_set_csr_ops(CSR_MNEPC, &(riscv_csr_operations){"mnepc", any, read_mnepc, write_mnepc});
    riscv_set_csr_ops(CSR_MNSCRATCH, &(riscv_csr_operations){"mnscratch", any, read_mnscratch, write_mnscratch});
    riscv_set_csr_ops(CSR_MNSTATUS, &(riscv_csr_operations){"mnstatus", any, read_mnstatus, write_mnstatus});
}
