#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu_vendorid.h"
#include "xqciu_csr.h"

static RISCVException any(CPURISCVState *env, int csrno)
{
    return RISCV_EXCP_NONE;
}

static RISCVException read_qc_mclicie0(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie0;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie0(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie0 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie1(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie1;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie1(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie1 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie2(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie2;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie2(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie2 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie3(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie3;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie3(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie3 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie4(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie4;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie4(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie4 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie5(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie5;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie5(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie5 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie6(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie6;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie6(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie6 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicie7(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicie7;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicie7(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicie7 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip0(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip0;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip0(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip0 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip1(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip1;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip1(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip1 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip2(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip2;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip2(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip2 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip3(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip3;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip3(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip3 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip4(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip4;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip4(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip4 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip5(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip5;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip5(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip5 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip6(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip6;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip6(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip6 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicip7(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicip7;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicip7(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicip7 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mncause(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mncause;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mncause(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mncause = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mnepc(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mnepc;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mnepc(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mnepc = val;
    return RISCV_EXCP_NONE;
}
void qc_iu_register_custom_csrs(RISCVCPU *cpu)
{
    riscv_set_csr_ops(CSR_QC_MCLICIE0, &(riscv_csr_operations){"qc_mclicie0", any, read_qc_mclicie0, write_qc_mclicie0});
    riscv_set_csr_ops(CSR_QC_MCLICIE1, &(riscv_csr_operations){"qc_mclicie1", any, read_qc_mclicie1, write_qc_mclicie1});
    riscv_set_csr_ops(CSR_QC_MCLICIE2, &(riscv_csr_operations){"qc_mclicie2", any, read_qc_mclicie2, write_qc_mclicie2});
    riscv_set_csr_ops(CSR_QC_MCLICIE3, &(riscv_csr_operations){"qc_mclicie3", any, read_qc_mclicie3, write_qc_mclicie3});
    riscv_set_csr_ops(CSR_QC_MCLICIE4, &(riscv_csr_operations){"qc_mclicie4", any, read_qc_mclicie4, write_qc_mclicie4});
    riscv_set_csr_ops(CSR_QC_MCLICIE5, &(riscv_csr_operations){"qc_mclicie5", any, read_qc_mclicie5, write_qc_mclicie5});
    riscv_set_csr_ops(CSR_QC_MCLICIE6, &(riscv_csr_operations){"qc_mclicie6", any, read_qc_mclicie6, write_qc_mclicie6});
    riscv_set_csr_ops(CSR_QC_MCLICIE7, &(riscv_csr_operations){"qc_mclicie7", any, read_qc_mclicie7, write_qc_mclicie7});
    riscv_set_csr_ops(CSR_QC_MCLICIP0, &(riscv_csr_operations){"qc_mclicip0", any, read_qc_mclicip0, write_qc_mclicip0});
    riscv_set_csr_ops(CSR_QC_MCLICIP1, &(riscv_csr_operations){"qc_mclicip1", any, read_qc_mclicip1, write_qc_mclicip1});
    riscv_set_csr_ops(CSR_QC_MCLICIP2, &(riscv_csr_operations){"qc_mclicip2", any, read_qc_mclicip2, write_qc_mclicip2});
    riscv_set_csr_ops(CSR_QC_MCLICIP3, &(riscv_csr_operations){"qc_mclicip3", any, read_qc_mclicip3, write_qc_mclicip3});
    riscv_set_csr_ops(CSR_QC_MCLICIP4, &(riscv_csr_operations){"qc_mclicip4", any, read_qc_mclicip4, write_qc_mclicip4});
    riscv_set_csr_ops(CSR_QC_MCLICIP5, &(riscv_csr_operations){"qc_mclicip5", any, read_qc_mclicip5, write_qc_mclicip5});
    riscv_set_csr_ops(CSR_QC_MCLICIP6, &(riscv_csr_operations){"qc_mclicip6", any, read_qc_mclicip6, write_qc_mclicip6});
    riscv_set_csr_ops(CSR_QC_MCLICIP7, &(riscv_csr_operations){"qc_mclicip7", any, read_qc_mclicip7, write_qc_mclicip7});
    riscv_set_csr_ops(CSR_QC_MNCAUSE, &(riscv_csr_operations){"qc_mncause", any, read_qc_mncause, write_qc_mncause});
    riscv_set_csr_ops(CSR_QC_MNEPC, &(riscv_csr_operations){"qc_mnepc", any, read_qc_mnepc, write_qc_mnepc});
}
