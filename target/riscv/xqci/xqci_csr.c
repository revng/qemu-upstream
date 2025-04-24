#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu_vendorid.h"
#include "xqci_csr.h"

static RISCVException any(CPURISCVState *env, int csrno)
{
    return RISCV_EXCP_NONE;
}

static RISCVException read_qc_mcause(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mcause;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mcause(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mcause = val;
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
static RISCVException read_qc_mclicilvl00(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl00;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl00(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl00 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl01(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl01;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl01(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl01 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl02(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl02;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl02(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl02 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl03(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl03;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl03(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl03 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl04(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl04;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl04(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl04 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl05(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl05;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl05(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl05 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl06(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl06;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl06(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl06 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl07(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl07;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl07(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl07 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl08(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl08;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl08(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl08 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl09(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl09;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl09(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl09 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl10(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl10;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl10(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl10 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl11(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl11;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl11(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl11 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl12(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl12;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl12(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl12 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl13(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl13;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl13(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl13 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl14(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl14;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl14(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl14 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl15(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl15;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl15(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl15 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl16(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl16;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl16(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl16 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl17(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl17;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl17(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl17 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl18(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl18;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl18(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl18 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl19(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl19;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl19(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl19 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl20(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl20;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl20(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl20 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl21(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl21;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl21(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl21 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl22(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl22;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl22(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl22 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl23(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl23;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl23(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl23 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl24(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl24;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl24(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl24 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl25(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl25;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl25(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl25 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl26(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl26;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl26(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl26 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl27(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl27;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl27(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl27 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl28(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl28;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl28(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl28 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl29(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl29;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl29(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl29 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl30(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl30;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl30(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl30 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mclicilvl31(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mclicilvl31;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mclicilvl31(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mclicilvl31 = val;
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
static RISCVException read_qc_mmcr(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mmcr;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mmcr(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mmcr = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mntvec(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mntvec;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mntvec(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mntvec = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mstkbottomaddr(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mstkbottomaddr;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mstkbottomaddr(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mstkbottomaddr = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mstktopaddr(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mstktopaddr;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mstktopaddr(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mstktopaddr = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mthreadptr(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mthreadptr;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mthreadptr(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mthreadptr = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpendaddr0(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpendaddr0;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpendaddr0(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpendaddr0 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpendaddr1(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpendaddr1;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpendaddr1(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpendaddr1 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpendaddr2(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpendaddr2;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpendaddr2(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpendaddr2 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpendaddr3(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpendaddr3;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpendaddr3(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpendaddr3 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpstartaddr0(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpstartaddr0;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpstartaddr0(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpstartaddr0 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpstartaddr1(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpstartaddr1;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpstartaddr1(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpstartaddr1 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpstartaddr2(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpstartaddr2;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpstartaddr2(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpstartaddr2 = val;
    return RISCV_EXCP_NONE;
}
static RISCVException read_qc_mwpstartaddr3(CPURISCVState *env, int csrno, target_ulong *val)
{
    *val = env->qc_mwpstartaddr3;
    return RISCV_EXCP_NONE;
}
static RISCVException write_qc_mwpstartaddr3(CPURISCVState *env, int csrno, target_ulong val)
{
    env->qc_mwpstartaddr3 = val;
    return RISCV_EXCP_NONE;
}
void xqci_register_custom_csrs(RISCVCPU *cpu)
{
    riscv_set_csr_ops(CSR_QC_MCAUSE, &(riscv_csr_operations){"qc_mcause", any, read_qc_mcause, write_qc_mcause});
    riscv_set_csr_ops(CSR_QC_MCLICIE0, &(riscv_csr_operations){"qc_mclicie0", any, read_qc_mclicie0, write_qc_mclicie0});
    riscv_set_csr_ops(CSR_QC_MCLICIE1, &(riscv_csr_operations){"qc_mclicie1", any, read_qc_mclicie1, write_qc_mclicie1});
    riscv_set_csr_ops(CSR_QC_MCLICIE2, &(riscv_csr_operations){"qc_mclicie2", any, read_qc_mclicie2, write_qc_mclicie2});
    riscv_set_csr_ops(CSR_QC_MCLICIE3, &(riscv_csr_operations){"qc_mclicie3", any, read_qc_mclicie3, write_qc_mclicie3});
    riscv_set_csr_ops(CSR_QC_MCLICIE4, &(riscv_csr_operations){"qc_mclicie4", any, read_qc_mclicie4, write_qc_mclicie4});
    riscv_set_csr_ops(CSR_QC_MCLICIE5, &(riscv_csr_operations){"qc_mclicie5", any, read_qc_mclicie5, write_qc_mclicie5});
    riscv_set_csr_ops(CSR_QC_MCLICIE6, &(riscv_csr_operations){"qc_mclicie6", any, read_qc_mclicie6, write_qc_mclicie6});
    riscv_set_csr_ops(CSR_QC_MCLICIE7, &(riscv_csr_operations){"qc_mclicie7", any, read_qc_mclicie7, write_qc_mclicie7});
    riscv_set_csr_ops(CSR_QC_MCLICILVL00, &(riscv_csr_operations){"qc_mclicilvl00", any, read_qc_mclicilvl00, write_qc_mclicilvl00});
    riscv_set_csr_ops(CSR_QC_MCLICILVL01, &(riscv_csr_operations){"qc_mclicilvl01", any, read_qc_mclicilvl01, write_qc_mclicilvl01});
    riscv_set_csr_ops(CSR_QC_MCLICILVL02, &(riscv_csr_operations){"qc_mclicilvl02", any, read_qc_mclicilvl02, write_qc_mclicilvl02});
    riscv_set_csr_ops(CSR_QC_MCLICILVL03, &(riscv_csr_operations){"qc_mclicilvl03", any, read_qc_mclicilvl03, write_qc_mclicilvl03});
    riscv_set_csr_ops(CSR_QC_MCLICILVL04, &(riscv_csr_operations){"qc_mclicilvl04", any, read_qc_mclicilvl04, write_qc_mclicilvl04});
    riscv_set_csr_ops(CSR_QC_MCLICILVL05, &(riscv_csr_operations){"qc_mclicilvl05", any, read_qc_mclicilvl05, write_qc_mclicilvl05});
    riscv_set_csr_ops(CSR_QC_MCLICILVL06, &(riscv_csr_operations){"qc_mclicilvl06", any, read_qc_mclicilvl06, write_qc_mclicilvl06});
    riscv_set_csr_ops(CSR_QC_MCLICILVL07, &(riscv_csr_operations){"qc_mclicilvl07", any, read_qc_mclicilvl07, write_qc_mclicilvl07});
    riscv_set_csr_ops(CSR_QC_MCLICILVL08, &(riscv_csr_operations){"qc_mclicilvl08", any, read_qc_mclicilvl08, write_qc_mclicilvl08});
    riscv_set_csr_ops(CSR_QC_MCLICILVL09, &(riscv_csr_operations){"qc_mclicilvl09", any, read_qc_mclicilvl09, write_qc_mclicilvl09});
    riscv_set_csr_ops(CSR_QC_MCLICILVL10, &(riscv_csr_operations){"qc_mclicilvl10", any, read_qc_mclicilvl10, write_qc_mclicilvl10});
    riscv_set_csr_ops(CSR_QC_MCLICILVL11, &(riscv_csr_operations){"qc_mclicilvl11", any, read_qc_mclicilvl11, write_qc_mclicilvl11});
    riscv_set_csr_ops(CSR_QC_MCLICILVL12, &(riscv_csr_operations){"qc_mclicilvl12", any, read_qc_mclicilvl12, write_qc_mclicilvl12});
    riscv_set_csr_ops(CSR_QC_MCLICILVL13, &(riscv_csr_operations){"qc_mclicilvl13", any, read_qc_mclicilvl13, write_qc_mclicilvl13});
    riscv_set_csr_ops(CSR_QC_MCLICILVL14, &(riscv_csr_operations){"qc_mclicilvl14", any, read_qc_mclicilvl14, write_qc_mclicilvl14});
    riscv_set_csr_ops(CSR_QC_MCLICILVL15, &(riscv_csr_operations){"qc_mclicilvl15", any, read_qc_mclicilvl15, write_qc_mclicilvl15});
    riscv_set_csr_ops(CSR_QC_MCLICILVL16, &(riscv_csr_operations){"qc_mclicilvl16", any, read_qc_mclicilvl16, write_qc_mclicilvl16});
    riscv_set_csr_ops(CSR_QC_MCLICILVL17, &(riscv_csr_operations){"qc_mclicilvl17", any, read_qc_mclicilvl17, write_qc_mclicilvl17});
    riscv_set_csr_ops(CSR_QC_MCLICILVL18, &(riscv_csr_operations){"qc_mclicilvl18", any, read_qc_mclicilvl18, write_qc_mclicilvl18});
    riscv_set_csr_ops(CSR_QC_MCLICILVL19, &(riscv_csr_operations){"qc_mclicilvl19", any, read_qc_mclicilvl19, write_qc_mclicilvl19});
    riscv_set_csr_ops(CSR_QC_MCLICILVL20, &(riscv_csr_operations){"qc_mclicilvl20", any, read_qc_mclicilvl20, write_qc_mclicilvl20});
    riscv_set_csr_ops(CSR_QC_MCLICILVL21, &(riscv_csr_operations){"qc_mclicilvl21", any, read_qc_mclicilvl21, write_qc_mclicilvl21});
    riscv_set_csr_ops(CSR_QC_MCLICILVL22, &(riscv_csr_operations){"qc_mclicilvl22", any, read_qc_mclicilvl22, write_qc_mclicilvl22});
    riscv_set_csr_ops(CSR_QC_MCLICILVL23, &(riscv_csr_operations){"qc_mclicilvl23", any, read_qc_mclicilvl23, write_qc_mclicilvl23});
    riscv_set_csr_ops(CSR_QC_MCLICILVL24, &(riscv_csr_operations){"qc_mclicilvl24", any, read_qc_mclicilvl24, write_qc_mclicilvl24});
    riscv_set_csr_ops(CSR_QC_MCLICILVL25, &(riscv_csr_operations){"qc_mclicilvl25", any, read_qc_mclicilvl25, write_qc_mclicilvl25});
    riscv_set_csr_ops(CSR_QC_MCLICILVL26, &(riscv_csr_operations){"qc_mclicilvl26", any, read_qc_mclicilvl26, write_qc_mclicilvl26});
    riscv_set_csr_ops(CSR_QC_MCLICILVL27, &(riscv_csr_operations){"qc_mclicilvl27", any, read_qc_mclicilvl27, write_qc_mclicilvl27});
    riscv_set_csr_ops(CSR_QC_MCLICILVL28, &(riscv_csr_operations){"qc_mclicilvl28", any, read_qc_mclicilvl28, write_qc_mclicilvl28});
    riscv_set_csr_ops(CSR_QC_MCLICILVL29, &(riscv_csr_operations){"qc_mclicilvl29", any, read_qc_mclicilvl29, write_qc_mclicilvl29});
    riscv_set_csr_ops(CSR_QC_MCLICILVL30, &(riscv_csr_operations){"qc_mclicilvl30", any, read_qc_mclicilvl30, write_qc_mclicilvl30});
    riscv_set_csr_ops(CSR_QC_MCLICILVL31, &(riscv_csr_operations){"qc_mclicilvl31", any, read_qc_mclicilvl31, write_qc_mclicilvl31});
    riscv_set_csr_ops(CSR_QC_MCLICIP0, &(riscv_csr_operations){"qc_mclicip0", any, read_qc_mclicip0, write_qc_mclicip0});
    riscv_set_csr_ops(CSR_QC_MCLICIP1, &(riscv_csr_operations){"qc_mclicip1", any, read_qc_mclicip1, write_qc_mclicip1});
    riscv_set_csr_ops(CSR_QC_MCLICIP2, &(riscv_csr_operations){"qc_mclicip2", any, read_qc_mclicip2, write_qc_mclicip2});
    riscv_set_csr_ops(CSR_QC_MCLICIP3, &(riscv_csr_operations){"qc_mclicip3", any, read_qc_mclicip3, write_qc_mclicip3});
    riscv_set_csr_ops(CSR_QC_MCLICIP4, &(riscv_csr_operations){"qc_mclicip4", any, read_qc_mclicip4, write_qc_mclicip4});
    riscv_set_csr_ops(CSR_QC_MCLICIP5, &(riscv_csr_operations){"qc_mclicip5", any, read_qc_mclicip5, write_qc_mclicip5});
    riscv_set_csr_ops(CSR_QC_MCLICIP6, &(riscv_csr_operations){"qc_mclicip6", any, read_qc_mclicip6, write_qc_mclicip6});
    riscv_set_csr_ops(CSR_QC_MCLICIP7, &(riscv_csr_operations){"qc_mclicip7", any, read_qc_mclicip7, write_qc_mclicip7});
    riscv_set_csr_ops(CSR_QC_MMCR, &(riscv_csr_operations){"qc_mmcr", any, read_qc_mmcr, write_qc_mmcr});
    riscv_set_csr_ops(CSR_QC_MNTVEC, &(riscv_csr_operations){"qc_mntvec", any, read_qc_mntvec, write_qc_mntvec});
    riscv_set_csr_ops(CSR_QC_MSTKBOTTOMADDR, &(riscv_csr_operations){"qc_mstkbottomaddr", any, read_qc_mstkbottomaddr, write_qc_mstkbottomaddr});
    riscv_set_csr_ops(CSR_QC_MSTKTOPADDR, &(riscv_csr_operations){"qc_mstktopaddr", any, read_qc_mstktopaddr, write_qc_mstktopaddr});
    riscv_set_csr_ops(CSR_QC_MTHREADPTR, &(riscv_csr_operations){"qc_mthreadptr", any, read_qc_mthreadptr, write_qc_mthreadptr});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR0, &(riscv_csr_operations){"qc_mwpendaddr0", any, read_qc_mwpendaddr0, write_qc_mwpendaddr0});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR1, &(riscv_csr_operations){"qc_mwpendaddr1", any, read_qc_mwpendaddr1, write_qc_mwpendaddr1});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR2, &(riscv_csr_operations){"qc_mwpendaddr2", any, read_qc_mwpendaddr2, write_qc_mwpendaddr2});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR3, &(riscv_csr_operations){"qc_mwpendaddr3", any, read_qc_mwpendaddr3, write_qc_mwpendaddr3});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR0, &(riscv_csr_operations){"qc_mwpstartaddr0", any, read_qc_mwpstartaddr0, write_qc_mwpstartaddr0});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR1, &(riscv_csr_operations){"qc_mwpstartaddr1", any, read_qc_mwpstartaddr1, write_qc_mwpstartaddr1});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR2, &(riscv_csr_operations){"qc_mwpstartaddr2", any, read_qc_mwpstartaddr2, write_qc_mwpstartaddr2});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR3, &(riscv_csr_operations){"qc_mwpstartaddr3", any, read_qc_mwpstartaddr3, write_qc_mwpstartaddr3});
}
