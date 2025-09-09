#include "qemu/osdep.h"
#include "cpu.h"
#include "cpu_vendorid.h"
#include "xqci-csr.h"

static RISCVException rmw_qc_mcause(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mcause;
    }
    env->qc_mcause = (env->qc_mcause & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mcause(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie0(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie0;
    }
    env->qc_mclicie0 = (env->qc_mclicie0 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie0(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie1(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie1;
    }
    env->qc_mclicie1 = (env->qc_mclicie1 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie1(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie2(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie2;
    }
    env->qc_mclicie2 = (env->qc_mclicie2 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie2(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie3(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie3;
    }
    env->qc_mclicie3 = (env->qc_mclicie3 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie3(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie4(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie4;
    }
    env->qc_mclicie4 = (env->qc_mclicie4 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie4(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie5(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie5;
    }
    env->qc_mclicie5 = (env->qc_mclicie5 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie5(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie6(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie6;
    }
    env->qc_mclicie6 = (env->qc_mclicie6 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie6(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicie7(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicie7;
    }
    env->qc_mclicie7 = (env->qc_mclicie7 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicie7(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl00(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl00;
    }
    env->qc_mclicilvl00 = (env->qc_mclicilvl00 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl00(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl01(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl01;
    }
    env->qc_mclicilvl01 = (env->qc_mclicilvl01 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl01(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl02(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl02;
    }
    env->qc_mclicilvl02 = (env->qc_mclicilvl02 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl02(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl03(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl03;
    }
    env->qc_mclicilvl03 = (env->qc_mclicilvl03 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl03(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl04(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl04;
    }
    env->qc_mclicilvl04 = (env->qc_mclicilvl04 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl04(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl05(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl05;
    }
    env->qc_mclicilvl05 = (env->qc_mclicilvl05 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl05(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl06(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl06;
    }
    env->qc_mclicilvl06 = (env->qc_mclicilvl06 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl06(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl07(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl07;
    }
    env->qc_mclicilvl07 = (env->qc_mclicilvl07 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl07(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl08(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl08;
    }
    env->qc_mclicilvl08 = (env->qc_mclicilvl08 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl08(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl09(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl09;
    }
    env->qc_mclicilvl09 = (env->qc_mclicilvl09 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl09(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl10(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl10;
    }
    env->qc_mclicilvl10 = (env->qc_mclicilvl10 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl10(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl11(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl11;
    }
    env->qc_mclicilvl11 = (env->qc_mclicilvl11 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl11(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl12(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl12;
    }
    env->qc_mclicilvl12 = (env->qc_mclicilvl12 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl12(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl13(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl13;
    }
    env->qc_mclicilvl13 = (env->qc_mclicilvl13 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl13(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl14(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl14;
    }
    env->qc_mclicilvl14 = (env->qc_mclicilvl14 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl14(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl15(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl15;
    }
    env->qc_mclicilvl15 = (env->qc_mclicilvl15 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl15(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl16(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl16;
    }
    env->qc_mclicilvl16 = (env->qc_mclicilvl16 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl16(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl17(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl17;
    }
    env->qc_mclicilvl17 = (env->qc_mclicilvl17 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl17(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl18(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl18;
    }
    env->qc_mclicilvl18 = (env->qc_mclicilvl18 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl18(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl19(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl19;
    }
    env->qc_mclicilvl19 = (env->qc_mclicilvl19 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl19(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl20(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl20;
    }
    env->qc_mclicilvl20 = (env->qc_mclicilvl20 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl20(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl21(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl21;
    }
    env->qc_mclicilvl21 = (env->qc_mclicilvl21 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl21(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl22(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl22;
    }
    env->qc_mclicilvl22 = (env->qc_mclicilvl22 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl22(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl23(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl23;
    }
    env->qc_mclicilvl23 = (env->qc_mclicilvl23 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl23(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl24(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl24;
    }
    env->qc_mclicilvl24 = (env->qc_mclicilvl24 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl24(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl25(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl25;
    }
    env->qc_mclicilvl25 = (env->qc_mclicilvl25 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl25(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl26(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl26;
    }
    env->qc_mclicilvl26 = (env->qc_mclicilvl26 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl26(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl27(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl27;
    }
    env->qc_mclicilvl27 = (env->qc_mclicilvl27 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl27(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl28(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl28;
    }
    env->qc_mclicilvl28 = (env->qc_mclicilvl28 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl28(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl29(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl29;
    }
    env->qc_mclicilvl29 = (env->qc_mclicilvl29 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl29(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl30(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl30;
    }
    env->qc_mclicilvl30 = (env->qc_mclicilvl30 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl30(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicilvl31(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicilvl31;
    }
    env->qc_mclicilvl31 = (env->qc_mclicilvl31 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicilvl31(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip0(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip0;
    }
    env->qc_mclicip0 = (env->qc_mclicip0 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip0(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip1(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip1;
    }
    env->qc_mclicip1 = (env->qc_mclicip1 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip1(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip2(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip2;
    }
    env->qc_mclicip2 = (env->qc_mclicip2 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip2(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip3(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip3;
    }
    env->qc_mclicip3 = (env->qc_mclicip3 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip3(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip4(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip4;
    }
    env->qc_mclicip4 = (env->qc_mclicip4 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip4(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip5(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip5;
    }
    env->qc_mclicip5 = (env->qc_mclicip5 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip5(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip6(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip6;
    }
    env->qc_mclicip6 = (env->qc_mclicip6 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip6(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mclicip7(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mclicip7;
    }
    env->qc_mclicip7 = (env->qc_mclicip7 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mclicip7(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mmcr(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mmcr;
    }
    env->qc_mmcr = (env->qc_mmcr & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mmcr(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mntvec(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mntvec;
    }
    env->qc_mntvec = (env->qc_mntvec & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mntvec(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mstkbottomaddr(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mstkbottomaddr;
    }
    env->qc_mstkbottomaddr = (env->qc_mstkbottomaddr & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mstkbottomaddr(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mstktopaddr(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mstktopaddr;
    }
    env->qc_mstktopaddr = (env->qc_mstktopaddr & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mstktopaddr(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mthreadptr(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mthreadptr;
    }
    env->qc_mthreadptr = (env->qc_mthreadptr & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mthreadptr(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpendaddr0(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpendaddr0;
    }
    env->qc_mwpendaddr0 = (env->qc_mwpendaddr0 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpendaddr0(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpendaddr1(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpendaddr1;
    }
    env->qc_mwpendaddr1 = (env->qc_mwpendaddr1 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpendaddr1(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpendaddr2(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpendaddr2;
    }
    env->qc_mwpendaddr2 = (env->qc_mwpendaddr2 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpendaddr2(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpendaddr3(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpendaddr3;
    }
    env->qc_mwpendaddr3 = (env->qc_mwpendaddr3 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpendaddr3(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpstartaddr0(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpstartaddr0;
    }
    env->qc_mwpstartaddr0 = (env->qc_mwpstartaddr0 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpstartaddr0(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpstartaddr1(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpstartaddr1;
    }
    env->qc_mwpstartaddr1 = (env->qc_mwpstartaddr1 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpstartaddr1(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpstartaddr2(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpstartaddr2;
    }
    env->qc_mwpstartaddr2 = (env->qc_mwpstartaddr2 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpstartaddr2(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
static RISCVException rmw_qc_mwpstartaddr3(CPURISCVState * env,
                              int csrno,
                              target_ulong *ret_val,
                              target_ulong new_val,
                              target_ulong wr_mask)
{
    if (ret_val) {
        *ret_val = env->qc_mwpstartaddr3;
    }
    env->qc_mwpstartaddr3 = (env->qc_mwpstartaddr3 & ~wr_mask) | (new_val & wr_mask);
    return RISCV_EXCP_NONE;
}
static RISCVException pred_qc_mwpstartaddr3(CPURISCVState * env,
                               int csrno)
{
    if (env->priv != PRV_M || (!riscv_cpu_cfg(env)->ext_xqci && !riscv_cpu_cfg(env)->ext_xqciint)) {
        return RISCV_EXCP_ILLEGAL_INST;
    }
    return RISCV_EXCP_NONE;
}
void xqci_register_custom_csrs(RISCVCPU *cpu)
{
    riscv_set_csr_ops(CSR_QC_MCAUSE, &(riscv_csr_operations){"qc_mcause", pred_qc_mcause, NULL, NULL, rmw_qc_mcause});
    riscv_set_csr_ops(CSR_QC_MCLICIE0, &(riscv_csr_operations){"qc_mclicie0", pred_qc_mclicie0, NULL, NULL, rmw_qc_mclicie0});
    riscv_set_csr_ops(CSR_QC_MCLICIE1, &(riscv_csr_operations){"qc_mclicie1", pred_qc_mclicie1, NULL, NULL, rmw_qc_mclicie1});
    riscv_set_csr_ops(CSR_QC_MCLICIE2, &(riscv_csr_operations){"qc_mclicie2", pred_qc_mclicie2, NULL, NULL, rmw_qc_mclicie2});
    riscv_set_csr_ops(CSR_QC_MCLICIE3, &(riscv_csr_operations){"qc_mclicie3", pred_qc_mclicie3, NULL, NULL, rmw_qc_mclicie3});
    riscv_set_csr_ops(CSR_QC_MCLICIE4, &(riscv_csr_operations){"qc_mclicie4", pred_qc_mclicie4, NULL, NULL, rmw_qc_mclicie4});
    riscv_set_csr_ops(CSR_QC_MCLICIE5, &(riscv_csr_operations){"qc_mclicie5", pred_qc_mclicie5, NULL, NULL, rmw_qc_mclicie5});
    riscv_set_csr_ops(CSR_QC_MCLICIE6, &(riscv_csr_operations){"qc_mclicie6", pred_qc_mclicie6, NULL, NULL, rmw_qc_mclicie6});
    riscv_set_csr_ops(CSR_QC_MCLICIE7, &(riscv_csr_operations){"qc_mclicie7", pred_qc_mclicie7, NULL, NULL, rmw_qc_mclicie7});
    riscv_set_csr_ops(CSR_QC_MCLICILVL00, &(riscv_csr_operations){"qc_mclicilvl00", pred_qc_mclicilvl00, NULL, NULL, rmw_qc_mclicilvl00});
    riscv_set_csr_ops(CSR_QC_MCLICILVL01, &(riscv_csr_operations){"qc_mclicilvl01", pred_qc_mclicilvl01, NULL, NULL, rmw_qc_mclicilvl01});
    riscv_set_csr_ops(CSR_QC_MCLICILVL02, &(riscv_csr_operations){"qc_mclicilvl02", pred_qc_mclicilvl02, NULL, NULL, rmw_qc_mclicilvl02});
    riscv_set_csr_ops(CSR_QC_MCLICILVL03, &(riscv_csr_operations){"qc_mclicilvl03", pred_qc_mclicilvl03, NULL, NULL, rmw_qc_mclicilvl03});
    riscv_set_csr_ops(CSR_QC_MCLICILVL04, &(riscv_csr_operations){"qc_mclicilvl04", pred_qc_mclicilvl04, NULL, NULL, rmw_qc_mclicilvl04});
    riscv_set_csr_ops(CSR_QC_MCLICILVL05, &(riscv_csr_operations){"qc_mclicilvl05", pred_qc_mclicilvl05, NULL, NULL, rmw_qc_mclicilvl05});
    riscv_set_csr_ops(CSR_QC_MCLICILVL06, &(riscv_csr_operations){"qc_mclicilvl06", pred_qc_mclicilvl06, NULL, NULL, rmw_qc_mclicilvl06});
    riscv_set_csr_ops(CSR_QC_MCLICILVL07, &(riscv_csr_operations){"qc_mclicilvl07", pred_qc_mclicilvl07, NULL, NULL, rmw_qc_mclicilvl07});
    riscv_set_csr_ops(CSR_QC_MCLICILVL08, &(riscv_csr_operations){"qc_mclicilvl08", pred_qc_mclicilvl08, NULL, NULL, rmw_qc_mclicilvl08});
    riscv_set_csr_ops(CSR_QC_MCLICILVL09, &(riscv_csr_operations){"qc_mclicilvl09", pred_qc_mclicilvl09, NULL, NULL, rmw_qc_mclicilvl09});
    riscv_set_csr_ops(CSR_QC_MCLICILVL10, &(riscv_csr_operations){"qc_mclicilvl10", pred_qc_mclicilvl10, NULL, NULL, rmw_qc_mclicilvl10});
    riscv_set_csr_ops(CSR_QC_MCLICILVL11, &(riscv_csr_operations){"qc_mclicilvl11", pred_qc_mclicilvl11, NULL, NULL, rmw_qc_mclicilvl11});
    riscv_set_csr_ops(CSR_QC_MCLICILVL12, &(riscv_csr_operations){"qc_mclicilvl12", pred_qc_mclicilvl12, NULL, NULL, rmw_qc_mclicilvl12});
    riscv_set_csr_ops(CSR_QC_MCLICILVL13, &(riscv_csr_operations){"qc_mclicilvl13", pred_qc_mclicilvl13, NULL, NULL, rmw_qc_mclicilvl13});
    riscv_set_csr_ops(CSR_QC_MCLICILVL14, &(riscv_csr_operations){"qc_mclicilvl14", pred_qc_mclicilvl14, NULL, NULL, rmw_qc_mclicilvl14});
    riscv_set_csr_ops(CSR_QC_MCLICILVL15, &(riscv_csr_operations){"qc_mclicilvl15", pred_qc_mclicilvl15, NULL, NULL, rmw_qc_mclicilvl15});
    riscv_set_csr_ops(CSR_QC_MCLICILVL16, &(riscv_csr_operations){"qc_mclicilvl16", pred_qc_mclicilvl16, NULL, NULL, rmw_qc_mclicilvl16});
    riscv_set_csr_ops(CSR_QC_MCLICILVL17, &(riscv_csr_operations){"qc_mclicilvl17", pred_qc_mclicilvl17, NULL, NULL, rmw_qc_mclicilvl17});
    riscv_set_csr_ops(CSR_QC_MCLICILVL18, &(riscv_csr_operations){"qc_mclicilvl18", pred_qc_mclicilvl18, NULL, NULL, rmw_qc_mclicilvl18});
    riscv_set_csr_ops(CSR_QC_MCLICILVL19, &(riscv_csr_operations){"qc_mclicilvl19", pred_qc_mclicilvl19, NULL, NULL, rmw_qc_mclicilvl19});
    riscv_set_csr_ops(CSR_QC_MCLICILVL20, &(riscv_csr_operations){"qc_mclicilvl20", pred_qc_mclicilvl20, NULL, NULL, rmw_qc_mclicilvl20});
    riscv_set_csr_ops(CSR_QC_MCLICILVL21, &(riscv_csr_operations){"qc_mclicilvl21", pred_qc_mclicilvl21, NULL, NULL, rmw_qc_mclicilvl21});
    riscv_set_csr_ops(CSR_QC_MCLICILVL22, &(riscv_csr_operations){"qc_mclicilvl22", pred_qc_mclicilvl22, NULL, NULL, rmw_qc_mclicilvl22});
    riscv_set_csr_ops(CSR_QC_MCLICILVL23, &(riscv_csr_operations){"qc_mclicilvl23", pred_qc_mclicilvl23, NULL, NULL, rmw_qc_mclicilvl23});
    riscv_set_csr_ops(CSR_QC_MCLICILVL24, &(riscv_csr_operations){"qc_mclicilvl24", pred_qc_mclicilvl24, NULL, NULL, rmw_qc_mclicilvl24});
    riscv_set_csr_ops(CSR_QC_MCLICILVL25, &(riscv_csr_operations){"qc_mclicilvl25", pred_qc_mclicilvl25, NULL, NULL, rmw_qc_mclicilvl25});
    riscv_set_csr_ops(CSR_QC_MCLICILVL26, &(riscv_csr_operations){"qc_mclicilvl26", pred_qc_mclicilvl26, NULL, NULL, rmw_qc_mclicilvl26});
    riscv_set_csr_ops(CSR_QC_MCLICILVL27, &(riscv_csr_operations){"qc_mclicilvl27", pred_qc_mclicilvl27, NULL, NULL, rmw_qc_mclicilvl27});
    riscv_set_csr_ops(CSR_QC_MCLICILVL28, &(riscv_csr_operations){"qc_mclicilvl28", pred_qc_mclicilvl28, NULL, NULL, rmw_qc_mclicilvl28});
    riscv_set_csr_ops(CSR_QC_MCLICILVL29, &(riscv_csr_operations){"qc_mclicilvl29", pred_qc_mclicilvl29, NULL, NULL, rmw_qc_mclicilvl29});
    riscv_set_csr_ops(CSR_QC_MCLICILVL30, &(riscv_csr_operations){"qc_mclicilvl30", pred_qc_mclicilvl30, NULL, NULL, rmw_qc_mclicilvl30});
    riscv_set_csr_ops(CSR_QC_MCLICILVL31, &(riscv_csr_operations){"qc_mclicilvl31", pred_qc_mclicilvl31, NULL, NULL, rmw_qc_mclicilvl31});
    riscv_set_csr_ops(CSR_QC_MCLICIP0, &(riscv_csr_operations){"qc_mclicip0", pred_qc_mclicip0, NULL, NULL, rmw_qc_mclicip0});
    riscv_set_csr_ops(CSR_QC_MCLICIP1, &(riscv_csr_operations){"qc_mclicip1", pred_qc_mclicip1, NULL, NULL, rmw_qc_mclicip1});
    riscv_set_csr_ops(CSR_QC_MCLICIP2, &(riscv_csr_operations){"qc_mclicip2", pred_qc_mclicip2, NULL, NULL, rmw_qc_mclicip2});
    riscv_set_csr_ops(CSR_QC_MCLICIP3, &(riscv_csr_operations){"qc_mclicip3", pred_qc_mclicip3, NULL, NULL, rmw_qc_mclicip3});
    riscv_set_csr_ops(CSR_QC_MCLICIP4, &(riscv_csr_operations){"qc_mclicip4", pred_qc_mclicip4, NULL, NULL, rmw_qc_mclicip4});
    riscv_set_csr_ops(CSR_QC_MCLICIP5, &(riscv_csr_operations){"qc_mclicip5", pred_qc_mclicip5, NULL, NULL, rmw_qc_mclicip5});
    riscv_set_csr_ops(CSR_QC_MCLICIP6, &(riscv_csr_operations){"qc_mclicip6", pred_qc_mclicip6, NULL, NULL, rmw_qc_mclicip6});
    riscv_set_csr_ops(CSR_QC_MCLICIP7, &(riscv_csr_operations){"qc_mclicip7", pred_qc_mclicip7, NULL, NULL, rmw_qc_mclicip7});
    riscv_set_csr_ops(CSR_QC_MMCR, &(riscv_csr_operations){"qc_mmcr", pred_qc_mmcr, NULL, NULL, rmw_qc_mmcr});
    riscv_set_csr_ops(CSR_QC_MNTVEC, &(riscv_csr_operations){"qc_mntvec", pred_qc_mntvec, NULL, NULL, rmw_qc_mntvec});
    riscv_set_csr_ops(CSR_QC_MSTKBOTTOMADDR, &(riscv_csr_operations){"qc_mstkbottomaddr", pred_qc_mstkbottomaddr, NULL, NULL, rmw_qc_mstkbottomaddr});
    riscv_set_csr_ops(CSR_QC_MSTKTOPADDR, &(riscv_csr_operations){"qc_mstktopaddr", pred_qc_mstktopaddr, NULL, NULL, rmw_qc_mstktopaddr});
    riscv_set_csr_ops(CSR_QC_MTHREADPTR, &(riscv_csr_operations){"qc_mthreadptr", pred_qc_mthreadptr, NULL, NULL, rmw_qc_mthreadptr});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR0, &(riscv_csr_operations){"qc_mwpendaddr0", pred_qc_mwpendaddr0, NULL, NULL, rmw_qc_mwpendaddr0});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR1, &(riscv_csr_operations){"qc_mwpendaddr1", pred_qc_mwpendaddr1, NULL, NULL, rmw_qc_mwpendaddr1});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR2, &(riscv_csr_operations){"qc_mwpendaddr2", pred_qc_mwpendaddr2, NULL, NULL, rmw_qc_mwpendaddr2});
    riscv_set_csr_ops(CSR_QC_MWPENDADDR3, &(riscv_csr_operations){"qc_mwpendaddr3", pred_qc_mwpendaddr3, NULL, NULL, rmw_qc_mwpendaddr3});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR0, &(riscv_csr_operations){"qc_mwpstartaddr0", pred_qc_mwpstartaddr0, NULL, NULL, rmw_qc_mwpstartaddr0});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR1, &(riscv_csr_operations){"qc_mwpstartaddr1", pred_qc_mwpstartaddr1, NULL, NULL, rmw_qc_mwpstartaddr1});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR2, &(riscv_csr_operations){"qc_mwpstartaddr2", pred_qc_mwpstartaddr2, NULL, NULL, rmw_qc_mwpstartaddr2});
    riscv_set_csr_ops(CSR_QC_MWPSTARTADDR3, &(riscv_csr_operations){"qc_mwpstartaddr3", pred_qc_mwpstartaddr3, NULL, NULL, rmw_qc_mwpstartaddr3});
}
