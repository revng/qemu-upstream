static bool trans_qc_beqi(DisasContext *ctx, arg_qc_beqi *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_beqi(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bgei(DisasContext *ctx, arg_qc_bgei *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_bgei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bgeui(DisasContext *ctx, arg_qc_bgeui *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_bgeui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_blti(DisasContext *ctx, arg_qc_blti *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_blti(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bltui(DisasContext *ctx, arg_qc_bltui *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_bltui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bnei(DisasContext *ctx, arg_qc_bnei *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_bnei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_c_bexti(DisasContext *ctx, arg_qc_c_bexti *arg)
{
    if (arg->shamt == 0) {
        return false;
    }
    emit_qc_c_bexti(ctx, tcg_env, arg->shamt, arg->rd);
    return true;
}
static bool trans_qc_c_bseti(DisasContext *ctx, arg_qc_c_bseti *arg)
{
    if (arg->shamt == 0) {
        return false;
    }
    emit_qc_c_bseti(ctx, tcg_env, arg->shamt, arg->rd);
    return true;
}
static bool trans_qc_c_clrint(DisasContext *ctx, arg_qc_c_clrint *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_c_clrint(ctx, tcg_env, arg->rs1);
    return true;
}
static bool trans_qc_c_di(DisasContext *ctx, arg_qc_c_di *arg)
{
    emit_qc_c_di(ctx, tcg_env);
    return true;
}
static bool trans_qc_c_dir(DisasContext *ctx, arg_qc_c_dir *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_c_dir(ctx, tcg_env, arg->rd);
    return true;
}
static bool trans_qc_c_ei(DisasContext *ctx, arg_qc_c_ei *arg)
{
    emit_qc_c_ei(ctx, tcg_env);
    return true;
}
static bool trans_qc_c_eir(DisasContext *ctx, arg_qc_c_eir *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_c_eir(ctx, tcg_env, arg->rs1);
    return true;
}
static bool trans_qc_c_extu(DisasContext *ctx, arg_qc_c_extu *arg)
{
    if (arg->width_minus1 == 0 || arg->width_minus1 == 1 || arg->width_minus1 == 2 || arg->width_minus1 == 3 || arg->width_minus1 == 4) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_c_extu(ctx, tcg_env, arg->width_minus1, arg->rd);
    return true;
}
static bool trans_qc_c_setint(DisasContext *ctx, arg_qc_c_setint *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_c_setint(ctx, tcg_env, arg->rs1);
    return true;
}
static bool trans_qc_clo(DisasContext *ctx, arg_qc_clo *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_clo(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_clrinti(DisasContext *ctx, arg_qc_clrinti *arg)
{
    emit_qc_clrinti(ctx, tcg_env, arg->imm);
    return true;
}
static bool trans_qc_compress2(DisasContext *ctx, arg_qc_compress2 *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_compress2(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_compress3(DisasContext *ctx, arg_qc_compress3 *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_compress3(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_cto(DisasContext *ctx, arg_qc_cto *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_cto(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_addai(DisasContext *ctx, arg_qc_e_addai *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_addai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_addi(DisasContext *ctx, arg_qc_e_addi *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_addi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_andai(DisasContext *ctx, arg_qc_e_andai *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_andai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_andi(DisasContext *ctx, arg_qc_e_andi *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_andi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_beqi(DisasContext *ctx, arg_qc_e_beqi *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_beqi(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bgei(DisasContext *ctx, arg_qc_e_bgei *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_bgei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bgeui(DisasContext *ctx, arg_qc_e_bgeui *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_bgeui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_blti(DisasContext *ctx, arg_qc_e_blti *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_blti(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bltui(DisasContext *ctx, arg_qc_e_bltui *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_bltui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bnei(DisasContext *ctx, arg_qc_e_bnei *arg)
{
    arg->offset <<= 1;
    if (arg->imm == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    emit_qc_e_bnei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_j(DisasContext *ctx, arg_qc_e_j *arg)
{
    arg->imm <<= 1;
    emit_qc_e_j(ctx, tcg_env, arg->imm);
    return true;
}
static bool trans_qc_e_jal(DisasContext *ctx, arg_qc_e_jal *arg)
{
    arg->imm <<= 1;
    emit_qc_e_jal(ctx, tcg_env, arg->imm);
    return true;
}
static bool trans_qc_e_lb(DisasContext *ctx, arg_qc_e_lb *arg)
{
    emit_qc_e_lb(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_lbu(DisasContext *ctx, arg_qc_e_lbu *arg)
{
    emit_qc_e_lbu(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_lh(DisasContext *ctx, arg_qc_e_lh *arg)
{
    emit_qc_e_lh(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_lhu(DisasContext *ctx, arg_qc_e_lhu *arg)
{
    emit_qc_e_lhu(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_li(DisasContext *ctx, arg_qc_e_li *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_li(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_lw(DisasContext *ctx, arg_qc_e_lw *arg)
{
    emit_qc_e_lw(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_orai(DisasContext *ctx, arg_qc_e_orai *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_orai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_ori(DisasContext *ctx, arg_qc_e_ori *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_ori(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_sb(DisasContext *ctx, arg_qc_e_sb *arg)
{
    emit_qc_e_sb(ctx, tcg_env, arg->imm, arg->rs2, arg->rs1);
    return true;
}
static bool trans_qc_e_sh(DisasContext *ctx, arg_qc_e_sh *arg)
{
    emit_qc_e_sh(ctx, tcg_env, arg->imm, arg->rs2, arg->rs1);
    return true;
}
static bool trans_qc_e_sw(DisasContext *ctx, arg_qc_e_sw *arg)
{
    emit_qc_e_sw(ctx, tcg_env, arg->imm, arg->rs2, arg->rs1);
    return true;
}
static bool trans_qc_e_xorai(DisasContext *ctx, arg_qc_e_xorai *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_xorai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_xori(DisasContext *ctx, arg_qc_e_xori *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_e_xori(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_expand2(DisasContext *ctx, arg_qc_expand2 *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_expand2(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_expand3(DisasContext *ctx, arg_qc_expand3 *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_expand3(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_ext(DisasContext *ctx, arg_qc_ext *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_ext(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_extu(DisasContext *ctx, arg_qc_extu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_extu(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insb(DisasContext *ctx, arg_qc_insb *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_insb(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbh(DisasContext *ctx, arg_qc_insbh *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_insbh(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbhr(DisasContext *ctx, arg_qc_insbhr *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_insbhr(ctx, tcg_env, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbi(DisasContext *ctx, arg_qc_insbi *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_insbi(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_insbpr(DisasContext *ctx, arg_qc_insbpr *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_insbpr(ctx, tcg_env, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_li(DisasContext *ctx, arg_qc_li *arg)
{
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_li(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_lieq(DisasContext *ctx, arg_qc_lieq *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_lieq(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lieqi(DisasContext *ctx, arg_qc_lieqi *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_lieqi(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lige(DisasContext *ctx, arg_qc_lige *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_lige(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligei(DisasContext *ctx, arg_qc_ligei *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_ligei(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligeu(DisasContext *ctx, arg_qc_ligeu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_ligeu(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligeui(DisasContext *ctx, arg_qc_ligeui *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_ligeui(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lilt(DisasContext *ctx, arg_qc_lilt *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_lilt(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lilti(DisasContext *ctx, arg_qc_lilti *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_lilti(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_liltu(DisasContext *ctx, arg_qc_liltu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_liltu(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_liltui(DisasContext *ctx, arg_qc_liltui *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_liltui(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_line(DisasContext *ctx, arg_qc_line *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_line(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_linei(DisasContext *ctx, arg_qc_linei *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_linei(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lrb(DisasContext *ctx, arg_qc_lrb *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_lrb(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrbu(DisasContext *ctx, arg_qc_lrbu *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_lrbu(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrh(DisasContext *ctx, arg_qc_lrh *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_lrh(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrhu(DisasContext *ctx, arg_qc_lrhu *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_lrhu(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrw(DisasContext *ctx, arg_qc_lrw *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_lrw(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_mveq(DisasContext *ctx, arg_qc_mveq *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mveq(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mveqi(DisasContext *ctx, arg_qc_mveqi *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mveqi(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvge(DisasContext *ctx, arg_qc_mvge *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvge(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgei(DisasContext *ctx, arg_qc_mvgei *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvgei(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgeu(DisasContext *ctx, arg_qc_mvgeu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvgeu(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgeui(DisasContext *ctx, arg_qc_mvgeui *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvgeui(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvlt(DisasContext *ctx, arg_qc_mvlt *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvlt(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvlti(DisasContext *ctx, arg_qc_mvlti *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvlti(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvltu(DisasContext *ctx, arg_qc_mvltu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvltu(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvltui(DisasContext *ctx, arg_qc_mvltui *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvltui(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvne(DisasContext *ctx, arg_qc_mvne *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvne(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvnei(DisasContext *ctx, arg_qc_mvnei *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs3 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_mvnei(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_norm(DisasContext *ctx, arg_qc_norm *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_norm(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_normeu(DisasContext *ctx, arg_qc_normeu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_normeu(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_normu(DisasContext *ctx, arg_qc_normu *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_normu(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_selectieq(DisasContext *ctx, arg_qc_selectieq *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectieq(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectieqi(DisasContext *ctx, arg_qc_selectieqi *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectieqi(ctx, tcg_env, arg->imm, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectiieq(DisasContext *ctx, arg_qc_selectiieq *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectiieq(ctx, tcg_env, arg->rs1, arg->simm1, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectiine(DisasContext *ctx, arg_qc_selectiine *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectiine(ctx, tcg_env, arg->rs1, arg->simm1, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectine(DisasContext *ctx, arg_qc_selectine *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectine(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectinei(DisasContext *ctx, arg_qc_selectinei *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_selectinei(ctx, tcg_env, arg->imm, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_setinti(DisasContext *ctx, arg_qc_setinti *arg)
{
    emit_qc_setinti(ctx, tcg_env, arg->imm);
    return true;
}
static bool trans_qc_shladd(DisasContext *ctx, arg_qc_shladd *arg)
{
    if (arg->shamt == 0 || arg->shamt == 1 || arg->shamt == 2 || arg->shamt == 3) {
        return false;
    }
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_shladd(ctx, tcg_env, arg->shamt, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_srb(DisasContext *ctx, arg_qc_srb *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_srb(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_srh(DisasContext *ctx, arg_qc_srh *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_srh(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_srw(DisasContext *ctx, arg_qc_srw *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    emit_qc_srw(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_wrap(DisasContext *ctx, arg_qc_wrap *arg)
{
    if (arg->rs2 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_wrap(ctx, tcg_env, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_wrapi(DisasContext *ctx, arg_qc_wrapi *arg)
{
    if (arg->rs1 == 0) {
        return false;
    }
    if (arg->rd == 0) {
        return false;
    }
    emit_qc_wrapi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
