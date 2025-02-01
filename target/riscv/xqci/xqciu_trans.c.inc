static bool trans_qc_beqi(DisasContext *ctx, arg_qc_beqi *arg)
{
    emit_qc_beqi(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bgei(DisasContext *ctx, arg_qc_bgei *arg)
{
    emit_qc_bgei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bgeui(DisasContext *ctx, arg_qc_bgeui *arg)
{
    emit_qc_bgeui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_blti(DisasContext *ctx, arg_qc_blti *arg)
{
    emit_qc_blti(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bltui(DisasContext *ctx, arg_qc_bltui *arg)
{
    emit_qc_bltui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_bnei(DisasContext *ctx, arg_qc_bnei *arg)
{
    emit_qc_bnei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_c_bexti(DisasContext *ctx, arg_qc_c_bexti *arg)
{
    emit_qc_c_bexti(ctx, tcg_env, arg->shamt, arg->rd);
    return true;
}
static bool trans_qc_c_bseti(DisasContext *ctx, arg_qc_c_bseti *arg)
{
    emit_qc_c_bseti(ctx, tcg_env, arg->shamt, arg->rd);
    return true;
}
static bool trans_qc_c_clrint(DisasContext *ctx, arg_qc_c_clrint *arg)
{
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
    emit_qc_c_eir(ctx, tcg_env, arg->rs1);
    return true;
}
static bool trans_qc_c_mienter_nest(DisasContext *ctx, arg_qc_c_mienter_nest *arg)
{
    emit_qc_c_mienter_nest(ctx, tcg_env);
    return true;
}
static bool trans_qc_c_mienter(DisasContext *ctx, arg_qc_c_mienter *arg)
{
    emit_qc_c_mienter(ctx, tcg_env);
    return true;
}
static bool trans_qc_c_setint(DisasContext *ctx, arg_qc_c_setint *arg)
{
    emit_qc_c_setint(ctx, tcg_env, arg->rs1);
    return true;
}
static bool trans_qc_clo(DisasContext *ctx, arg_qc_clo *arg)
{
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
    emit_qc_compress2(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_compress3(DisasContext *ctx, arg_qc_compress3 *arg)
{
    emit_qc_compress3(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_cto(DisasContext *ctx, arg_qc_cto *arg)
{
    emit_qc_cto(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_addai(DisasContext *ctx, arg_qc_e_addai *arg)
{
    emit_qc_e_addai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_addi(DisasContext *ctx, arg_qc_e_addi *arg)
{
    emit_qc_e_addi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_andai(DisasContext *ctx, arg_qc_e_andai *arg)
{
    emit_qc_e_andai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_andi(DisasContext *ctx, arg_qc_e_andi *arg)
{
    emit_qc_e_andi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_e_beqi(DisasContext *ctx, arg_qc_e_beqi *arg)
{
    emit_qc_e_beqi(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bgei(DisasContext *ctx, arg_qc_e_bgei *arg)
{
    emit_qc_e_bgei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bgeui(DisasContext *ctx, arg_qc_e_bgeui *arg)
{
    emit_qc_e_bgeui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_blti(DisasContext *ctx, arg_qc_e_blti *arg)
{
    emit_qc_e_blti(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bltui(DisasContext *ctx, arg_qc_e_bltui *arg)
{
    emit_qc_e_bltui(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_bnei(DisasContext *ctx, arg_qc_e_bnei *arg)
{
    emit_qc_e_bnei(ctx, tcg_env, arg->offset, arg->imm, arg->rs1);
    return true;
}
static bool trans_qc_e_j(DisasContext *ctx, arg_qc_e_j *arg)
{
    emit_qc_e_j(ctx, tcg_env, arg->imm);
    return true;
}
static bool trans_qc_e_jal(DisasContext *ctx, arg_qc_e_jal *arg)
{
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
    emit_qc_e_orai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_ori(DisasContext *ctx, arg_qc_e_ori *arg)
{
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
    emit_qc_e_xorai(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_e_xori(DisasContext *ctx, arg_qc_e_xori *arg)
{
    emit_qc_e_xori(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_expand2(DisasContext *ctx, arg_qc_expand2 *arg)
{
    emit_qc_expand2(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_expand3(DisasContext *ctx, arg_qc_expand3 *arg)
{
    emit_qc_expand3(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_ext(DisasContext *ctx, arg_qc_ext *arg)
{
    emit_qc_ext(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_extu(DisasContext *ctx, arg_qc_extu *arg)
{
    emit_qc_extu(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insb(DisasContext *ctx, arg_qc_insb *arg)
{
    emit_qc_insb(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbh(DisasContext *ctx, arg_qc_insbh *arg)
{
    emit_qc_insbh(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbhr(DisasContext *ctx, arg_qc_insbhr *arg)
{
    emit_qc_insbhr(ctx, tcg_env, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_insbi(DisasContext *ctx, arg_qc_insbi *arg)
{
    emit_qc_insbi(ctx, tcg_env, arg->width_minus1, arg->shamt, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_insbpr(DisasContext *ctx, arg_qc_insbpr *arg)
{
    emit_qc_insbpr(ctx, tcg_env, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_li(DisasContext *ctx, arg_qc_li *arg)
{
    emit_qc_li(ctx, tcg_env, arg->imm, arg->rd);
    return true;
}
static bool trans_qc_lieq(DisasContext *ctx, arg_qc_lieq *arg)
{
    emit_qc_lieq(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lieqi(DisasContext *ctx, arg_qc_lieqi *arg)
{
    emit_qc_lieqi(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lige(DisasContext *ctx, arg_qc_lige *arg)
{
    emit_qc_lige(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligei(DisasContext *ctx, arg_qc_ligei *arg)
{
    emit_qc_ligei(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligeu(DisasContext *ctx, arg_qc_ligeu *arg)
{
    emit_qc_ligeu(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_ligeui(DisasContext *ctx, arg_qc_ligeui *arg)
{
    emit_qc_ligeui(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lilt(DisasContext *ctx, arg_qc_lilt *arg)
{
    emit_qc_lilt(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lilti(DisasContext *ctx, arg_qc_lilti *arg)
{
    emit_qc_lilti(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_liltu(DisasContext *ctx, arg_qc_liltu *arg)
{
    emit_qc_liltu(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_liltui(DisasContext *ctx, arg_qc_liltui *arg)
{
    emit_qc_liltui(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_line(DisasContext *ctx, arg_qc_line *arg)
{
    emit_qc_line(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_linei(DisasContext *ctx, arg_qc_linei *arg)
{
    emit_qc_linei(ctx, tcg_env, arg->rs1, arg->imm, arg->simm, arg->rd);
    return true;
}
static bool trans_qc_lrb(DisasContext *ctx, arg_qc_lrb *arg)
{
    emit_qc_lrb(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrbu(DisasContext *ctx, arg_qc_lrbu *arg)
{
    emit_qc_lrbu(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrh(DisasContext *ctx, arg_qc_lrh *arg)
{
    emit_qc_lrh(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrhu(DisasContext *ctx, arg_qc_lrhu *arg)
{
    emit_qc_lrhu(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_lrw(DisasContext *ctx, arg_qc_lrw *arg)
{
    emit_qc_lrw(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_muladdi(DisasContext *ctx, arg_qc_muladdi *arg)
{
    emit_qc_muladdi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_mveq(DisasContext *ctx, arg_qc_mveq *arg)
{
    emit_qc_mveq(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mveqi(DisasContext *ctx, arg_qc_mveqi *arg)
{
    emit_qc_mveqi(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvge(DisasContext *ctx, arg_qc_mvge *arg)
{
    emit_qc_mvge(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgei(DisasContext *ctx, arg_qc_mvgei *arg)
{
    emit_qc_mvgei(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgeu(DisasContext *ctx, arg_qc_mvgeu *arg)
{
    emit_qc_mvgeu(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvgeui(DisasContext *ctx, arg_qc_mvgeui *arg)
{
    emit_qc_mvgeui(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvlt(DisasContext *ctx, arg_qc_mvlt *arg)
{
    emit_qc_mvlt(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvlti(DisasContext *ctx, arg_qc_mvlti *arg)
{
    emit_qc_mvlti(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvltu(DisasContext *ctx, arg_qc_mvltu *arg)
{
    emit_qc_mvltu(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvltui(DisasContext *ctx, arg_qc_mvltui *arg)
{
    emit_qc_mvltui(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvne(DisasContext *ctx, arg_qc_mvne *arg)
{
    emit_qc_mvne(ctx, tcg_env, arg->rs1, arg->rs2, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_mvnei(DisasContext *ctx, arg_qc_mvnei *arg)
{
    emit_qc_mvnei(ctx, tcg_env, arg->rs1, arg->imm, arg->rs3, arg->rd);
    return true;
}
static bool trans_qc_norm(DisasContext *ctx, arg_qc_norm *arg)
{
    emit_qc_norm(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_normeu(DisasContext *ctx, arg_qc_normeu *arg)
{
    emit_qc_normeu(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_normu(DisasContext *ctx, arg_qc_normu *arg)
{
    emit_qc_normu(ctx, tcg_env, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_selectieq(DisasContext *ctx, arg_qc_selectieq *arg)
{
    emit_qc_selectieq(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectieqi(DisasContext *ctx, arg_qc_selectieqi *arg)
{
    emit_qc_selectieqi(ctx, tcg_env, arg->imm, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectiieq(DisasContext *ctx, arg_qc_selectiieq *arg)
{
    emit_qc_selectiieq(ctx, tcg_env, arg->rs1, arg->simm1, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectiine(DisasContext *ctx, arg_qc_selectiine *arg)
{
    emit_qc_selectiine(ctx, tcg_env, arg->rs1, arg->simm1, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectine(DisasContext *ctx, arg_qc_selectine *arg)
{
    emit_qc_selectine(ctx, tcg_env, arg->rs1, arg->rs2, arg->simm2, arg->rd);
    return true;
}
static bool trans_qc_selectinei(DisasContext *ctx, arg_qc_selectinei *arg)
{
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
    emit_qc_shladd(ctx, tcg_env, arg->shamt, arg->rs2, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_srb(DisasContext *ctx, arg_qc_srb *arg)
{
    emit_qc_srb(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_srh(DisasContext *ctx, arg_qc_srh *arg)
{
    emit_qc_srh(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_srw(DisasContext *ctx, arg_qc_srw *arg)
{
    emit_qc_srw(ctx, tcg_env, arg->shamt, arg->rs1, arg->rs2, arg->rs3);
    return true;
}
static bool trans_qc_wrap(DisasContext *ctx, arg_qc_wrap *arg)
{
    emit_qc_wrap(ctx, tcg_env, arg->rs1, arg->rs2, arg->rd);
    return true;
}
static bool trans_qc_wrapi(DisasContext *ctx, arg_qc_wrapi *arg)
{
    emit_qc_wrapi(ctx, tcg_env, arg->imm, arg->rs1, arg->rd);
    return true;
}
static bool trans_qc_c_extu(DisasContext *ctx, arg_qc_c_extu *arg)
{
    emit_qc_c_extu(ctx, tcg_env, arg->width_minus1, arg->rd);
    return true;
}
static bool trans_qc_c_muladdi(DisasContext *ctx, arg_qc_c_muladdi *arg)
{
    emit_qc_c_muladdi(ctx, tcg_env, arg->uimm, arg->rs1, arg->rd);
    return true;
}
