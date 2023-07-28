/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file defines the QEMU helper functions that could be inlined by
 *   the LLVM translators.
 */

#ifndef __LLVM_HELPER_H
#define __LLVM_HELPER_H

/* Speical TCG runtime helper */
    "tcg_helper_div_i32",
    "tcg_helper_rem_i32",
    "tcg_helper_divu_i32",
    "tcg_helper_remu_i32",
    "tcg_helper_shl_i64",
    "tcg_helper_shr_i64",
    "tcg_helper_sar_i64",
    "tcg_helper_div_i64",
    "tcg_helper_rem_i64",
    "tcg_helper_divu_i64",
    "tcg_helper_remu_i64",

#if defined(TARGET_I386)
    /* General */
    "helper_cc_compute_c",
    "helper_cc_compute_all",
    "helper_load_seg",
    "helper_write_eflags",
    "helper_read_eflags",
    "helper_cli",
    "helper_sti",
    "helper_set_inhibit_irq",
    "helper_reset_inhibit_irq",
    /* FPU */
    "helper_divb_AL", 
    "helper_idivb_AL", 
    "helper_divw_AX", 
    "helper_idivw_AX", 
    "helper_divl_EAX", 
    "helper_idivl_EAX", 
    "helper_flds_FT0", 
    "helper_fldl_FT0", 
    "helper_fildl_FT0", 
    "helper_flds_ST0", 
    "helper_fldl_ST0", 
    "helper_fildl_ST0", 
    "helper_fildll_ST0",
    "helper_fsts_ST0",
    "helper_fstl_ST0", 
    "helper_fist_ST0", 
    "helper_fistl_ST0", 
    "helper_fistll_ST0", 
    "helper_fistt_ST0", 
    "helper_fisttl_ST0", 
    "helper_fisttll_ST0",
    "helper_fldt_ST0", 
    "helper_fstt_ST0", 
    "helper_fpush", 
    "helper_fpop", 
    "helper_fdecstp", 
    "helper_fincstp", 
    "helper_ffree_STN", 
    "helper_fmov_ST0_FT0", 
    "helper_fmov_FT0_STN", 
    "helper_fmov_ST0_STN", 
    "helper_fmov_STN_ST0", 
    "helper_fxchg_ST0_STN",
    "helper_fcom_ST0_FT0", 
    "helper_fucom_ST0_FT0", 
    "helper_fcomi_ST0_FT0", 
    "helper_fucomi_ST0_FT0",
    "helper_fadd_ST0_FT0", 
    "helper_fmul_ST0_FT0", 
    "helper_fsub_ST0_FT0",
    "helper_fsubr_ST0_FT0", 
    "helper_fdiv_ST0_FT0", 
    "helper_fdivr_ST0_FT0", 
    "helper_fadd_STN_ST0", 
    "helper_fmul_STN_ST0", 
    "helper_fsub_STN_ST0", 
    "helper_fsubr_STN_ST0", 
    "helper_fdiv_STN_ST0", 
    "helper_fdivr_STN_ST0", 
    "helper_fchs_ST0",
    "helper_fabs_ST0",
#if defined(TCG_TARGET_I386) && TCG_TARGET_REG_BITS == 64
    "helper_fxam_ST0",
#endif
    "helper_fld1_ST0",
    "helper_fldl2t_ST0",
    "helper_fldl2e_ST0",
    "helper_fldpi_ST0", 
    "helper_fldlg2_ST0",
    "helper_fldln2_ST0",
    "helper_fldz_ST0", 
    "helper_fldz_FT0", 
    "helper_fnstsw", 
    "helper_fnstcw", 
    "helper_fldcw",
    "helper_fclex",
    "helper_fwait", 
    "helper_fninit", 
    "helper_fbld_ST0",
    "helper_fbst_ST0",
    "helper_f2xm1", 
    "helper_fyl2x", 
    "helper_fptan", 
    "helper_fpatan", 
    "helper_fxtract",
    "helper_fprem1", 
    "helper_fprem", 
    "helper_fyl2xp1",
    "helper_fsqrt", 
    "helper_fsincos",
    "helper_frndint",
    "helper_fscale", 
    "helper_fsin", 
    "helper_fcos", 
    "helper_fstenv", 
    "helper_fldenv", 
    "helper_fsave", 
    "helper_frstor", 
    "helper_fxsave", 
    "helper_fxrstor",
    "helper_bsf",
    "helper_bsr",
    "helper_lzcnt", 

    /* MMX/SSE */
    "helper_psrlw_xmm", 
    "helper_psraw_xmm",
    "helper_psllw_xmm",
    "helper_psrld_xmm",
    "helper_psrad_xmm",
    "helper_pslld_xmm",
    "helper_psrlq_xmm",
    "helper_psllq_xmm",
    "helper_psrldq_xmm", 
    "helper_pslldq_xmm",
    "helper_paddb_xmm", 
    "helper_paddw_xmm",
    "helper_paddl_xmm",
    "helper_paddq_xmm",
    "helper_psubb_xmm",
    "helper_psubw_xmm",
    "helper_psubl_xmm",
    "helper_psubq_xmm",
    "helper_paddusb_xmm",
    "helper_paddsb_xmm",
    "helper_psubusb_xmm",
    "helper_psubsb_xmm", 
    "helper_paddusw_xmm",
    "helper_paddsw_xmm",
    "helper_psubusw_xmm",
    "helper_psubsw_xmm", 
    "helper_pminub_xmm",
    "helper_pmaxub_xmm",
    "helper_pminsw_xmm",
    "helper_pmaxsw_xmm",
    "helper_pand_xmm",
    "helper_pandn_xmm",
    "helper_por_xmm",
    "helper_pxor_xmm",
    "helper_pcmpgtb_xmm", 
    "helper_pcmpgtw_xmm",
    "helper_pcmpgtl_xmm",
    "helper_pcmpeqb_xmm",
    "helper_pcmpeqw_xmm",
    "helper_pcmpeql_xmm",
    "helper_pmullw_xmm",
    "helper_pmulhuw_xmm",
    "helper_pmulhw_xmm",
    "helper_pavgb_xmm",
    "helper_pavgw_xmm",
    "helper_pmuludq_xmm",
    "helper_pmaddwd_xmm",
    "helper_psadbw_xmm",
    "helper_maskmov_xmm",
    "helper_movl_mm_T0_xmm",
    "helper_shufps_xmm",
    "helper_shufpd_xmm",
#if !defined(TCG_TARGET_ARM)
    "helper_pshufd_xmm",
    "helper_pshuflw_xmm",
    "helper_pshufhw_xmm",
    "helper_punpcklbw_xmm", 
    "helper_punpcklwd_xmm",
    "helper_punpckldq_xmm",
    "helper_punpckhbw_xmm",
    "helper_punpckhwd_xmm",
    "helper_punpckhdq_xmm",
#endif
    "helper_punpcklqdq_xmm",
    "helper_punpckhqdq_xmm",

    "helper_enter_mmx",
    "helper_psrlw_mmx", 
    "helper_psraw_mmx",
    "helper_psllw_mmx",
    "helper_psrld_mmx",
    "helper_psrad_mmx",
    "helper_pslld_mmx",
    "helper_psrlq_mmx",
    "helper_psllq_mmx",
    "helper_psrldq_mmx", 
    "helper_pslldq_mmx",
    "helper_paddb_mmx", 
    "helper_paddw_mmx",
    "helper_paddl_mmx",
    "helper_paddq_mmx",
    "helper_psubb_mmx",
    "helper_psubw_mmx",
    "helper_psubl_mmx",
    "helper_psubq_mmx",
    "helper_paddusb_mmx",
    "helper_paddsb_mmx",
    "helper_psubusb_mmx",
    "helper_psubsb_mmx", 
    "helper_paddusw_mmx",
    "helper_paddsw_mmx",
    "helper_psubusw_mmx",
    "helper_psubsw_mmx", 
    "helper_pminub_mmx",
    "helper_pmaxub_mmx",
    "helper_pminsw_mmx",
    "helper_pmaxsw_mmx",
    "helper_pand_mmx",
    "helper_pandn_mmx",
    "helper_por_mmx",
    "helper_pxor_mmx",
    "helper_pcmpgtb_mmx", 
    "helper_pcmpgtw_mmx",
    "helper_pcmpgtl_mmx",
    "helper_pcmpeqb_mmx",
    "helper_pcmpeqw_mmx",
    "helper_pcmpeql_mmx",
    "helper_pmullw_mmx",
    "helper_pmulhuw_mmx",
    "helper_pmulhw_mmx",
    "helper_pavgb_mmx",
    "helper_pavgw_mmx",
    "helper_pmuludq_mmx",
    "helper_pmaddwd_mmx",
    "helper_psadbw_mmx",
    "helper_maskmov_mmx",
    "helper_movl_mm_T0_mmx",
    "helper_shufps_mmx",
    "helper_shufpd_mmx",
#if !defined(TCG_TARGET_ARM)
    "helper_pshufd_mmx",
    "helper_pshuflw_mmx",
    "helper_pshufhw_mmx",
    "helper_punpcklbw_mmx", 
    "helper_punpcklwd_mmx",
    "helper_punpckldq_mmx",
    "helper_punpckhbw_mmx",
    "helper_punpckhwd_mmx",
    "helper_punpckhdq_mmx",
#endif
    "helper_punpcklqdq_mmx",
    "helper_punpckhqdq_mmx",

    "helper_addps",
    "helper_addss",
    "helper_addpd",
    "helper_addsd",
    "helper_subps",
    "helper_subss",
    "helper_subpd",
    "helper_subsd",
    "helper_mulps",
    "helper_mulss",
    "helper_mulpd",
    "helper_mulsd",
    "helper_divps",
    "helper_divss",
    "helper_divpd",
    "helper_divsd",
    "helper_minps",
    "helper_minss",
    "helper_minpd",
    "helper_minsd",
    "helper_maxps",
    "helper_maxss",
    "helper_maxpd",
    "helper_maxsd",
    "helper_sqrtps",
    "helper_sqrtss",
    "helper_sqrtpd",
    "helper_sqrtsd",
    "helper_shufps",
    "helper_shufpd",

    "helper_cmpeqps",
    "helper_cmpeqss",
    "helper_cmpeqpd",
    "helper_cmpeqsd",
    "helper_cmpltps",
    "helper_cmpltss",
    "helper_cmpltpd",
    "helper_cmpltsd",
    "helper_cmpleps",
    "helper_cmpless",
    "helper_cmplepd",
    "helper_cmplesd",
    "helper_cmpunordps",
    "helper_cmpunordss",
    "helper_cmpunordpd",
    "helper_cmpunordsd",
    "helper_cmpneqps",
    "helper_cmpneqss",
    "helper_cmpneqpd",
    "helper_cmpneqsd",
    "helper_cmpnltps",
    "helper_cmpnltss",
    "helper_cmpnltpd",
    "helper_cmpnltsd",
    "helper_cmpnleps",
    "helper_cmpnless",
    "helper_cmpnlepd",
    "helper_cmpnlesd",
    "helper_cmpordps",
    "helper_cmpordss",
    "helper_cmpordpd",
    "helper_cmpordsd",

    "helper_cvtps2pd",
    "helper_cvtpd2ps",
    "helper_cvtss2sd",
    "helper_cvtsd2ss",
    "helper_cvtdq2ps",
    "helper_cvtdq2pd",
    "helper_cvtpi2ps",
    "helper_cvtpi2pd",
    "helper_cvtsi2ss",
    "helper_cvtsi2sd",
    "helper_cvtps2dq",
    "helper_cvtpd2dq",
    "helper_cvtps2pi",
    "helper_cvtpd2pi",
    "helper_cvtss2si",
    "helper_cvtsd2si",
    "helper_cvttps2dq",
    "helper_cvttpd2dq",
    "helper_cvttps2pi",
    "helper_cvttpd2pi",
    "helper_cvttss2si",
    "helper_cvttsd2si",

    "helper_cmpeqps",
    "helper_cmpeqss",
    "helper_cmpeqpd",
    "helper_cmpeqsd",
    "helper_cmpltps",
    "helper_cmpltss",
    "helper_cmpltpd",
    "helper_cmpltsd",
    "helper_cmpleps",
    "helper_cmpless",
    "helper_cmplepd",
    "helper_cmplesd",
    "helper_cmpunordps",
    "helper_cmpunordss",
    "helper_cmpunordpd",
    "helper_cmpunordsd",
    "helper_cmpneqps",
    "helper_cmpneqss",
    "helper_cmpneqpd",
    "helper_cmpneqsd",
    "helper_cmpnltps",
    "helper_cmpnltss",
    "helper_cmpnltpd",
    "helper_cmpnltsd",
    "helper_cmpnleps",
    "helper_cmpnless",
    "helper_cmpnlepd",
    "helper_cmpnlesd",
    "helper_cmpordps",
    "helper_cmpordss",
    "helper_cmpordpd",
    "helper_cmpordsd",

    "helper_ucomisd",
    "helper_comisd",
    "helper_ucomiss",
    "helper_comiss",

    "helper_packuswb_xmm",
    "helper_packsswb_xmm",
    "helper_pmovmskb_xmm",
    "helper_pshufw_mmx",

#elif defined(TARGET_ARM)
    "helper_add_cc",
    "helper_sub_cc",
    "helper_shl_cc",
    "helper_shr_cc",
    "helper_sar_cc",
    "helper_adc_cc",
    "helper_sbc_cc",
    "helper_shl",
    "helper_shr",
    "helper_sar",
    "helper_clz",

    "helper_sadd8",
    "helper_sadd16",
    "helper_ssub8",
    "helper_ssub16",
    "helper_ssubaddx",
    "helper_saddsubx",
    "helper_uadd8",
    "helper_uadd16",
    "helper_usub8",
    "helper_usub16",
    "helper_usubaddx",
    "helper_uaddsubx",

    "helper_qadd8",
    "helper_qadd16",
    "helper_qsub8",
    "helper_qsub16",
    "helper_qsubaddx",
    "helper_qaddsubx",
    "helper_uqadd8",
    "helper_uqadd16",
    "helper_uqsub8",
    "helper_uqsub16",
    "helper_uqsubaddx",
    "helper_uqaddsubx",

    "helper_set_rmode",
    "helper_cpsr_write_nzcv",
    "helper_cpsr_write",
    "helper_cpsr_read",
    "helper_vfp_get_fpscr",
    "helper_vfp_set_fpscr",
    "helper_vfp_adds",
    "helper_vfp_addd",
    "helper_vfp_subs",
    "helper_vfp_subd",
    "helper_vfp_muls",
    "helper_vfp_muld",
    "helper_vfp_divs",
    "helper_vfp_divd",
    "helper_vfp_negs",
    "helper_vfp_negd",
    "helper_vfp_abss",
    "helper_vfp_absd",
    "helper_vfp_sqrts",
    "helper_vfp_sqrtd",
    "helper_vfp_cmps",
    "helper_vfp_cmpd",
    "helper_vfp_cmpes",
    "helper_vfp_cmped",

    "helper_vfp_muladds",
    "helper_vfp_muladdd",

#if defined(TARGET_AARCH64)
    "helper_vfp_cmps_a64",
    "helper_vfp_cmpd_a64",
    "helper_vfp_cmpes_a64",
    "helper_vfp_cmped_a64",
    "helper_vfp_minnums",
    "helper_vfp_maxnums",
    "helper_vfp_minnumd",
    "helper_vfp_maxnumd",
#endif
#if !defined(TCG_TARGET_PPC64)
    "helper_vfp_fcvtds",
    "helper_vfp_fcvtsd",
    "helper_vfp_uitos",
    "helper_vfp_uitod",
    "helper_vfp_sitos",
    "helper_vfp_sitod",
    "helper_vfp_touis",
    "helper_vfp_touid",
    "helper_vfp_touizs",
    "helper_vfp_touizd",
    "helper_vfp_tosis",
    "helper_vfp_tosid",
    "helper_vfp_tosizs",
    "helper_vfp_tosizd",
    "helper_vfp_toshs", 
    "helper_vfp_tosls", 
    "helper_vfp_touhs", 
    "helper_vfp_touls", 
    "helper_vfp_toshd", 
    "helper_vfp_tosld", 
    "helper_vfp_touhd", 
    "helper_vfp_tould", 
    "helper_vfp_shtos", 
    "helper_vfp_sltos", 
    "helper_vfp_uhtos", 
    "helper_vfp_ultos", 
    "helper_vfp_shtod", 
    "helper_vfp_sltod", 
    "helper_vfp_uhtod", 
    "helper_vfp_ultod", 
#endif

    /* neon helper */
    "helper_neon_qadd_u8",
    "helper_neon_qadd_s8",
    "helper_neon_qadd_u16",
    "helper_neon_qadd_s16",
    "helper_neon_qsub_u8",
    "helper_neon_qsub_s8",
    "helper_neon_qsub_u16",
    "helper_neon_qsub_s16",
 
    "helper_neon_hadd_s8",
    "helper_neon_hadd_u8",
    "helper_neon_hadd_s16",
    "helper_neon_hadd_u16",
    "helper_neon_hadd_s32",
    "helper_neon_hadd_u32",
    "helper_neon_rhadd_s8",
    "helper_neon_rhadd_u8",
    "helper_neon_rhadd_s16",
    "helper_neon_rhadd_u16",
    "helper_neon_rhadd_s32",
    "helper_neon_rhadd_u32",
    "helper_neon_hsub_s8",
    "helper_neon_hsub_u8",
    "helper_neon_hsub_s16",
    "helper_neon_hsub_u16",
    "helper_neon_hsub_s32",
    "helper_neon_hsub_u32",

    "helper_neon_cgt_u8",
    "helper_neon_cgt_s8",
    "helper_neon_cgt_u16",
    "helper_neon_cgt_s16",
    "helper_neon_cgt_u32",
    "helper_neon_cgt_s32",
    "helper_neon_cge_u8",
    "helper_neon_cge_s8",
    "helper_neon_cge_u16",
    "helper_neon_cge_s16",
    "helper_neon_cge_u32",
    "helper_neon_cge_s32",

    "helper_neon_min_u8",
    "helper_neon_min_s8",
    "helper_neon_min_u16",
    "helper_neon_min_s16",
    "helper_neon_min_u32",
    "helper_neon_min_s32",
    "helper_neon_max_u8",
    "helper_neon_max_s8",
    "helper_neon_max_u16",
    "helper_neon_max_s16",
    "helper_neon_max_u32",
    "helper_neon_max_s32",
    "helper_neon_pmin_u8",
    "helper_neon_pmin_s8",
    "helper_neon_pmin_u16",
    "helper_neon_pmin_s16",
    "helper_neon_pmax_u8",
    "helper_neon_pmax_s8",
    "helper_neon_pmax_u16",
    "helper_neon_pmax_s16",

    "helper_neon_abd_u8",
    "helper_neon_abd_s8",
    "helper_neon_abd_u16",
    "helper_neon_abd_s16",
    "helper_neon_abd_u32",
    "helper_neon_abd_s32",

    "helper_neon_shl_u8",
    "helper_neon_shl_s8",
    "helper_neon_shl_u16",
    "helper_neon_shl_s16",
    "helper_neon_shl_u32",
    "helper_neon_shl_s32",
    "helper_neon_shl_u64",
    "helper_neon_shl_s64",
    "helper_neon_rshl_u8",
    "helper_neon_rshl_s8",
    "helper_neon_rshl_u16",
    "helper_neon_rshl_s16",
    "helper_neon_rshl_u32",
    "helper_neon_rshl_s32",
    "helper_neon_rshl_u64",
    "helper_neon_rshl_s64",
    "helper_neon_qshl_u8",
    "helper_neon_qshl_s8",
    "helper_neon_qshl_u16",
    "helper_neon_qshl_s16",
    "helper_neon_qshl_u32",
    "helper_neon_qshl_s32",
    "helper_neon_qshl_u64",
    "helper_neon_qshl_s64",
    "helper_neon_qrshl_u8",
    "helper_neon_qrshl_s8",
    "helper_neon_qrshl_u16",
    "helper_neon_qrshl_s16",
    "helper_neon_qrshl_u32",
    "helper_neon_qrshl_s32",
    "helper_neon_qrshl_u64",
    "helper_neon_qrshl_s64",

    "helper_neon_add_u8",
    "helper_neon_add_u16",
    "helper_neon_padd_u8",
    "helper_neon_padd_u16",
    "helper_neon_sub_u8",
    "helper_neon_sub_u16",
    "helper_neon_mul_u8",
    "helper_neon_mul_u16",
    "helper_neon_mul_p8",

    "helper_neon_tst_u8",
    "helper_neon_tst_u16",
    "helper_neon_tst_u32",
    "helper_neon_ceq_u8",
    "helper_neon_ceq_u16",
    "helper_neon_ceq_u32",

    "helper_neon_abs_s8",
    "helper_neon_abs_s16",
    "helper_neon_clz_u8",
    "helper_neon_clz_u16",
    "helper_neon_cls_s8",
    "helper_neon_cls_s16",
    "helper_neon_cls_s32",
    "helper_neon_cnt_u8",

    "helper_neon_qdmulh_s16",
    "helper_neon_qrdmulh_s16",
    "helper_neon_qdmulh_s32",
    "helper_neon_qrdmulh_s32",

    "helper_neon_narrow_u8",
    "helper_neon_narrow_u16",
    "helper_neon_narrow_sat_u8",
    "helper_neon_narrow_sat_s8",
    "helper_neon_narrow_sat_u16",
    "helper_neon_narrow_sat_s16",
    "helper_neon_narrow_sat_u32",
    "helper_neon_narrow_sat_s32",
    "helper_neon_narrow_high_u8",
    "helper_neon_narrow_high_u16",
    "helper_neon_narrow_round_high_u8",
    "helper_neon_narrow_round_high_u16",
    "helper_neon_widen_u8",
    "helper_neon_widen_s8",
    "helper_neon_widen_u16",
    "helper_neon_widen_s16",

    "helper_neon_addl_u16",
    "helper_neon_addl_u32",
    "helper_neon_paddl_u16",
    "helper_neon_paddl_u32",
    "helper_neon_subl_u16",
    "helper_neon_subl_u32",
    "helper_neon_addl_saturate_s32",
    "helper_neon_addl_saturate_s64",
    "helper_neon_abdl_u16",
    "helper_neon_abdl_s16",
    "helper_neon_abdl_u32",
    "helper_neon_abdl_s32",
    "helper_neon_abdl_u64",
    "helper_neon_abdl_s64",
    "helper_neon_mull_u8",
    "helper_neon_mull_s8",
    "helper_neon_mull_u16",
    "helper_neon_mull_s16",

    "helper_neon_negl_u16",
    "helper_neon_negl_u32",
    "helper_neon_negl_u64",

    "helper_neon_qabs_s8",
    "helper_neon_qabs_s16",
    "helper_neon_qabs_s32",
    "helper_neon_qneg_s8",
    "helper_neon_qneg_s16",
    "helper_neon_qneg_s32",

    "helper_neon_min_f32",
    "helper_neon_max_f32",
    "helper_neon_abd_f32",
    "helper_neon_add_f32",
    "helper_neon_sub_f32",
    "helper_neon_mul_f32",
    "helper_neon_ceq_f32",
    "helper_neon_cge_f32",
    "helper_neon_cgt_f32",
    "helper_neon_acge_f32",
    "helper_neon_acgt_f32",

#elif defined(TARGET_PPC)
    "helper_popcntb",
    "helper_cntlzw",
    "helper_cntlsw32",
    "helper_cntlzw32",

    "helper_compute_fprf",
    "helper_store_fpscr",
    "helper_fpscr_clrbit",
    "helper_fpscr_setbit",
    "helper_fcmpo",
    "helper_fcmpu",

    "helper_fctiw",
    "helper_fctiwz",
    "helper_frsp",
    "helper_frin",
    "helper_friz",
    "helper_frip",
    "helper_frim",

    "helper_fadd",
    "helper_fsub",
    "helper_fmul",
    "helper_fdiv",
    "helper_fmadd",
    "helper_fmsub",
    "helper_fnmadd",
    "helper_fnmsub",
    "helper_fabs",
    "helper_fnabs",
    "helper_fneg",
    "helper_fsqrt",
    "helper_fre",
    "helper_fres",
    "helper_frsqrte",
    "helper_fsel",

#elif defined(TARGET_MICROBLAZE)
    "helper_addkc",
    "helper_subkc",
    "helper_cmp",
    "helper_cmpu",
    "helper_divs",
    "helper_divu",
#elif defined(TARGET_MIPS)
    "helper_lwl",
    "helper_lwr",
    "helper_swl",
    "helper_swr",
#endif

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

