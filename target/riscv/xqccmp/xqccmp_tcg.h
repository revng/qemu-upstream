static void emit_qc_cm_mva01s(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_13);
static void emit_qc_cm_mvsa01(DisasContext *ctx, TCGv_env env, int8_t vi_17, int8_t vi_13);
static void emit_qc_cm_pop(DisasContext *ctx, TCGv_env env, int8_t vi_68, int8_t vi_62);
static void emit_qc_cm_popret(DisasContext *ctx, TCGv_env env, int8_t vi_69, int8_t vi_63);
static void emit_qc_cm_popretz(DisasContext *ctx, TCGv_env env, int8_t vi_70, int8_t vi_64);
static void emit_qc_cm_push(DisasContext *ctx, TCGv_env env, int8_t vi_65, int8_t vi_9);
static void emit_qc_cm_pushfp(DisasContext *ctx, TCGv_env env, int8_t vi_66, int8_t vi_10);
int helper_to_tcg_dispatcher(void *func, TCGTemp *ret_temp, int nargs, TCGTemp **args);
