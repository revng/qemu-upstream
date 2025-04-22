static bool trans_mnret(DisasContext *ctx, arg_mnret *arg)
{
#ifndef TARGET_RISCV32
    return false;
#else
    emit_mnret(ctx, tcg_env);
    return true;
#endif
}
