typedef struct {
    int rs2;
    int rs1;
    int rd;
} arg_qc_insbhr;
typedef struct {
    int unused_rs2;
    int rs1;
    int rd;
} arg_qc_compress3;
bool trans_qc_insbhr_qc_compress3(DisasContext *ctx, arg_qc_insbhr_qc_compress3 *arg)
{
    switch(arg->rs2) {
    case 0:
        trans_qc_compress3(ctx, (arg_qc_compress3 *) arg);
        break;
    default:
        trans_qc_insbhr(ctx, (arg_qc_insbhr *) arg);
    }
}

typedef struct {
    int rs2;
    int rs1;
    int rd;
} arg_qc_insbpr;
typedef struct {
    int unused_rs2;
    int rs1;
    int rd;
} arg_qc_expand2;
bool trans_qc_insbpr_qc_expand2(DisasContext *ctx, arg_qc_insbpr_qc_expand2 *arg)
{
    switch(arg->rs2) {
    case 0:
        trans_qc_expand2(ctx, (arg_qc_expand2 *) arg);
        break;
    default:
        trans_qc_insbpr(ctx, (arg_qc_insbpr *) arg);
    }
}

