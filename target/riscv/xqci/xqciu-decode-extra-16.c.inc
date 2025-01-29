typedef struct {
    int rs1;
    int unused_width_minus1;
} arg_qc_c_setint;
typedef struct {
    int rd;
    int width_minus1;
} arg_qc_c_extu;
typedef struct {
    int unused_rs1;
    int unused_width_minus1;
} arg_qc_c_mienter_nest;
typedef struct {
    int unused_rs1;
    int unused_width_minus1;
} arg_qc_c_mienter;
typedef struct {
    int rs1;
    int unused_width_minus1;
} arg_qc_c_eir;
typedef struct {
    int rd;
    int unused_width_minus1;
} arg_qc_c_dir;
typedef struct {
    int rs1;
    int unused_width_minus1;
} arg_qc_c_clrint;
typedef struct {
    int unused_rs1;
    int unused_width_minus1;
} arg_qc_c_di;
typedef struct {
    int unused_rs1;
    int unused_width_minus1;
} arg_qc_c_ei;
bool trans_qc_c_setint_qc_c_extu_qc_c_mienter_nest_qc_c_mienter_qc_c_eir_qc_c_dir_qc_c_clrint_qc_c_di_qc_c_ei(DisasContext *ctx, arg_qc_c_setint_qc_c_extu_qc_c_mienter_nest_qc_c_mienter_qc_c_eir_qc_c_dir_qc_c_clrint_qc_c_di_qc_c_ei *arg)
{
    switch(arg->width_minus1) {
    case 2:
        trans_qc_c_setint(ctx, (arg_qc_c_setint *) arg);
        break;
    case 4:
        switch(arg->rs1) {
        case 17:
            trans_qc_c_mienter_nest(ctx, (arg_qc_c_mienter_nest *) arg);
            break;
        case 16:
            trans_qc_c_mienter(ctx, (arg_qc_c_mienter *) arg);
            break;
        case 22:
            trans_qc_c_di(ctx, (arg_qc_c_di *) arg);
            break;
        case 23:
            trans_qc_c_ei(ctx, (arg_qc_c_ei *) arg);
            break;
        }
        break;
    case 1:
        trans_qc_c_eir(ctx, (arg_qc_c_eir *) arg);
        break;
    case 0:
        trans_qc_c_dir(ctx, (arg_qc_c_dir *) arg);
        break;
    case 3:
        trans_qc_c_clrint(ctx, (arg_qc_c_clrint *) arg);
        break;
    default:
        trans_qc_c_extu(ctx, (arg_qc_c_extu *) arg);
    }
}

