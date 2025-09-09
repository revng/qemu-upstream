static bool trans_mnret(rv_decode *dec, arg_mnret *arg)
{
    if (!dec->cfg->ext_smrnmi) {
        return false;
    }
    dec->op = rv_op_mnret;
    return true;
}
