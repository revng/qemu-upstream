static bool trans_mnret(rv_decode *dec, arg_mnret *arg)
{
    dec->op = rv_op_mnret;
    return true;
}
