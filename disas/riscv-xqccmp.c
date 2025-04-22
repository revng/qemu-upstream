#include "qemu/osdep.h"
#include "qemu/bitops.h"
#include "disas/riscv.h"
#include "disas/riscv-xqci.h"

typedef enum {
    rv_op_qc_cm_mva01s = 1,
    rv_op_qc_cm_mvsa01,
    rv_op_qc_cm_pop,
    rv_op_qc_cm_popret,
    rv_op_qc_cm_popretz,
    rv_op_qc_cm_push,
    rv_op_qc_cm_pushfp,
} rv_xqci_opcode;

const rv_opcode_data xqci_opcode_data[] = {
    { "qc.illegal", rv_codec_illegal, rv_fmt_none, NULL, 0, 0, 0 },
    { "qc.cm.mva01s", rv_codec_skip, "O\t2,1", NULL, 0, 0, 0 },
    { "qc.cm.mvsa01", rv_codec_skip, "O\t2,1", NULL, 0, 0, 0 },
    { "qc.cm.pop", rv_codec_skip, "O\ti,k", NULL, 0, 0, 0 },
    { "qc.cm.popret", rv_codec_skip, "O\ti,k", NULL, 0, 0, 0 },
    { "qc.cm.popretz", rv_codec_skip, "O\ti,k", NULL, 0, 0, 0 },
    { "qc.cm.push", rv_codec_skip, "O\ti,k", NULL, 0, 0, 0 },
    { "qc.cm.pushfp", rv_codec_skip, "O\ti,k", NULL, 0, 0, 0 },
};

static uint64_t decode_xqci_48_impl_load_bytes(rv_decode *dec, uint64_t insn, int offset, int length)
{
    return 0;
}
#include "riscv-xqci-16-decode.c.inc"
#include "riscv-xqci-32-decode.c.inc"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
#include "riscv-xqci-48-decode.c.inc"
#pragma GCC diagnostic pop
#include "riscv-xqci-trans.c.inc"

void decode_xqci(rv_decode *dec, rv_isa isa) {
    rv_inst inst = dec->inst;
    dec->op = rv_op_illegal;
    switch (dec->inst_length) {
    case 2:
        decode_xqci_16_impl(dec, inst);
        break;
    case 4:
        decode_xqci_32_impl(dec, inst);
        break;
    case 6:
        decode_xqci_48_impl(dec, inst << 16);
        break;
    }
}
