#include "qemu/osdep.h"
#include "qemu/bitops.h"
#include "disas/riscv.h"
#include "disas/riscv-smrnmi.h"

typedef enum {
    rv_op_mnret = 1,
} rv_smrnmi_opcode;

const rv_opcode_data smrnmi_opcode_data[] = {
    { "qc.illegal", rv_codec_illegal, rv_fmt_none, NULL, 0, 0, 0 },
    { "mnret", rv_codec_skip, "O\t", NULL, 0, 0, 0 },
};

#include "riscv-smrnmi-32-decode.c.inc"
#include "riscv-smrnmi-trans.c.inc"

void decode_smrnmi(rv_decode *dec, rv_isa isa) {
    rv_inst inst = dec->inst;
    dec->op = rv_op_illegal;
    switch (dec->inst_length) {
    case 4:
        decode_smrnmi_32_impl(dec, inst);
        break;
    }
}
