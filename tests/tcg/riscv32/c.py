#!/usr/bin/env python3

import argparse
import yaml
import re
import os
import struct
from math import ceil
from ctypes import c_int32
import common

expected_reg = 8
dst_reg = 9
address_reg = 10

func_exit="""
__attribute__((noreturn))
void exit(int code) {
    __asm__ volatile("addi a0, %0, 0\\n"
                     "li a7, 93\\n"
                     "ecall" :: "r"(code));
    for(;;);
}
"""

func_check="""
void check(int cond) {
    if (!cond) {
        exit(255);
    }
}
"""

should_sext = {
    'qc.beqi',
    'qc.bgei',
    'qc.blti',
    'qc.bnei',
    'qc.e.addi',
    'qc.e.beqi',
    'qc.e.bgei',
    'qc.e.bgeui',
    'qc.e.blti',
    'qc.e.bltui',
    'qc.e.bnei',
    'qc.e.j',
    'qc.e.jal',
    'qc.e.lb',
    'qc.e.lh',
    'qc.e.lhu',
    'qc.e.lw',
    'qc.e.sb',
    'qc.e.sh',
    'qc.e.sw',
    'qc.insbri',
    'qc.lieqi',
    'qc.linei',
    'qc.muliadd',
    'qc.mveqi',
    'qc.mvnei',
    'qc.selecteqi',
    'qc.selectieqi',
    'qc.selectinei',
    'qc.selectnei',
    'qc.e.lbu',
}

skip_insn = {
    'qc.c.clrint',
    'qc.c.setint',
    'qc.e.beqi',
    'qc.e.bgei',
    'qc.e.bgeui',
    'qc.e.blti',
    'qc.e.bltui',
    'qc.e.bnei',
    'qc.e.j',
    'qc.e.jal',
}

def ashr32(x, n):
    if x & 0x80000000:
        return (x >> n) | (0xFFFFFFFF << (32 - n))
    else:
        return x >> n


def ashl32(x, n):
    return (x << n) & 0xFFFFFFFF


def sext(imm, len):
    res = c_int32(ashr32(ashl32(imm, 32-len), 32-len)).value
    return res


class CPrinter:
    def __init__(self, out, inst_dir):
        self.yamls = {}
        self.inst_dir = inst_dir
        self.bytes = bytes()
        self.out = out
        self.indent = 0

    def set_indent(self, n):
        self.indent = n

    def line(self, str):
        self.out.write(' ' * self.indent + str + '\n')

    def load(self, inst):
        self.yamls[inst] = common.load_yaml_or_exit(
            os.path.join(self.inst_dir, f"{inst}.yaml"))

    def append(self, inst, *args):
        if not inst in self.yamls:
            self.load(inst)
        encoding = self.yamls[inst]['encoding']
        enc = int(re.sub(r'-', r'0', encoding['match']), 2)
        if 'variables' in encoding:
            len_expected = len(encoding['variables'])
            len_got = len(args)
            if len_got != len_expected:
                print(f'error: {inst} expected {len_expected} args got {len_got}')
                return

            for i, v in enumerate(encoding['variables']):
                offset = 0

                arg_value = args[i]
                if 'left_shift' in v:
                    arg_value >>= v['left_shift']

                ranges = [p for p in common.ranges_in_location(v['location'])]
                for start, length in reversed(ranges):
                    mask = ((1 << length) - 1) << offset
                    arg_chunk = (arg_value & mask) >> offset
                    enc |= (arg_chunk << start)
                    offset += length

        enc_len = len(encoding['match'])
        num_bytes = ceil(enc_len/8)
        self.bytes += struct.pack('<Q', enc)[0:num_bytes]




def output_elf(f, text_bytes):
    # ELF Header
    f.write(b'\x7fELF' + b'\x01'*3 + b'\x00' * 9)  # ELF Header
    f.write(struct.pack('<H', 2))                  # ET_EXEC (Executable)
    # EM_* (riscv32 architecture)
    f.write(struct.pack('<H', 243))
    f.write(struct.pack('<I', 1))                  # Version
    # Entry point (dummy address)
    f.write(struct.pack('<I', 0x10000+52+2*32))
    f.write(struct.pack('<I', 52))                 # Program header offset
    f.write(struct.pack('<I', 0))                  # Section header offset
    f.write(struct.pack('<I', 0))                  # Flags
    f.write(struct.pack('<H', 52))                 # ELF Header size
    f.write(struct.pack('<H', 32))                 # Program header entry size
    f.write(struct.pack('<H', 2))                  # Number of program headers
    f.write(struct.pack('<H', 0))                  # No. section headers
    f.write(struct.pack('<H', 0))                  # No. section headers
    # No. section header string table
    f.write(struct.pack('<H', 0))

    # Program Header (.text)
    f.write(struct.pack('<I', 1))                  # PT_LOAD
    f.write(struct.pack('<I', 0))                  # Offset in the file
    f.write(struct.pack('<I', 0x1000))             # Virtual address
    f.write(struct.pack('<I', 0x1000))             # Physical address
    # Size of the segment in the file
    f.write(struct.pack('<I', 0))
    # Size of the segment in memory
    f.write(struct.pack('<I', 0x1000))
    f.write(struct.pack('<I', 6))                  # R (read) and E (execute)
    f.write(struct.pack('<I', 0x1000))             # Alignment

    # Program Header (.test_data)
    f.write(struct.pack('<I', 1))                  # PT_LOAD
    f.write(struct.pack('<I', 0))                  # Offset in the file
    f.write(struct.pack('<I', 0x10000))            # Virtual address
    f.write(struct.pack('<I', 0x10000))            # Physical address
    # Size of the segment in the file
    f.write(struct.pack('<I', len(text_bytes)))
    # Size of the segment in memory
    f.write(struct.pack('<I', len(text_bytes)))
    f.write(struct.pack('<I', 5))                  # R (read) and E (execute)
    f.write(struct.pack('<I', 0x1000))             # Alignment

    # Text section
    f.write(text_bytes)


def test_has_variables(test):
    return isinstance(test['variables'], list)


def main():
    parser = argparse.ArgumentParser(
        prog='assemble',
        description='Assemble KLEE output to elf tests'
    )
    parser.add_argument('--inst-dir', required=True)
    parser.add_argument('--io-file', required=True)
    parser.add_argument('--inst-name', required=True)
    parser.add_argument('--out', required=True)
    args = parser.parse_args()

    if args.inst_name in skip_insn:
        return

    io_yaml = common.load_yaml_or_exit(args.io_file)
    io_var_map = {}
    for test in io_yaml:
        if test_has_variables(test):
            for v in test['variables']:
                io_var_map[v['name']] = v

    # Skip non arithmetic tests
    for test_index,test in enumerate(io_yaml):
        if 'has_jump' in test:
            return

    with open(f'{args.out}', 'w') as f:
        printer = CPrinter(f, args.inst_dir)
        if not args.inst_name in printer.yamls:
            printer.load(args.inst_name)
        y = printer.yamls[args.inst_name]
        is_compressed = common.inst_is_compressed(y)
        vars = common.variables(y)
        var_map = common.variable_map(y)

        printer.line('#include <stddef.h>')
        printer.line('#include <stdint.h>')

        if 'has_load' in test or 'has_store' in test:
            printer.line('__attribute__((section(".mem_test_section")))')
            printer.line('char data[4096];')

        printer.line(func_exit)
        printer.line(func_check)

        printer.line('void _start() {')
        printer.set_indent(4)

        tmp_index = 0;
        for test_index,test in enumerate(io_yaml):
            expected_result = None

            # TODO(anjo): Not testing jumps in C yet
            #if 'has_jump' in test:
            #    if test['has_jump']['valid_test_jump'] == 0:
            #        continue

            if 'has_valid_test_memop' in test and test['has_valid_test_memop'] == 0:
                continue

            if 'has_load' in test:
                for loadop in test['has_load']:
                    printer.line(f'intptr_t address{tmp_index} = {loadop["address"]};')
                    printer.line(f'*(uint32_t *)address{tmp_index} = {loadop["value"]};')
                    tmp_index += 1

            out_args = []
            in_args = []
            variable_order = []
            fmt = ''
            asm = ''
            reg_s_index = 0
            if test_has_variables(test):
                for i, v in enumerate(reversed(test['variables'])):
                    if not 'in' in v:
                        continue

                    variable_order.append(v['name'])
                    var = vars[len(vars)-1-i]

                    if common.var_is_imm(y['operation()'], v['name']):
                        imm = v['in']
                        if args.inst_name in should_sext:
                            imm = sext(int(v['in']), common.var_size_from_location(var["location"]))

                        if var['name'] == 'width_minus1':
                            imm += 1

                        if 'left_shift' in var:
                            imm >>= var['left_shift']

                        in_args.append(('i', imm))
                    else:
                        name = None
                        value = '0'

                        is_read_write = 'out' in v and 'in' in v
                        if is_read_write:
                            expected_result = v['out']
                            name = f'out{tmp_index}'
                            value = v['in']
                            out_args.append(('+r', name))
                        elif 'out' in v:
                            expected_result = v['out']
                            name = f'out{tmp_index}'
                            out_args.append(('=r', name))
                        elif 'in' in v:
                            name = f'in{tmp_index}'
                            value = v['in']
                            in_args.append(('r', name))

                        if var['name'].startswith('r') and var['name'].endswith('s'):
                            reg_s_index += 1
                            printer.line(f'register unsigned int {name} asm("s{reg_s_index}") = {value};')
                        else:
                            printer.line(f'unsigned int {name} = {value};')
                        
                        tmp_index += 1

                num_vars = len(test['variables'])
                fmts = [f'%{i}' for i in range(0,num_vars)]
                fmt = ', '.join(fmts)

                asm = y['assembly']
                for i,v in enumerate(variable_order):
                    name = v

                    remap_names = {
                        'width_minus1' : 'width',
                    }

                    if name in remap_names:
                        name = remap_names[name]
                    elif v.startswith('rs') or v == 'rd':
                        name = 'x' + v[1:]

                    asm = asm.replace(name, f'%{i}')

            fmt_out = [f'"{r}"({o})'for r,o in out_args]
            fmt_in = [f'"{r}"({o})'for r,o in in_args]
            printer.line(f'__asm__ volatile("{args.inst_name} {asm}" : {", ".join(fmt_out)} : {", ".join(fmt_in)} :);')
            if expected_result != None:
                for _,o in out_args:
                    printer.line(f'check({o} == {expected_result});')

            if 'has_store' in test:
                for storeop in test['has_store']:
                    printer.line(f'intptr_t address{tmp_index} = {storeop["address"]};')
                    fs = "F"*int(2*int(storeop["size"])/8)
                    printer.line(f'check((*(uint32_t*)address{tmp_index} & 0x{fs}) == {storeop["value"]});')
                    tmp_index += 1

        printer.line('exit(0);')
        printer.set_indent(0)
        printer.line('}')

    #for test_index,test in enumerate(io_yaml):
    #    printer.bytes = bytes()

    #    expected_result = None

    #    if 'has_jump' in test:
    #        if test['has_jump']['valid_test_jump'] == 0:
    #            continue

    #    if 'has_valid_test_memop' in test and test['has_valid_test_memop'] == 0:
    #        continue

    #    if 'has_load' in test:
    #        for loadop in test['has_load']:
    #            printer.li(loadop['address'], address_reg)
    #            printer.li(loadop['value'], dst_reg)
    #            printer.append('sw', 0, address_reg, dst_reg)

    #    inst_args = []
    #    if test_has_variables(test):
    #        for i, v in enumerate(test['variables']):
    #            if not 'in' in v:
    #                continue

    #            if common.var_is_imm(y['operation()'], v['name']):
    #                inst_args.append(v['in'])
    #            else:
    #                reg = dst_reg + 1 + i
    #                if 'out' in v:
    #                    reg = dst_reg
    #                    expected_result = v['out']

    #                if 'in' in v:
    #                    printer.li(v['in'], reg)

    #                if is_compressed:
    #                    reg -= 8
    #                inst_args.append(reg)

    #    printer.append(args.inst_name, *inst_args)

    #    if 'has_jump' in test:
    #        if expected_result != None:
    #            printer.check_branch_and_result(expected_result)
    #        else:
    #            printer.check_branch()
    #    else:
    #        if expected_result != None:
    #            printer.check_result(expected_result)

    #    if 'has_store' in test:
    #        for storeop in test['has_store']:
    #            printer.li(storeop['address'], address_reg)
    #            printer.append('lw', 0, dst_reg, address_reg)
    #            printer.check_result(storeop['value'])

    #    printer.exit_success()
    #            
    #    #printer.li(69, 6)
    #    #printer.li(0x1234, 7)
    #    #printer.append('sw', 0, 7, 6)
    #    #printer.append('lw', 0, 5, 7)


if __name__ == '__main__':
    main()
