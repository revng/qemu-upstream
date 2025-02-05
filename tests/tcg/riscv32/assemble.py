#!/usr/bin/env python3

import argparse
import yaml
import re
import os
import struct
from math import ceil
from ctypes import c_int32

def ashr32(x, n):
    if x & 0x80000000:
        return (x >> n) | (0xFFFFFFFF << (32 - n))
    else:
        return x >> n

def ashl32(x, n):
    return (x << n) & 0xFFFFFFFF

class InstPrinter:
    def __init__(self, inst_dir):
        self.yamls = {}
        self.inst_dir = inst_dir
        self.bytes = bytes()

        # Manually add encoding for two riscv32 instructions which need to be
        # emitted with different operands in tests.
        try:
            self.yamls['lui'] = yaml.safe_load(
            """
            encoding:
              match:      -------------------------0110111
              variables:
              - name: imm
                location: 31-12
              - name: rd
                location: 11-7
                not: 0
            """
            )
            self.yamls['addi'] = yaml.safe_load(
            """
            encoding:
              match:      -----------------000-----0010011
              variables:
              - name: imm
                location: 31-20
              - name: rs1
                location: 19-15
                not: 0
              - name: rd
                location: 11-7
                not: 0
            """
            )
        except yaml.YAMLError as e:
            print(e)

    def load(self, inst):
        with open(os.path.join(self.inst_dir, f"{inst}.yaml")) as f:
            try:
                y = yaml.safe_load(f)
                self.yamls[inst] = y
            except yaml.YAMLError as e:
                print(e)

    def li(self, N, reg):
        # sign extend low 12 bits
        M = ashr32(ashl32(N, 20), 20)
        # Upper 20 bits
        K = ashr32((c_int32(N).value-c_int32(M).value),12)
        self.append('lui', K, reg)
        self.append('addi', M, reg, reg)

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

            for i,v in enumerate(encoding['variables']):
                offset = 0
                for r in reversed(v['location'].split('|')):
                    start = 0
                    length = 0
                    if '-' in r:
                        offsets = [int(s) for s in r.split('-')]
                        start = offsets[1]
                        length = offsets[0] - offsets[1] + 1
                    else:
                        start = int(r)
                        length = 1

                    mask = ((1 << length) - 1) << offset
                    arg_chunk = (args[i] & mask) >> offset
                    enc |= (arg_chunk << start)

                    offset += length

        enc_len = len(encoding['match'])
        num_bytes = ceil(enc_len/8)
        self.bytes += struct.pack('<Q', enc)[0:num_bytes]

    def exit(self):
        # Hard code checking of expected vs returned value.
        # Only testing against state in returned register.
        self.bytes += bytes.fromhex('6396ef01') # bne t6,t5,12
        self.bytes += bytes.fromhex('13050000') # li a0, 0
        self.bytes += bytes.fromhex('6f008000') # j 8
        self.bytes += bytes.fromhex('1305f00f') # li a0 255
        self.bytes += bytes.fromhex('9308d005') # li a7 93
        self.bytes += bytes.fromhex('73000000') # ecall

def output_elf(f, text_bytes):
    # ELF Header
    f.write(b'\x7fELF' + b'\x01'*3 + b'\x00' * 9)  # ELF Header
    f.write(struct.pack('<H', 2))                  # ET_EXEC (Executable)
    f.write(struct.pack('<H', 243))                # EM_* (riscv32 architecture)
    f.write(struct.pack('<I', 1))                  # Version
    f.write(struct.pack('<I', 0x10000+52+32))      # Entry point (dummy address)
    f.write(struct.pack('<I', 52))                 # Program header offset
    f.write(struct.pack('<I', 0))                  # Section header offset
    f.write(struct.pack('<I', 0))                  # Flags
    f.write(struct.pack('<H', 52))                 # ELF Header size
    f.write(struct.pack('<H', 32))                 # Program header entry size
    f.write(struct.pack('<H', 1))                  # Number of program headers
    f.write(struct.pack('<H', 0))                  # No. section headers
    f.write(struct.pack('<H', 0))                  # No. section headers
    f.write(struct.pack('<H', 0))                  # No. section header string table

    # Program Header
    f.write(struct.pack('<I', 1))                  # PT_LOAD
    f.write(struct.pack('<I', 0))                  # Offset in the file
    f.write(struct.pack('<I', 0x10000))            # Virtual address
    f.write(struct.pack('<I', 0x10000))            # Physical address
    f.write(struct.pack('<I', len(text_bytes)))    # Size of the segment in the file
    f.write(struct.pack('<I', len(text_bytes)))    # Size of the segment in memory
    f.write(struct.pack('<I', 5))                  # R (read) and E (execute)
    f.write(struct.pack('<I', 0x1000))             # Alignment

    # Text section
    f.write(text_bytes)

def main():
    parser = argparse.ArgumentParser(
        prog='assemble',
        description='Assemble KLEE output to elf tests'
    )
    parser.add_argument('--inst-dir')
    parser.add_argument('--io-file')
    parser.add_argument('--inst-name')
    parser.add_argument('--out')
    args = parser.parse_args()

    printer = InstPrinter(args.inst_dir)

    if args.io_file and args.inst_name and args.out:
        if not args.inst_name in printer.yamls:
            printer.load(args.inst_name)
        y = printer.yamls[args.inst_name]

        vars = []
        if 'variables' in y['encoding']:
            vars = [v['name'] for v in y['encoding']['variables']]

        with open(args.io_file, 'r') as f:
            for j,l in enumerate(f.readlines()):
                if len(l) == 0 or l == "\n":
                    break

                printer.bytes = bytes()
                values = [int(i) for i in l.strip().split(' ')]
                assert(len(values) == len(vars))
                dst_reg = 30
                expected = 0
                inst_args = []
                for i in range(0,len(values)):
                    if 'rd' in vars[i]:
                        inst_args.append(dst_reg)
                        expected = values[i]
                    elif 'imm' in vars[i]:
                        inst_args.append(values[i])
                    else:
                        printer.li(values[i], 5+i)
                        inst_args.append(5+i)
                printer.append(args.inst_name, *inst_args)
                printer.li(expected, 31) # t6
                printer.exit()

                with open(f'{args.out}-{j}', 'wb') as f:
                    output_elf(f, printer.bytes)

if __name__ == '__main__':
    main()
