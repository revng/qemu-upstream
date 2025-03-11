import yaml
import sys
import re

decode_only = {
    'qc.brev32.yaml',
    'qc.lwmi.yaml',
    'qc.lwm.yaml',
    'qc.swmi.yaml',
    'qc.swm.yaml',
    'qc.setwmi.yaml',
    'qc.setwm.yaml',
    'qc.shlsat.yaml',
    'qc.shlusat.yaml',
    'qc.c.mveqz.yaml',
    'qc.c.mienter.yaml',
    'qc.c.mienter.nest.yaml',
    'qc.c.mileaveret.yaml',
}

system_only = {
    'qc.c.mienter',
    'qc.c.mienter.nest',
    'qc.c.mileaveret',
}

def ranges_in_location(loc_str):
    for r in loc_str.split('|'):
        if '-' in r:
            offsets = [int(s) for s in r.split('-')]
            yield (offsets[1], offsets[0] - offsets[1] + 1)
        else:
            yield (int(r), 1)

def var_is_imm(op, name):
    return f'X[{name}]' not in op and f'X[{name}+8]' not in op

def inst_is_compressed(y):
    return '.c.' in y['name']

def variables(y):
    return y['encoding']['variables'] if 'variables' in y['encoding'] else []

def variable_map(y):
    map = {}
    for v in variables(y):
        map[v['name']] = v
    return map

def load_yaml_or_exit(path):
    with open(path, 'r') as f:
        try:
            return yaml.safe_load(f)
        except yaml.YAMLError as e:
            print(f'Failed to load yaml file {path}: {e}', file=sys.stderr)
            exit(1)

def op_to_cpp(op):
    op = re.sub(r'#', r'//', op)
    op = re.sub(r'\$signed', r'_signed', op)
    op = re.sub(r'\$encoding', r'0', op)

    op = re.sub(r'raise (.*) if (.*);', r'// \1', op)

    op = re.sub(r'for \(', r'#pragma unroll\nfor (', op)

    op = re.sub(r'\(1 << ([a-zA-Z0-9]+)\)', r'(1ul << \1.value)', op)

    op = re.sub(r'{XLEN{1\'b0}}', r'0u', op)
    op = re.sub(r'{XLEN{1\'b1}}', r'~0u', op)
    op = re.sub(r'Bits<{1\'b0, XLEN}\*2> pair = {X\[rs1 \+ 1\], X\[rs1\]};', r'uint64_t pair = ((uint64_t) X[rs1+1].value << 32) | ((uint64_t) X[rs1].value);', op)
    op = re.sub(r'{{XLEN{X\[([a-zA-Z0-9]+)\]\[xlen\(\)-1\]}}, X\[\1\]}', r'((int64_t)(int32_t)X[\1].value)', op)

    op = re.sub(r"([0-9]+)'b([0-9]+)", r'XRegRange(0b\2, \1)', op)
    op = re.sub(r'\[([0-9]+):([0-9]+)\]', r'.range(\2, \1)', op)
    op = re.sub(r'implemented\?', r'implemented', op)
    op = re.sub(r'\$pc', r'pc', op)
    op = re.sub(r'jump_halfword\(([a-z_A-Z]+)[ ]+\+[ ]+([a-z_A-Z\(\)]+)\)', r'xqci_jump_pcrel(\1, \2)', op)
    return op
