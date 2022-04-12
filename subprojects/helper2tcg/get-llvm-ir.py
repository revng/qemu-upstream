#!/usr/bin/env python3

import argparse
import json
import os
import shlex
import sys
import subprocess

def log(msg):
    print(msg, file=sys.stderr)

def run_command(command):
    proc = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    if proc.wait() != 0:
        log(f"Command: {' '.join(command)} exited with {proc.returncode}\n")
        log(f"stdout:\n{out}\n")
        log(f"stderr:\n{err}\n")

def find_compile_commands(target, clang_path, input_path):
    if os.path.exists('compile_commands.json'):
        with open('compile_commands.json', 'r') as compile_commands_file:
            compile_commands = json.load(compile_commands_file)
            for compile_command in compile_commands:
                path = compile_command['file']
                if os.path.basename(path) != os.path.basename(input_path):
                    continue

                os.chdir(compile_command['directory'])
                command = compile_command['command']

                # If building mulitple target there's a chance
                # input files share the same path and name.
                # This could cause us to find the wrong compile
                # command, we use the target to distinguish
                # between these.
                if not target in command:
                    continue

                argv = shlex.split(command)
                argv[0] = clang_path
                return argv

    log(f"Unable to find compile commands for {input_path}")
    os.abort()

def generate_llvm_ir(target, clang_path, input_path, output_path):
    command = find_compile_commands(target, clang_path, input_path)

    for i, arg in reversed(list(enumerate(command))):
        if arg in {'-MQ', '-o', '-MF'} or arg.startswith('-O'):
            del command[i:i+2]

    # TODO(anjo): Change -DTCGG to some more descriptive define
    command += ['-S', '-DTCGG', '-O0', '-Xclang', '-disable-O0-optnone', '-emit-llvm', '-o', output_path]
    run_command(command)

def main():
    parser = argparse.ArgumentParser(description='Produce the LLVM IR for a given source file.')
    parser.add_argument('clang',       metavar='CLANG_PATH',            help='Path to clang.')
    parser.add_argument('output_path', metavar='OUTPUT_PATH',           help='Output .ll file path')
    parser.add_argument('input_path',  metavar='INPUT_PATH',            help='Source file input path')
    parser.add_argument('target',      metavar='TARGET',                help='Target')
    args = parser.parse_args()

    generate_llvm_ir(args.target, args.clang, args.input_path, args.output_path)

if __name__ == '__main__':
    sys.exit(main())
