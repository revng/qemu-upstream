#!/usr/bin/env python3

import argparse
import json
import os
import shlex
import sys

def main():
    parser = argparse.ArgumentParser(description='Produce the LLVM IR for a given source file.')
    parser.add_argument("clang", metavar="CLANG_PATH", help="Path to clang.")
    parser.add_argument("input_path", metavar="INPUT_PATH", help="Source file input path")
    parser.add_argument("output_path", metavar="OUTPUT_PATH", help="Path for the output.")
    parser.add_argument('-S', action='store_true', help="Emit textual LLVM IR.")
    args = parser.parse_args()

    with open("compile_commands.json", "r") as compile_commands_file:
        compile_commands = json.load(compile_commands_file)
        for compile_command in compile_commands:
            if not compile_command["file"].endswith(args.input_path):
                continue

            os.chdir(compile_command["directory"])
            command = compile_command["command"]
            argv = shlex.split(command)
            argv[0] = args.clang

            if args.S:
                argv.append("-S")

            argv += ["-emit-llvm", "-o", args.output_path]

            os.execvp(argv[0], argv)

if __name__ == "__main__":
    sys.exit(main())
