#!/usr/bin/env python3

import argparse
import json
import os
import shlex
import sys
import subprocess

common_files = [
    "accel/tcg/tcg-runtime.c",
    "accel/tcg/tcg-runtime-gvec.c",
    "accel/tcg/user-exec.c",
    "linux-user/main.c",
    "linux-user/syscall.c",
    "linux-user/thunk.c",
    "linux-user/mmap.c",
    "linux-user/signal.c",
    "linux-user/uaccess.c",
    "linux-user/uname.c",
    "linux-user/fd-trans.c",
    "util/cutils.c",
    "util/interval-tree.c",
    "fpu/softfloat.c",
    "trace/trace-linux_user.c",
    #"trace/trace-qom.c",
    "trace/control.c",
    #"qom/object.c",
]

helpers = {
    "arm" : [
        "target/arm/helper.c",
        "target/arm/tcg/crypto_helper.c",
        "target/arm/tcg/hflags.c",
        "target/arm/tcg/iwmmxt_helper.c",
        "target/arm/tcg/m_helper.c",
        "target/arm/tcg/mve_helper.c",
        "target/arm/tcg/neon_helper.c",
        "target/arm/tcg/op_helper.c",
        "target/arm/tcg/tlb_helper.c",
        "target/arm/tcg/translate-m-nocp.c",
        "target/arm/tcg/translate-mve.c",
        "target/arm/tcg/translate-neon.c",
        "target/arm/tcg/translate-vfp.c",
        "target/arm/tcg/translate.c",
        "target/arm/tcg/vec_helper.c",
        "target/arm/vfp_helper.c",
        "target/arm/debug_helper.c",
        "linux-user/arm/nwfpe/fpa11.c",
        "linux-user/arm/nwfpe/fpa11_cpdo.c",
        "linux-user/arm/nwfpe/fpa11_cpdt.c",
        "linux-user/arm/nwfpe/fpa11_cprt.c",
        "linux-user/arm/nwfpe/fpopcode.c",
        "linux-user/arm/nwfpe/single_cpdo.c",
        "linux-user/arm/nwfpe/double_cpdo.c",
        "linux-user/arm/nwfpe/extended_cpdo.c",
        "linux-user/arm/cpu_loop.c",
        "linux-user/arm/signal.c",
    ],
    "aarch64" : [
        "target/arm/helper.c",
        "target/arm/debug_helper.c",
        "target/arm/tcg/crypto_helper.c",
        "target/arm/tcg/helper-a64.c",
        "target/arm/tcg/hflags.c",
        "target/arm/tcg/iwmmxt_helper.c",
        "target/arm/tcg/m_helper.c",
        "target/arm/tcg/mte_helper.c",
        "target/arm/tcg/mve_helper.c",
        "target/arm/tcg/neon_helper.c",
        "target/arm/tcg/op_helper.c",
        "target/arm/tcg/pauth_helper.c",
        "target/arm/tcg/sme_helper.c",
        "target/arm/tcg/sve_helper.c",
        "target/arm/tcg/tlb_helper.c",
        "target/arm/tcg/translate-a64.c",
        "target/arm/tcg/translate-m-nocp.c",
        "target/arm/tcg/translate-mve.c",
        "target/arm/tcg/translate-neon.c",
        "target/arm/tcg/translate-sme.c",
        "target/arm/tcg/translate-sve.c",
        "target/arm/tcg/translate-vfp.c",
        "target/arm/tcg/translate.c",
        "target/arm/tcg/vec_helper.c",
        "target/arm/vfp_helper.c",
        "linux-user/aarch64/cpu_loop.c",
        "linux-user/aarch64/signal.c",
    ],
    "s390x" : [
        "target/s390x/tcg/cc_helper.c",
        "target/s390x/tcg/crypto_helper.c",
        "target/s390x/tcg/excp_helper.c",
        "target/s390x/tcg/fpu_helper.c",
        "target/s390x/tcg/int_helper.c",
        "target/s390x/tcg/mem_helper.c",
        "target/s390x/tcg/misc_helper.c",
        "target/s390x/tcg/translate.c",
        "target/s390x/tcg/vec_fpu_helper.c",
        "target/s390x/tcg/vec_helper.c",
        "target/s390x/tcg/vec_int_helper.c",
        "target/s390x/tcg/vec_string_helper.c",
        "target/s390x/interrupt.c",
        "target/s390x/cpu-dump.c",
        "linux-user/s390x/cpu_loop.c",
        "linux-user/s390x/signal.c",
    ],
    "i386" : [
        "target/i386/tcg/misc_helper.c",
        "target/i386/tcg/translate.c",
        "target/i386/tcg/bpt_helper.c",
        "target/i386/tcg/cc_helper.c",
        "target/i386/tcg/excp_helper.c",
        "target/i386/tcg/fpu_helper.c",
        "target/i386/tcg/int_helper.c",
        "target/i386/tcg/mem_helper.c",
        "target/i386/tcg/mpx_helper.c",
        "target/i386/tcg/seg_helper.c",
        "target/i386/tcg/user/seg_helper.c",
        "target/i386/helper.c",
        "linux-user/i386/cpu_loop.c",
        "linux-user/i386/signal.c",
    ],
    "x86_64" : [
        "target/i386/tcg/misc_helper.c",
        "target/i386/tcg/translate.c",
        "target/i386/tcg/bpt_helper.c",
        "target/i386/tcg/cc_helper.c",
        "target/i386/tcg/excp_helper.c",
        "target/i386/tcg/fpu_helper.c",
        "target/i386/tcg/int_helper.c",
        "target/i386/tcg/mem_helper.c",
        "target/i386/tcg/mpx_helper.c",
        "target/i386/tcg/seg_helper.c",
        "target/i386/tcg/user/seg_helper.c",
        "target/i386/helper.c",
        "linux-user/x86_64/cpu_loop.c",
        "linux-user/x86_64/signal.c",
    ],
    "mips" : [
        "target/mips/tcg/dsp_helper.c",
        "target/mips/tcg/exception.c",
        "target/mips/tcg/fpu_helper.c",
        "target/mips/tcg/ldst_helper.c",
        "target/mips/tcg/lmmi_helper.c",
        "target/mips/tcg/msa_helper.c",
        "target/mips/tcg/msa_translate.c",
        "target/mips/tcg/op_helper.c",
        "target/mips/tcg/translate.c",
        "target/mips/tcg/vr54xx_helper.c",
        "target/mips/tcg/vr54xx_translate.c",
        "target/mips/tcg/mxu_translate.c",
        "linux-user/mips/cpu_loop.c",
        "linux-user/mips/signal.c",
    ],
    "mipsel" : [
        "target/mips/tcg/dsp_helper.c",
        "target/mips/tcg/exception.c",
        "target/mips/tcg/fpu_helper.c",
        "target/mips/tcg/ldst_helper.c",
        "target/mips/tcg/lmmi_helper.c",
        "target/mips/tcg/msa_helper.c",
        "target/mips/tcg/msa_translate.c",
        "target/mips/tcg/op_helper.c",
        "target/mips/tcg/translate.c",
        "target/mips/tcg/vr54xx_helper.c",
        "target/mips/tcg/vr54xx_translate.c",
        "target/mips/tcg/mxu_translate.c",
        "linux-user/mips/cpu_loop.c",
        "linux-user/mips/signal.c",
    ],
    "hexagon" : [
        "target/hexagon/decode.c",
        "target/hexagon/genptr.c",
        "target/hexagon/op_helper.c",
        "target/hexagon/translate.c",
        "target/hexagon/arch.c",
        "linux-user/hexagon/cpu_loop.c",
        "linux-user/hexagon/signal.c",
    ],
    "loongarch64" : [
        "target/loongarch/fpu_helper.c",
        "target/loongarch/op_helper.c",
        "target/loongarch/translate.c",
        "target/loongarch/vec_helper.c",
        "target/loongarch/cpu.c",
        "linux-user/loongarch64/cpu_loop.c",
        "linux-user/loongarch64/signal.c",
    ],
    "aarch64_be" : [],
    "alpha" : [],
    "armeb" : [],
    "cris" : [],
    "hppa" : [],
    "m68k" : [],
    "microblazeel" : [],
    "microblaze" : [],
    "mips64el" : [],
    "mips64" : [],
    "mipsn32el" : [],
    "mipsn32" : [],
    "nios2" : [],
    "or1k" : [],
    "ppc64le" : [],
    "ppc64" : [],
    "ppc" : [],
    "riscv32" : [],
    "riscv64" : [],
    "s390x" : [
        "target/s390x/tcg/cc_helper.c",
        "target/s390x/tcg/crypto_helper.c",
        "target/s390x/tcg/excp_helper.c",
        "target/s390x/tcg/fpu_helper.c",
        "target/s390x/tcg/int_helper.c",
        "target/s390x/tcg/mem_helper.c",
        "target/s390x/tcg/misc_helper.c",
        "target/s390x/tcg/translate.c",
        "target/s390x/tcg/vec_fpu_helper.c",
        "target/s390x/tcg/vec_helper.c",
        "target/s390x/tcg/vec_int_helper.c",
        "target/s390x/tcg/vec_string_helper.c",
        "linux-user/s390x/cpu_loop.c",
        "linux-user/s390x/signal.c",
    ],
    "sh4eb" : [],
    "sh4" : [],
    "sparc32plus" : [],
    "sparc64" : [],
    "sparc" : [],
    "xtensaeb" : [],
    "xtensa" : [],
}

def run_command(argv):
    proc = subprocess.Popen(argv, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out,err = proc.communicate()
    if proc.wait() != 0:
        print("Command exited with %d:\n%s\n" % (proc.returncode," ".join(argv)))
        print(f"out:\n{out}\n")
        print(f"err:\n{err}\n")

def is_target_file(output):
    for target in helpers:
        if target in output:
            return True
    return False


def find_compile_commands(target_name, clang_path, input_path):
    if os.path.exists("compile_commands.json"):
        with open("compile_commands.json", "r") as compile_commands_file:
            compile_commands = json.load(compile_commands_file)
            for compile_command in compile_commands:
                path = compile_command["file"]
                #print(f"candidate: {path}");
                #print(f"  abs: {os.path.abspath(path)}");
                #print(f"  abs: {os.path.abspath(input_path)}");
                # For files defined in frontends we check that the absolute path
                # matches. Otherwise, checking for matching endings to also
                # handle auto-generated files that end up in build/.
                #
                # NOTE: this does no handle target-specific auto-generated
                # frontend files that have the same name.
                command = compile_command["command"]
                command_output = compile_command["output"]
                target_file = is_target_file(command_output)
                if os.path.abspath(path) != os.path.abspath(input_path) and not (not target_file and input_path.endswith(path)):
                    continue
                os.chdir(compile_command["directory"])
                if target_file and not command_output.startswith(f"libqemu-{target_name}"):
                    continue

                print(f"{input_path} -> {compile_command['output']}")

                argv = shlex.split(command)
                argv[0] = clang_path
                return argv

    # If we coulnd't find "compile_commands" just default
    # to the following.
    print(f"Unable to find {input_path}")
    assert(False)
    return [clang_path, input_path]

def generate_llvm_ir(target_name, clang_path, input_path, output_path):
    argv = find_compile_commands(target_name, clang_path, input_path)

    for i, arg in reversed(list(enumerate(argv))):
        if arg in {'-MQ', '-o', '-MF'}:
            del argv[i:i+2]

    argv += ["-DGEN_LLVM_HELPERS=1", "-Wno-unknown-warning-option",  "-emit-llvm", "-O0", "-Xclang", "-disable-O0-optnone", "-o", output_path]

    run_command(argv)

def link(llvm_link_path, ll_paths, output_path):
    argv = [llvm_link_path] + ll_paths + ['-o', output_path]
    run_command(argv)

def temp_file(output_folder, file):
    _, containing_folder = os.path.split(os.path.abspath(os.path.dirname(file)))
    dir = os.path.join(output_folder, containing_folder)
    os.makedirs(dir, exist_ok=True)
    filename, _ = os.path.splitext(os.path.basename(file))
    new_file = os.path.join(dir, filename) + ".bc"
    return new_file

def main():
    parser = argparse.ArgumentParser(description='Produce the LLVM IR for a given source file.')
    parser.add_argument('source_dir',  metavar='SOURCE_DIR',            help='qemu source directory')
    parser.add_argument('target_name', metavar='TARGET_NAME',           help='Name of target')
    parser.add_argument('target',      metavar='TARGET',                help='Target')
    parser.add_argument("clang",       metavar="CLANG_PATH",            help="Path to clang.")
    parser.add_argument("llvm_link",   metavar="LLVM_LINK_PATH",        help="Path to llvm-link")
    parser.add_argument("opt",         metavar="OPT_PATH",              help="Path to opt")
    parser.add_argument('output',      metavar='OUTPUT_PATH',           help='Path to output file')
    args = parser.parse_args()

    output_folder = os.path.join("llvm-helpers", args.target_name)
    os.makedirs(output_folder, exist_ok=True)

    # Check that all paths in `helper` actually exist
    has_nonexistent_files = False
    for target in helpers:
        for file in helpers[target]:
            if not os.path.exists(os.path.join(args.source_dir, file)):
                has_nonexistent_files = True
                print(f"Error for {target}: File does not exist {file}")
    assert(not has_nonexistent_files)

    # Generate LLVM IR
    ll_paths = []
    print(args.source_dir)
    for file in common_files + helpers[args.target_name]:
        input_path = os.path.join(args.source_dir, file)
        output_path = temp_file(output_folder, input_path)
        ll_paths.append(output_path)
        generate_llvm_ir(args.target_name, args.clang, input_path, output_path)

    link(args.llvm_link, ll_paths, args.output)

if __name__ == "__main__":
    sys.exit(main())
