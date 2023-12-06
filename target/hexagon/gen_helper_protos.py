#!/usr/bin/env python3

##
##  Copyright(c) 2019-2023 Qualcomm Innovation Center, Inc. All Rights Reserved.
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, see <http://www.gnu.org/licenses/>.
##

import sys
import re
import string
import hex_common

##
## Helpers for gen_helper_prototype
##
def_helper_types = {
    "N": "s32",
    "O": "s32",
    "P": "s32",
    "M": "s32",
    "C": "s32",
    "R": "s32",
    "V": "ptr",
    "Q": "ptr",
}

def_helper_types_pair = {
    "R": "s64",
    "C": "s64",
    "S": "s64",
    "G": "s64",
    "V": "ptr",
    "Q": "ptr",
}


def gen_def_helper_opn(f, tag, regtype, regid, i):
    if hex_common.is_pair(regid):
        f.write(f", {def_helper_types_pair[regtype]}")
    elif hex_common.is_single(regid):
        f.write(f", {def_helper_types[regtype]}")
    else:
        hex_common.bad_register(regtype, regid)


##
## Generate the DEF_HELPER prototype for an instruction
##     For A2_add: Rd32=add(Rs32,Rt32)
##     We produce:
##         DEF_HELPER_3(A2_add, s32, env, s32, s32)
##
def gen_helper_prototype(f, name, tag, tagregs, tagimms):
    regs = tagregs[tag]
    imms = tagimms[tag]

    numresults = 0
    numscalarresults = 0
    numscalarreadwrite = 0
    for regtype, regid in regs:
        if hex_common.is_written(regid):
            numresults += 1
            if hex_common.is_scalar_reg(regtype):
                numscalarresults += 1
        if hex_common.is_readwrite(regid):
            if hex_common.is_scalar_reg(regtype):
                numscalarreadwrite += 1

    if numscalarresults > 1:
        ## The helper is bogus when there is more than one result
        f.write(f"DEF_HELPER_1({tag}, void, env)\n")
    else:
        ## Figure out how many arguments the helper will take
        if numscalarresults == 0:
            def_helper_size = len(regs) + len(imms) + numscalarreadwrite + 1
            if hex_common.need_pkt_has_multi_cof(tag):
                def_helper_size += 1
            if hex_common.need_pkt_need_commit(tag):
                def_helper_size += 1
            if hex_common.need_part1(tag):
                def_helper_size += 1
            if hex_common.need_slot(tag):
                def_helper_size += 1
            if hex_common.need_PC(tag):
                def_helper_size += 1
            if hex_common.helper_needs_next_PC(tag):
                def_helper_size += 1
            if hex_common.need_condexec_reg(tag, regs):
                def_helper_size += 1
            f.write(f"DEF_HELPER_{def_helper_size}({tag}")
            ## The return type is void
            f.write(", void")
        else:
            def_helper_size = len(regs) + len(imms) + numscalarreadwrite
            if hex_common.need_pkt_has_multi_cof(tag):
                def_helper_size += 1
            if hex_common.need_pkt_need_commit(tag):
                def_helper_size += 1
            if hex_common.need_part1(tag):
                def_helper_size += 1
            if hex_common.need_slot(tag):
                def_helper_size += 1
            if hex_common.need_PC(tag):
                def_helper_size += 1
            if hex_common.need_condexec_reg(tag, regs):
                def_helper_size += 1
            if hex_common.helper_needs_next_PC(tag):
                def_helper_size += 1
            f.write(f"DEF_HELPER_{def_helper_size}({tag}")

        ## Generate the qemu DEF_HELPER type for each result
        ## Iterate over this list twice
        ## - Emit the scalar result
        ## - Emit the vector result
        i = 0
        for regtype, regid in regs:
            if hex_common.is_written(regid):
                if not hex_common.is_hvx_reg(regtype):
                    gen_def_helper_opn(f, tag, regtype, regid, i)
                i += 1

        ## Put the env between the outputs and inputs
        f.write(", env")
        i += 1

        # Second pass
        for regtype, regid in regs:
            if hex_common.is_written(regid):
                if hex_common.is_hvx_reg(regtype):
                    gen_def_helper_opn(f, tag, regtype, regid, i)
                    i += 1

        ## For conditional instructions, we pass in the destination register
        if "A_CONDEXEC" in hex_common.attribdict[tag]:
            for regtype, regid in regs:
                if hex_common.is_writeonly(regid) and not hex_common.is_hvx_reg(
                    regtype
                ):
                    gen_def_helper_opn(f, tag, regtype, regid, i)
                    i += 1

        ## Generate the qemu type for each input operand (regs and immediates)
        for regtype, regid in regs:
            if hex_common.is_read(regid):
                if hex_common.is_hvx_reg(regtype) and hex_common.is_readwrite(regid):
                    continue
                gen_def_helper_opn(f, tag, regtype, regid, i)
                i += 1
        for immlett, bits, immshift in imms:
            f.write(", s32")

        ## Add the arguments for the instruction pkt_has_multi_cof,
        ## pkt_needs_commit, PC, next_PC, slot, and part1 (if needed)
        if hex_common.need_pkt_has_multi_cof(tag):
            f.write(", i32")
        if hex_common.need_pkt_need_commit(tag):
            f.write(', i32')
        if hex_common.need_PC(tag):
            f.write(", i32")
        if hex_common.helper_needs_next_PC(tag):
            f.write(", i32")
        if hex_common.need_slot(tag):
            f.write(", i32")
        if hex_common.need_part1(tag):
            f.write(" , i32")
        f.write(")\n")


def main():
    hex_common.read_semantics_file(sys.argv[1])
    hex_common.read_attribs_file(sys.argv[2])
    hex_common.read_overrides_file(sys.argv[3])
    hex_common.read_overrides_file(sys.argv[4])
    ## Whether or not helper2tcg is enabled is
    ## determined by the number of arguments to
    ## this script:
    ##
    ##   5 args. -> not enabled,
    ##   6 args. -> helper2tcg enabled.
    ##
    ## The 6:th arg. then holds a list of the successfully
    ## parsed instructions.
    is_helper2tcg_enabled = len(sys.argv) > 6
    if is_helper2tcg_enabled:
        hex_common.read_helper2tcg_enabled_file(sys.argv[5])
    hex_common.calculate_attribs()
    tagregs = hex_common.get_tagregs()
    tagimms = hex_common.get_tagimms()

    output_file = sys.argv[-1]
    with open(output_file, "w") as f:
        for tag in hex_common.tags:
            ## Skip the priv instructions
            if "A_PRIV" in hex_common.attribdict[tag]:
                continue
            ## Skip the guest instructions
            if "A_GUEST" in hex_common.attribdict[tag]:
                continue
            ## Skip the diag instructions
            if tag == "Y6_diag":
                continue
            if tag == "Y6_diag0":
                continue
            if tag == "Y6_diag1":
                continue
            #if hex_common.skip_qemu_helper(tag):
            #    continue
            if tag.endswith("_locked"):
                continue

            # Disable all overrides
            if ( tag.startswith("F") and not tag in {
                'F2_sfimm_p',
                'F2_sfimm_n',
                'F2_dfimm_p',
                'F2_dfimm_n',
                'F2_dfmpyll',
                'F2_dfmpylh',
                }):
                continue
            if tag.startswith("V6") and "hist" in tag:
                continue
            # helper of V6_vcombine does not handle overlapping src/dst
            if tag == "V6_vcombine":
                continue;
            if tag.startswith("J2"):
                continue
            if tag.endswith("_pcr") or tag.endswith("_pci"):
                continue
            if tag in {"S2_allocframe", "L2_deallocframe", "S2_cabacdecbin", "SL2_deallocframe", "SS2_allocframe"}:
                continue
            if tag in {"SA1_clrtnew", "SA1_clrfnew", "SA1_cmpeqi"}:
                continue
            if tag.startswith("Y2") or tag.startswith("Y4") or tag.startswith("Y5"):
                continue
            if tag.startswith("R6"):
                continue
            # End disable all overrides

            if "V6_vS" in tag or "V6_vL" in tag:
                continue
            if "A_COF" in hex_common.attribdict[tag]:
                continue

            gen_helper_prototype(f, tag, tag, tagregs, tagimms)


if __name__ == "__main__":
    main()
