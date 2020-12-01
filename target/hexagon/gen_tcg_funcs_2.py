#!/usr/bin/env python3

##
##  Copyright(c) 2019-2020 Qualcomm Innovation Center, Inc. All Rights Reserved.
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
from io import StringIO

from hex_common import *

##
## Helpers for gen_tcg_func
##
def gen_decl_ea_tcg(f, tag):
    f.write("    DECL_EA;\n")

def gen_free_ea_tcg(f):
    f.write("    FREE_EA;\n")

def genptr_decl_pair_writeble(f, tag, regtype, regid, regno):
    regN="%s%sN" % (regtype,regid)
    f.write("    TCGv_i64 %s%sV = tcg_temp_local_new_i64();\n" % \
        (regtype, regid))
    f.write("    const int %s = insn->regno[%d];\n" % (regN, regno))
    if ('A_CONDEXEC' in attribdict[tag]):
        f.write("    if (!is_preloaded(ctx, %s)) {\n" % regN)
        f.write("        tcg_gen_mov_tl(hex_new_value[%s], hex_gpr[%s]);\n" % \
                             (regN, regN))
        f.write("    }\n")
        f.write("    if (!is_preloaded(ctx, %s + 1)) {\n" % regN)
        f.write("        tcg_gen_mov_tl(hex_new_value[%s + 1], hex_gpr[%s + 1]);\n" % \
                             (regN, regN))
        f.write("    }\n")

def genptr_decl_writeble(f, tag, regtype, regid, regno):
    regN="%s%sN" % (regtype,regid)
    f.write("    TCGv %s%sV = tcg_temp_local_new();\n" % \
        (regtype, regid))
    f.write("    const int %s = insn->regno[%d];\n" % (regN, regno))
    if ('A_CONDEXEC' in attribdict[tag]):
        f.write("    if (!is_preloaded(ctx, %s)) {\n" % regN)
        f.write("        tcg_gen_mov_tl(hex_new_value[%s], hex_gpr[%s]);\n" % \
                             (regN, regN))
        f.write("    }\n")

def genptr_decl(f,tag,regtype,regid,regno):
    regN="%s%sN" % (regtype,regid)
    regV = "%s%sV" % (regtype, regid)
    macro = "    DECL_%sREG_%s" % (regtype, regid)
    f.write("%s(%s, %s, %d, 0);\n" % \
        (macro, regV, regN, regno))
    return regV

def genptr_decl_new(f,regtype,regid,regno):
    regN="%s%sX" % (regtype,regid)
    regV = "%s%sN" % (regtype,regid)
    macro = "    DECL_NEW_%sREG_%s" % (regtype, regid)
    f.write("%s(%s, %s, %d, 0);\n" % \
        (macro, regV, regN, regno))
    return regV

def genptr_decl_opn(f, tag, regtype, regid, toss, numregs, i):
    if (is_pair(regid)):
        return genptr_decl(f, tag, regtype, regid, i)
    elif (is_single(regid)):
        if is_old_val(regtype, regid, tag):
            return genptr_decl(f, tag, regtype, regid,i)
        elif is_new_val(regtype, regid, tag):
            return genptr_decl_new(f,regtype,regid,i)
        else:
            print("Bad register parse: ",regtype,regid,toss,numregs)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

    return None

def genptr_decl_imm(f,immlett):
    if (immlett.isupper()):
        i = 1
    else:
        i = 0
    immV = imm_name(immlett)
    f.write("    DECL_IMM(%s,%d);\n" % (immV,i))
    return immV

def genptr_free(f,regtype,regid,regno):
    macro = "    FREE_%sREG_%s" % (regtype, regid)
    f.write("%s(%s%sV);\n" % (macro, regtype, regid))

def genptr_free_new(f,regtype,regid,regno):
    macro = "    FREE_NEW_%sREG_%s" % (regtype, regid)
    f.write("%s(%s%sN);\n" % (macro, regtype, regid))

def genptr_free_opn(f,regtype,regid,i,tag):
    if (is_pair(regid)):
        genptr_free(f,regtype,regid,i)
    elif (is_single(regid)):
        if is_old_val(regtype, regid, tag):
            genptr_free(f,regtype,regid,i)
        elif is_new_val(regtype, regid, tag):
            genptr_free_new(f,regtype,regid,i)
        else:
            print("Bad register parse: ",regtype,regid,toss,numregs)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

def genptr_src_read(f,regtype,regid):
    macro = "    READ_%sREG_%s" % (regtype, regid)
    f.write("%s(%s%sV, %s%sN);\n" % \
        (macro,regtype,regid,regtype,regid))

def genptr_src_read_new(f,regtype,regid):
    macro = "    READ_NEW_%sREG_%s" % (regtype, regid)
    f.write("%s(%s%sN, %s%sX);\n" % \
        (macro,regtype,regid,regtype,regid))

def genptr_src_read_opn(f,regtype,regid,tag):
    if (is_pair(regid)):
        genptr_src_read(f,regtype,regid)
    elif (is_single(regid)):
        if is_old_val(regtype, regid, tag):
            genptr_src_read(f,regtype,regid)
        elif is_new_val(regtype, regid, tag):
            genptr_src_read_new(f,regtype,regid)
        else:
            print("Bad register parse: ",regtype,regid,toss,numregs)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

def gen_helper_call_opn(f, tag, regtype, regid, toss, numregs, i):
    if (i > 0): f.write(", ")
    if (is_pair(regid)):
        f.write("%s%sV" % (regtype,regid))
    elif (is_single(regid)):
        if is_old_val(regtype, regid, tag):
            f.write("%s%sV" % (regtype,regid))
        elif is_new_val(regtype, regid, tag):
            f.write("%s%sN" % (regtype,regid))
        else:
            print("Bad register parse: ",regtype,regid,toss,numregs)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

def gen_helper_decl_imm(f,immlett):
    f.write("    DECL_TCG_IMM(tcgv_%s, %s);\n" % \
        (imm_name(immlett), imm_name(immlett)))

def gen_helper_call_imm(f,immlett):
    f.write(", tcgv_%s" % imm_name(immlett))

def gen_helper_free_imm(f,immlett):
    f.write("    FREE_TCG_IMM(tcgv_%s);\n" % imm_name(immlett))

def genptr_dst_write(f, tag, regtype, regid):
    macro = "    WRITE_%sREG_%s" % (regtype, regid)
    f.write("%s(%s%sN, %s%sV);\n" % (macro, regtype, regid, regtype, regid))

def genptr_dst_write_opn(f,regtype, regid, tag):
    if (is_pair(regid)):
        genptr_dst_write(f, tag, regtype, regid)
    elif (is_single(regid)):
        genptr_dst_write(f, tag, regtype, regid)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

##
## Generate the TCG code to call the helper
##     For A2_add: Rd32=add(Rs32,Rt32), { RdV=RsV+RtV;}
##     We produce:
##    static void generate_A2_add()
##                    CPUHexagonState *env
##                    DisasContext *ctx,
##                    Insn *insn,
##                    Packet *pkt)
##       {
##           TCGv RdV = tcg_temp_local_new();
##           const int RdN = insn->regno[0];
##           TCGv RsV = hex_gpr[insn->regno[1]];
##           TCGv RtV = hex_gpr[insn->regno[2]];
##           <GEN>
##           gen_log_reg_write(RdN, RdV);
##           ctx_log_reg_write(ctx, RdN);
##           tcg_temp_free(RdV);
##       }
##
##       where <GEN> depends on skip_qemu_helper(tag)
##       if skip_qemu_helper(tag) is True
##       <GEN>  is fGEN_TCG_A2_add({ RdV=RsV+RtV;});
##       if skip_qemu_helper(tag) is False
##       <GEN>  is gen_helper_A2_add(RdV, cpu_env, RsV, RtV);
##
def gen_tcg_func(f, tag, regs, imms):
    f.write(tag + ' {\n')
    if need_ea(tag): gen_decl_ea_tcg(f, tag)
    i=0
    ## Declare all the operands (regs and immediates)
    declared = []
    for regtype,regid,toss,numregs in regs:
        declared.append(genptr_decl_opn(f, tag, regtype, regid, toss, numregs, i))
        i += 1
    for immlett,bits,immshift in imms:
        declared.append(genptr_decl_imm(f,immlett))

    declared = [name for name in declared if name is not None]
    ## When M.V is passed, pass also M.N
    for i, name in enumerate(declared[:]):
        match = re.search("M.V", name)
        if match is not None:
            declared.insert(i+1, match.string.replace("V", "N"))

    ## Instead of passing N.N, pass N.V
    for i, name in enumerate(declared):
        declared[i] = re.sub(r"N([esdtuvxy])N", r"N\1X", name)

    if 'A_PRIV' in attribdict[tag]:
        f.write('    fCHECKFORPRIV();\n')
    if 'A_GUEST' in attribdict[tag]:
        f.write('    fCHECKFORGUEST();\n')

    ## Read all the inputs
    for regtype,regid,toss,numregs in regs:
        if (is_read(regid)):
            genptr_src_read_opn(f,regtype,regid,tag)

    if True:
        f.write("    %s\n" % semdict[tag])
    elif ( skip_qemu_helper(tag) ):
        f.write("    fGEN_TCG_%s(%s);\n" % (tag, semdict[tag]))
    else:
        ## Generate the call to the helper
        for immlett,bits,immshift in imms:
            gen_helper_decl_imm(f,immlett)
        if need_part1(tag):
            f.write("    TCGv part1 = tcg_const_tl(insn->part1);\n")
        if need_slot(tag):
            f.write("    TCGv slot = tcg_const_tl(insn->slot);\n")
        f.write("    gen_helper_%s(" % (tag))
        i=0
        ## If there is a scalar result, it is the return type
        for regtype,regid,toss,numregs in regs:
            if (is_written(regid)):
                gen_helper_call_opn(f, tag, regtype, regid, toss, numregs, i)
                i += 1
        if (i > 0): f.write(", ")
        f.write("cpu_env")
        i=1
        for regtype,regid,toss,numregs in regs:
            if (is_read(regid)):
                gen_helper_call_opn(f, tag, regtype, regid, toss, numregs, i)
                i += 1
        for immlett,bits,immshift in imms:
            gen_helper_call_imm(f,immlett)

        if need_slot(tag): f.write(", slot")
        if need_part1(tag): f.write(", part1" )
        f.write(");\n")
        if need_slot(tag):
            f.write("    tcg_temp_free(slot);\n")
        if need_part1(tag):
            f.write("    tcg_temp_free(part1);\n")
        for immlett,bits,immshift in imms:
            gen_helper_free_imm(f,immlett)

    ## Write all the outputs
    for regtype,regid,toss,numregs in regs:
        if (is_written(regid)):
            genptr_dst_write_opn(f,regtype, regid, tag)

    ## Free all the operands (regs and immediates)
    if need_ea(tag): gen_free_ea_tcg(f)
    for regtype,regid,toss,numregs in regs:
        genptr_free_opn(f,regtype,regid,i,tag)
        i += 1

    f.write("}\n\n")

def gen_def_tcg_func(f, tag, tagregs, tagimms):
    regs = tagregs[tag]
    imms = tagimms[tag]

    gen_tcg_func(f, tag, regs, imms)

def main():
    read_semantics_file(sys.argv[1])
    read_attribs_file(sys.argv[2])
    read_overrides_file(sys.argv[3])
    calculate_attribs()
    tagregs = get_tagregs()
    tagimms = get_tagimms()

    f = StringIO()

    f.write('#include "macros.h"\n\n')

    for tag in tags:
        ## Skip the priv instructions
        if ( "A_PRIV" in attribdict[tag] ) :
            continue
        ## Skip the guest instructions
        if ( "A_GUEST" in attribdict[tag] ) :
            continue
        ## Skip the diag instructions
        if ( tag == "Y6_diag" ) :
            continue
        if ( tag == "Y6_diag0" ) :
            continue
        if ( tag == "Y6_diag1" ) :
            continue

        gen_def_tcg_func(f, tag, tagregs, tagimms)

    realf = open(sys.argv[4], 'w')
    realf.write(f.getvalue())
    realf.close()
    f.close()

if __name__ == "__main__":
    main()
