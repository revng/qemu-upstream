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
    if ('A_CONDEXEC' in attribdict[tag] or
        'A_LOAD' in attribdict[tag]):
        f.write("    TCGv EA = tcg_temp_local_new();\n")
    else:
        f.write("    TCGv EA = tcg_temp_new();\n")

def gen_free_ea_tcg(f):
    f.write("    tcg_temp_free(EA);\n")

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

def genptr_decl(f, tag, regtype, regid, regno):
    regN="%s%sN" % (regtype,regid)
    if (regtype == "R"):
        if (regid in {"ss", "tt"}):
            f.write("    TCGv_i64 %s%sV = tcg_temp_local_new_i64();\n" % \
                (regtype, regid))
            f.write("    const int %s = insn->regno[%d];\n" % \
                (regN, regno))
        elif (regid in {"dd", "ee", "xx", "yy"}):
            genptr_decl_pair_writeble(f, tag, regtype, regid, regno)
        elif (regid in {"s", "t", "u", "v"}):
            f.write("    TCGv %s%sV = hex_gpr[insn->regno[%d]];\n" % \
                (regtype, regid, regno))
        elif (regid in {"d", "e", "x", "y"}):
            genptr_decl_writeble(f, tag, regtype, regid, regno)
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid in {"s", "t", "u", "v"}):
            f.write("    TCGv %s%sV = hex_pred[insn->regno[%d]];\n" % \
                (regtype, regid, regno))
        elif (regid in {"d", "e", "x"}):
            genptr_decl_writeble(f, tag, regtype, regid, regno)
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "C"):
        if (regid == "ss"):
            f.write("    TCGv_i64 %s%sV = tcg_temp_local_new_i64();\n" % \
                (regtype, regid))
            f.write("    const int %s = insn->regno[%d];\n" % \
                (regN, regno))
        elif (regid == "dd"):
            genptr_decl_pair_writeble(f, tag, regtype, regid, regno)
        elif (regid == "s"):
            f.write("    TCGv %s%sV = tcg_temp_local_new();\n" % \
                (regtype, regid))
            f.write("    const int %s%sN = insn->regno[%d];\n" % \
                (regtype, regid, regno))
        elif (regid == "d"):
            genptr_decl_writeble(f, tag, regtype, regid, regno)
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "M"):
        if (regid == "u"):
            f.write("    const int %s%sN = insn->regno[%d];\n"% \
                (regtype, regid, regno))
            f.write("    TCGv %s%sV = hex_gpr[%s%sN + HEX_REG_M0];\n" % \
                (regtype, regid, regtype, regid))
        else:
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

def genptr_decl_new(f,regtype,regid,regno):
    if (regtype == "N"):
        if (regid in {"s", "t"}):
            f.write("    const int %s%sX = insn->regno[%d];\n" % \
                (regtype, regid, regno))
            f.write("    TCGv %s%sN = tcg_const_tl(%s%sX);\n" % \
                (regtype, regid, regtype, regid))
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid in {"t", "u", "v"}):
            f.write("    TCGv %s%sN = hex_new_pred_value[insn->regno[%d]];\n" % \
                (regtype, regid, regno))
        else:
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

def genptr_decl_opn(f, tag, regtype, regid, toss, numregs, i):
    if (is_pair(regid)):
        genptr_decl(f, tag, regtype, regid, i)
    elif (is_single(regid)):
        if is_old_val(regtype, regid, tag):
            genptr_decl(f,tag, regtype, regid, i)
        elif is_new_val(regtype, regid, tag):
            genptr_decl_new(f,regtype,regid,i)
        else:
            print("Bad register parse: ",regtype,regid,toss,numregs)
    else:
        print("Bad register parse: ",regtype,regid,toss,numregs)

def genptr_decl_imm(f,immlett):
    if (immlett.isupper()):
        i = 1
    else:
        i = 0
    f.write("    int %s = insn->immed[%d];\n" % \
        (imm_name(immlett), i))

def genptr_free(f,regtype,regid,regno):
    if (regtype == "R"):
        if (regid in {"dd", "ss", "tt", "xx", "yy"}):
            f.write("    tcg_temp_free_i64(%s%sV);\n" % (regtype, regid))
        elif (regid in {"d", "e", "x", "y"}):
            f.write("    tcg_temp_free(%s%sV);\n" % (regtype, regid))
        elif (regid not in {"s", "t", "u", "v"}):
            print("Bad register parse: ",regtype,regid)
    elif (regtype == "P"):
        if (regid in {"d", "e", "x"}):
            f.write("    tcg_temp_free(%s%sV);\n" % (regtype, regid))
        elif (regid not in {"s", "t", "u", "v"}):
            print("Bad register parse: ",regtype,regid)
    elif (regtype == "C"):
        if (regid in {"dd", "ss"}):
            f.write("    tcg_temp_free_i64(%s%sV);\n" % (regtype, regid))
        elif (regid in {"d", "s"}):
            f.write("    tcg_temp_free(%s%sV);\n" % (regtype, regid))
        else:
            print("Bad register parse: ",regtype,regid)
    elif (regtype == "M"):
        if (regid != "u"):
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

def genptr_free_new(f,regtype,regid,regno):
    if (regtype == "N"):
        if (regid in {"s", "t"}):
            f.write("    tcg_temp_free(%s%sN);\n" % \
                (regtype, regid))
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid not in {"t", "u", "v"}):
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

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
    if (regtype == "R"):
        if (regid in {"ss", "tt", "xx", "yy"}):
            f.write("    tcg_gen_concat_i32_i64(%s%sV, hex_gpr[%s%sN],\n" % \
                (regtype, regid, regtype, regid))
            f.write("                                 hex_gpr[%s%sN + 1]);\n" % \
                (regtype, regid))
        elif (regid in {"x", "y"}):
            f.write("    tcg_gen_mov_tl(%s%sV, hex_gpr[%s%sN]);\n" % \
                (regtype,regid,regtype,regid))
        elif (regid not in {"s", "t", "u", "v"}):
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid == "x"):
            f.write("    tcg_gen_mov_tl(%s%sV, hex_pred[%s%sN]);\n" % \
                (regtype, regid, regtype, regid))
        elif (regid not in {"s", "t", "u", "v"}):
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "C"):
        if (regid == "ss"):
            f.write("    if (%s%sN + HEX_REG_SA0 == HEX_REG_P3_0) {\n" % \
                                 (regtype, regid))
            f.write("        TCGv p3_0 = tcg_temp_new();\n")
            f.write("        gen_read_p3_0(p3_0);\n")
            f.write("        tcg_gen_concat_i32_i64(%s%sV, p3_0,\n" % \
                                 (regtype, regid))
            f.write("                                     hex_gpr[%s%sN + 1]);\n" % \
                                 (regtype, regid))
            f.write("        tcg_temp_free(p3_0);\n")
            f.write("    } else {\n")
            f.write("        tcg_gen_concat_i32_i64(%s%sV, hex_gpr[%s%sN],\n" % \
                                 (regtype, regid, regtype, regid))
            f.write("                                     hex_gpr[%s%sN + 1]);\n" % \
                                 (regtype, regid))
            f.write("    }\n")
        elif (regid == "s"):
            f.write("    if (%s%sN + HEX_REG_SA0 == HEX_REG_P3_0) {\n" % \
                                 (regtype, regid))
            f.write("        gen_read_p3_0(%s%sV);\n" % \
                                 (regtype, regid))
            f.write("    } else {\n")
            f.write("        tcg_gen_mov_tl(%s%sV, hex_gpr[%s%sN + HEX_REG_SA0]);\n" % \
                                 (regtype, regid, regtype, regid))
            f.write("    }\n")
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "M"):
        if (regid != "u"):
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

def genptr_src_read_new(f,regtype,regid):
    if (regtype == "N"):
        if (regid not in {"s", "t"}):
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid not in {"t", "u", "v"}):
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

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
    f.write("    TCGv tcgv_%s = tcg_const_tl(%s);\n" % \
        (imm_name(immlett), imm_name(immlett)))

def gen_helper_call_imm(f,immlett):
    f.write(", tcgv_%s" % imm_name(immlett))

def gen_helper_free_imm(f,immlett):
    f.write("    tcg_temp_free(tcgv_%s);\n" % imm_name(immlett))

def genptr_dst_write_pair(f, tag, regtype, regid):
    if ('A_CONDEXEC' in attribdict[tag]):
        f.write("    gen_log_predicated_reg_write_pair(%s%sN, %s%sV, insn->slot);\n" % \
            (regtype, regid, regtype, regid))
    else:
        f.write("    gen_log_reg_write_pair(%s%sN, %s%sV);\n" % \
            (regtype, regid, regtype, regid))
    f.write("    ctx_log_reg_write(ctx, %s%sN);\n" % \
        (regtype, regid))
    f.write("    ctx_log_reg_write(ctx, %s%sN + 1);\n" % \
        (regtype, regid))

def genptr_dst_write(f, tag, regtype, regid):
    if (regtype == "R"):
        if (regid in {"dd", "xx", "yy"}):
            genptr_dst_write_pair(f, tag, regtype, regid)
        elif (regid in {"d", "e", "x", "y"}):
            if ('A_CONDEXEC' in attribdict[tag]):
                f.write("    gen_log_predicated_reg_write(%s%sN, %s%sV,\n" % \
                    (regtype, regid, regtype, regid))
                f.write("                                 insn->slot);\n")
            else:
                f.write("    gen_log_reg_write(%s%sN, %s%sV);\n" % \
                    (regtype, regid, regtype, regid))
            f.write("    ctx_log_reg_write(ctx, %s%sN);\n" % \
                (regtype, regid))
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "P"):
        if (regid in {"d", "e", "x"}):
            f.write("    gen_log_pred_write(%s%sN, %s%sV);\n" % \
                (regtype, regid, regtype, regid))
            f.write("    ctx_log_pred_write(ctx, %s%sN);\n" % \
                (regtype, regid))
        else:
            print("Bad register parse: ", regtype, regid)
    elif (regtype == "C"):
        if (regid == "dd"):
            f.write("    if (%s%sN + HEX_REG_SA0 == HEX_REG_P3_0) {\n" % \
                                 (regtype, regid))
            f.write("        TCGv val32 = tcg_temp_new();\n")
            f.write("        tcg_gen_extrl_i64_i32(val32, %s%sV);\n" % \
                                 (regtype, regid))
            f.write("        gen_write_p3_0(val32);\n")
            f.write("        tcg_gen_extrh_i64_i32(val32, %s%sV);\n" % \
                                 (regtype, regid))
            f.write("        gen_log_reg_write(%s%sN + HEX_REG_SA0 + 1, val32);\n" % \
                                 (regtype, regid))
            f.write("        tcg_temp_free(val32);\n")
            f.write("        ctx_log_reg_write(ctx, %s%sN + HEX_REG_SA0 + 1);\n" % \
                                 (regtype, regid))
            f.write("    } else {\n")
            f.write("        gen_log_reg_write_pair(%s%sN + HEX_REG_SA0, %s%sV);\n" % \
                                 (regtype, regid, regtype, regid))
            f.write("        ctx_log_reg_write(ctx, %s%sN + HEX_REG_SA0);\n" % \
                                 (regtype, regid))
            f.write("        ctx_log_reg_write(ctx, %s%sN + HEX_REG_SA0 + 1);\n" % \
                                 (regtype, regid))
            f.write("    }\n")
        elif (regid == "d"):
            f.write("    if (%s%sN + HEX_REG_SA0 == HEX_REG_P3_0) {\n" % \
                                 (regtype, regid))
            f.write("        gen_write_p3_0(%s%sV);\n" % \
                                 (regtype, regid))
            f.write("    } else {\n")
            f.write("        gen_log_reg_write(%s%sN + HEX_REG_SA0, %s%sV);\n" % \
                                 (regtype, regid, regtype, regid))
            f.write("        ctx_log_reg_write(ctx, %s%sN + HEX_REG_SA0);\n" % \
                                 (regtype, regid))
            f.write("    }\n")
        else:
            print("Bad register parse: ", regtype, regid)
    else:
        print("Bad register parse: ", regtype, regid)

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
    f.write("static void generate_%s(\n" %tag)
    f.write("                CPUHexagonState *env,\n")
    f.write("                DisasContext *ctx,\n")
    f.write("                Insn *insn,\n")
    f.write("                Packet *pkt)\n")
    f.write('{\n')
    if need_ea(tag): gen_decl_ea_tcg(f, tag)
    i=0
    ## Declare all the operands (regs and immediates)
    for regtype,regid,toss,numregs in regs:
        genptr_decl_opn(f, tag, regtype, regid, toss, numregs, i)
        i += 1
    for immlett,bits,immshift in imms:
        genptr_decl_imm(f,immlett)

    if 'A_PRIV' in attribdict[tag]:
        f.write('    fCHECKFORPRIV();\n')
    if 'A_GUEST' in attribdict[tag]:
        f.write('    fCHECKFORGUEST();\n')

    ## Read all the inputs
    for regtype,regid,toss,numregs in regs:
        if (is_read(regid)):
            genptr_src_read_opn(f,regtype,regid,tag)

    if is_semantics_enabled(tag):
        declared = []
        ## Handle registers
        for regtype,regid,toss,numregs in regs:
            if is_pair(regid) or (is_single(regid) and is_old_val(regtype, regid, tag)):
                declared.append("%s%sV" % (regtype, regid))
                if regtype == "M":
                    declared.append("%s%sN" % (regtype, regid))
            elif is_new_val(regtype, regid, tag):
                if regtype == "N":
                    declared.append("%s%sX" % (regtype,regid))
                else:
                    declared.append("%s%sN" % (regtype,regid))
            else:
                print("Bad register parse: ",regtype,regid,toss,numregs)

        ## Handle immediates
        for immlett,bits,immshift in imms:
            declared.append(imm_name(immlett))

        f.write("    emit_%s(%s);\n" % (tag, ", ".join(["ctx", "insn", "pkt"] + declared)))

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
    read_semantics_enabled_file(sys.argv[4])
    calculate_attribs()
    tagregs = get_tagregs()
    tagimms = get_tagimms()

    f = StringIO()

    f.write("#ifndef HEXAGON_TCG_FUNCS_H\n")
    f.write("#define HEXAGON_TCG_FUNCS_H\n\n")

    f.write("#include \"idef-generated-emitter.h\"\n\n")

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

    f.write("#endif    /* HEXAGON_TCG_FUNCS_H */\n")

    realf = open(sys.argv[5], 'w')
    realf.write(f.getvalue())
    realf.close()
    f.close()

if __name__ == "__main__":
    main()
