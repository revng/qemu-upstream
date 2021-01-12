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

import hex_common

def main():
    hex_common.read_semantics_file(sys.argv[1])
    hex_common.read_attribs_file(sys.argv[2])
    hex_common.read_overrides_file(sys.argv[3])
    hex_common.calculate_attribs()
    tagregs = hex_common.get_tagregs()
    tagimms = hex_common.get_tagimms()

    f = StringIO()

    f.write('#include "macros.h"\n\n')

    for tag in hex_common.tags:
        ## Skip the priv instructions
        if ( "A_PRIV" in hex_common.attribdict[tag] ) :
            continue
        ## Skip the guest instructions
        if ( "A_GUEST" in hex_common.attribdict[tag] ) :
            continue
        ## Skip the diag instructions
        if ( tag == "Y6_diag" ) :
            continue
        if ( tag == "Y6_diag0" ) :
            continue
        if ( tag == "Y6_diag1" ) :
            continue

        regs = tagregs[tag]
        imms = tagimms[tag]

        arguments = []
        if hex_common.need_ea(tag):
            arguments.append("ea")

        for regtype,regid,toss,numregs in regs:
            prefix = "in_" if hex_common.is_read(regid) else ""

            is_pair = hex_common.is_pair(regid)
            is_single_old = (hex_common.is_single(regid)
                             and hex_common.is_old_val(regtype, regid, tag))
            is_single_new = (hex_common.is_single(regid)
                             and hex_common.is_new_val(regtype, regid, tag))

            if is_pair or is_single_old:
                arguments.append("%s%sREG_%s" % (prefix, regtype, regid))
            elif is_single_new:
                arguments.append("%sNEW_%sREG_%s" % (prefix, regtype, regid))
            else:
                print("Bad register parse: ",regtype,regid,toss,numregs)

        for immlett,bits,immshift in imms:
            arguments.append("IMM_" + hex_common.imm_name(immlett))

        f.write("%s(%s) {\n" % (tag, ", ".join(arguments)))
        f.write("    %s\n" % hex_common.semdict[tag])
        f.write("}\n\n")

    realf = open(sys.argv[4], 'w')
    realf.write(f.getvalue())
    realf.close()
    f.close()

if __name__ == "__main__":
    main()
