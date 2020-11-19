%{
/*
 * Hexagon emulation for qemu: semantics parser.
 *
 * Copyright (c) 2017 Alessandro Di Federico, rev.ng Srls Unipersonale
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; withOUT even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include "csvparser.h"
#include "semantics_struct.h"
#include "semantics_helpers.h"
#include "semantics.tab.h"
#include "lex.yy.h"

// Decomment this to disable yyasserts
//#define NDEBUG

#define ERR_LINE_CONTEXT 40

// Variadic macros to wrap the buffer printing functions
#define EMIT(...) c->out_c += snprintf(c->out_buffer+c->out_c, \
                                       OUT_BUF_LEN-c->out_c,   \
                                       __VA_ARGS__);
#define EMIT_SIG(...) c->signature_c += snprintf(c->signature_buffer + \
                                                 c->signature_c,       \
                                                 SIGNATURE_BUF_LEN -   \
                                                 c->signature_c,       \
                                                 __VA_ARGS__);
%}

%lex-param {void *scanner}
%parse-param {void *scanner}
%parse-param {context_t *c}

%define parse.error detailed
%define parse.lac full
%define api.pure full

%locations

%union {
    t_hex_value rvalue;
    t_hex_sat sat;
    t_hex_cast cast;
    t_hex_range range;
    t_hex_mpy mpy;
    bool is_unsigned;
    int index;
}

/* Tokens */
%start code

%expect 1

%token DREG DIMM DPRE DEA RREG WREG FREG FIMM RPRE WPRE FPRE FWRAP FEA PART1
%token VAR LBR RBR LPAR RPAR LSQ RSQ SEMI COLON PLUS MINUS MUL POW DIV MOD ABS
%token CROUND ROUND CIRCADD COUNTONES AND OR XOR NOT ASSIGN INC DEC ANDA ORA
%token XORA PLUSPLUS LT GT ASL ASR LSR EQ NEQ LTE GTE MIN MAX ANDL ORL NOTL
%token COMMA FOR ICIRC IF MUN FSCR FCHK SXT ZXT NEW CONSTEXT LOCNT BREV SIGN
%token LOAD STORE CONSTLL CONSTULL PC NPC LPCFG CANC QMARK IDENTITY

%token <rvalue> REG IMM PRE
%token <index> ELSE
%token <mpy> MPY
%token <sat> SAT
%token <cast> CAST EXTRACT DEPOSIT
%token <range> SETBITS INSBITS INSRANGE EXTBITS EXTRANGE
%type <rvalue> rvalue lvalue VAR assign_statement pre
%type <rvalue> DREG DIMM DPRE RREG RPRE
%type <index> if_stmt IF
%type <is_unsigned> SIGN

/* Operator Precedences */
%left MIN MAX
%left LPAR
%left COMMA
%left ASSIGN
%right CIRCADD
%right INC DEC ANDA ORA XORA
%left QMARK COLON
%left ORL
%left ANDL
%left OR
%left XOR ANDOR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left ASL ASR LSR
%right ABS CROUND
%left MINUS PLUS
%left POW
%left MUL DIV MOD MPY
%right NOT NOTL
%left LSQ
%left NEW
%right CAST
%right LOCNT BREV

/* Bison Grammar */
%%

/* Return the modified registers list */
code  : LBR
      {
        EMIT_SIG("void emit_%s(DisasContext *ctx, "
                 "insn_t *insn, packet_t *pkt",
                 c->inst_name);
      }
      decls
      {
        EMIT_SIG(")");
        OUT(c, &@1, "\n{");

        /* Initialize declared but uninitialized registers,
           but only for non-conditional instructions */
        for (int i = 0; i < c->init_count; i++) {
            bool is64 = c->init_list[i].bit_width == 64;
            const char *type = is64 ? "i64" : "i32";
            if (c->init_list[i].type == REGISTER) {
              OUT(c, &@1, "tcg_gen_movi_", type, "(", &(c->init_list[i]), ", 0);\n");
            } else if (c->init_list[i].type == PREDICATE) {
              char suffix = c->init_list[i].is_dotnew ? 'N' : 'V';
              OUT(c, &@1, "tcg_gen_movi_", type, "(", &(c->init_list[i]), ", 0);\n");
            }
        }
      }
      FWRAP statements RPAR SEMI decls RBR
      {
         YYACCEPT;
      }
;

decls : decls decl
      | %empty
;

decl  : DREG
      {
          if ($1.reg.type == DOTNEW) {
            EMIT_SIG(", int N%cX", $1.reg.id);
          } else {
            bool is64 = ($1.bit_width == 64);
            const char *type = is64 ? "TCGv_i64" : "TCGv_i32";
            char reg_id[5] = { 0 };
            reg_compose(c, &@1, &($1.reg), reg_id);
            EMIT_SIG(", %s %s", type, reg_id);
            /* MuV register requires also MuN to provide its index */
            if ($1.reg.type == MODIFIER)
              EMIT_SIG(", int MuN", type);
          }
          /* Enqueue register into initialization list */
          c->init_list[c->init_count] = $1;
          c->init_count++;
      }
      | DIMM
      {
        EMIT_SIG(", int %ciV", $1.imm.id);
      }
      | DPRE
      {
        char suffix = $1.is_dotnew ? 'N' : 'V';
        EMIT_SIG(", TCGv P%c%c", $1.pre.id, suffix);
        /* Enqueue predicate into initialization list */
        c->init_list[c->init_count] = $1;
        c->init_count++;
      }
      | DEA
      | RREG
      {
        /* Remove register from initialization list */
        int iter_count = c->init_count;
        for (int i = 0; i < iter_count; i++) {
            if (rvalue_equal(&($1), &(c->init_list[i]))) {
                c->init_list[i] = c->init_list[c->init_count-1];
                c->init_count--;
            }
        }
      }
      | WREG
      | FREG
      | FIMM
      | RPRE
      {
        /* Remove predicate from initialization list */
        int iter_count = c->init_count;
        for (int i = 0; i < iter_count; i++) {
            if (rvalue_equal(&($1), &(c->init_list[i]))) {
                c->init_list[i] = c->init_list[c->init_count-1];
                c->init_count--;
            }
        }
      }
      | WPRE
      | FPRE
      | FEA
;

code_block : LBR statements RBR            { /* does nothing */ }
           | LBR RBR                       { /* does nothing */ }
;

/* A list of one or more statements */
statements  : statements statement         { /* does nothing */ }
            | statement                    { /* does nothing */ }
;

/* Statements can be assignment (rvalue SEMI), control or memory statements */
statement   : control_statement            { /* does nothing */ }
            | rvalue SEMI                  { rvalue_free(c, &@1, &$1); }
            | code_block                   { /* does nothing */ }
            | SEMI                         { /* does nothing */ }
;

assign_statement  : lvalue ASSIGN rvalue
                  {
                    @1.last_column = @3.last_column;
                    gen_assign(c, &@1, &$1, &$3);
                    $$ = $1;
                  }
                  | lvalue INC rvalue
                  {
                    @1.last_column = @3.last_column;
                    t_hex_value tmp = gen_bin_op(c, &@1, ADD_OP, &$1, &$3);
                    gen_assign(c, &@1, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue DEC rvalue
                  {
                    @1.last_column = @3.last_column;
                    t_hex_value tmp = gen_bin_op(c, &@1, SUB_OP, &$1, &$3);
                    gen_assign(c, &@1, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue ANDA rvalue
                  {
                    @1.last_column = @3.last_column;
                    t_hex_value tmp = gen_bin_op(c, &@1, ANDB_OP, &$1, &$3);
                    gen_assign(c, &@1, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue ORA rvalue
                  {
                    @1.last_column = @3.last_column;
                    t_hex_value tmp = gen_bin_op(c, &@1, ORB_OP, &$1, &$3);
                    gen_assign(c, &@1, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue XORA rvalue
                  {
                    @1.last_column = @3.last_column;
                    t_hex_value tmp = gen_bin_op(c, &@1, XORB_OP, &$1, &$3);
                    gen_assign(c, &@1, &$1, &tmp);
                    $$ = $1;
                  }
                  | pre ASSIGN rvalue
                  {
                    @1.last_column = @3.last_column;
                    bool is_direct = is_direct_predicate(&$1);
                    char pre_id[2] = " ";
                    pre_id[0] = $1.pre.id;
                    /* Extract predicate TCGv */
                    if (is_direct) {
                        $1 = gen_tmp_value(c, &@1, "0", 32);
                    }
                    rvalue_materialize(c, &@1, &$3);
                    rvalue_truncate(c, &@1, &$3);
                    /* Extract first 8 bits, and store new predicate value */
                    if ($3.type == IMMEDIATE) {
                        OUT(c, &@1, &$3, " = (", &$3, " & 0xff) << i;\n");
                        OUT(c, &@1, "tcg_gen_ori_i32(", &$1, ", ", &$1, ", ", &$3, ");\n");
                    } else {
                        OUT(c, &@1, "tcg_gen_or_i32(", &$1, ", ", &$1, ", ", &$3, ");\n");
                        OUT(c, &@1, "tcg_gen_andi_i32(", &$1, ", ", &$1, ", 0xff);\n");
                    }
                    if (is_direct) {
                        OUT(c, &@1, "LOG_PRED_WRITE(", pre_id, ", ", &$1, ");\n");
                        rvalue_free(c, &@1, &$1);
                    }
                    rvalue_free(c, &@1, &$3);  /* Free temporary value */
                  }
                  | IMM ASSIGN rvalue
                  {
                    @1.last_column = @3.last_column;
                    yyassert(c, &@1, $3.type == IMMEDIATE,
                           "Cannot assign non-immediate to immediate!");
                    yyassert(c, &@1, $1.imm.type == VARIABLE,
                           "Cannot assign to non-variable!");
                    /* Assign to the function argument */
                    OUT(c, &@1, &$1, " = ", &$3, ";\n");
                    $$ = $1;
                  }
                  | PC ASSIGN rvalue
                  {
                    @1.last_column = @3.last_column;
                    rvalue_truncate(c, &@1, &$3);
                    rvalue_materialize(c, &@1, &$3);
                    OUT(c, &@1, "gen_write_new_pc(", &$3, ");\n");
                    rvalue_free(c, &@1, &$3); /* Free temporary value */
                  }
                  | LOAD LPAR IMM COMMA IMM COMMA SIGN COMMA VAR COMMA lvalue RPAR
                  {
                    @1.last_column = @12.last_column;
                    /* Memop width is specified in the load macro */
                    int bit_width = ($5.imm.value > 4) ? 64 : 32;
                    char *sign_suffix = ($5.imm.value > 4) ? "" : (($7) ? "u" : "s");
                    char *helper_suffix = ($7) ? "u" : "s";
                    char size_suffix[4] = { 0 };
                    /* Create temporary variable (if not present) */
                    if ($11.type == VARID)
                        varid_allocate(c, &@1, &$11, bit_width, $7);
                    snprintf(size_suffix, 4, "%" PRIu64, $5.imm.value * 8);
                    if (bit_width == 32) {
                        rvalue_truncate(c, &@1, &$11);
                    } else {
                        rvalue_extend(c, &@1, &$11);
                    }
                    OUT(c, &@1, "tcg_gen_qemu_ld", size_suffix, sign_suffix);
                    OUT(c, &@1, "(", &$11, ", ", &$9, ", 0);\n");
                    OUT(c, &@1, "if (insn->slot == 0 && pkt->pkt_has_store_s1) {\n");
                    OUT(c, &@1, "gen_helper_merge_inflight_store", &$5.imm.value);
                    OUT(c, &@1, helper_suffix, "(", &$11, ", cpu_env, ", &$9, ", ", &$11, ");\n");
                    OUT(c, &@1, "}\n");
                  }
                  | STORE LPAR IMM COMMA IMM COMMA VAR COMMA rvalue RPAR /* Store primitive */
                  {
                    @1.last_column = @10.last_column;
                    /* Memop width is specified in the store macro */
                    int mem_width = $5.imm.value;
                    /* Adjust operand bit width to memop bit width */
                    if (mem_width < 8) {
                        rvalue_truncate(c, &@1, &$9);
                    } else {
                        rvalue_extend(c, &@1, &$9);
                    }
                    rvalue_materialize(c, &@1, &$9);
                    OUT(c, &@1, "gen_store", &mem_width, "(cpu_env, ", &$7, ", ", &$9);
                    OUT(c, &@1, ", ctx, insn->slot);\n");
                    rvalue_free(c, &@1, &$9);
                  }
                  | LPCFG ASSIGN rvalue
                  {
                    @1.last_column = @3.last_column;
                    rvalue_truncate(c, &@1, &$3);
                    rvalue_materialize(c, &@1, &$3);
                    OUT(c, &@1, "SET_USR_FIELD(USR_LPCFG, ", &$3, ");\n");
                    rvalue_free(c, &@1, &$3);
                  }
                  | DEPOSIT LPAR rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    gen_deposit_op(c, &@1, &$5, &$7, &$3, &$1);
                  }
                  | SETBITS LPAR rvalue COMMA rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @10.last_column;
                    yyassert(c, &@1, $3.type == IMMEDIATE &&
                                     $3.imm.type == VALUE &&
                                     $5.type == IMMEDIATE &&
                                     $5.imm.type == VALUE,
                             "Range deposit needs immediate values!\n");
                    $1.begin = $5.imm.value;
                    $1.end = $3.imm.value;
                    gen_rdeposit_op(c, &@1, &$7, &$9, &$1);
                  }
                  | INSBITS LPAR rvalue COMMA rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @10.last_column;
                    yyassert(c, &@1, $5.type == IMMEDIATE &&
                                     $5.imm.type == VALUE &&
                                     $7.type == IMMEDIATE &&
                                     $7.imm.type == VALUE,
                             "Range deposit needs immediate values!\n");
                    $1.begin = $7.imm.value;
                    $1.end = $5.imm.value - 1 + $7.imm.value;
                    gen_rdeposit_op(c, &@1, &$3, &$9, &$1);
                  }
                  | INSRANGE LPAR rvalue COMMA rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @10.last_column;
                    yyassert(c, &@1, $5.type == IMMEDIATE &&
                                     $5.imm.type == VALUE &&
                                     $7.type == IMMEDIATE &&
                                     $7.imm.type == VALUE,
                             "Range deposit needs immediate values!\n");
                    $1.begin = $7.imm.value;
                    $1.end = $5.imm.value;
                    gen_rdeposit_op(c, &@1, &$3, &$9, &$1);
                  }
                  | IDENTITY LPAR rvalue RPAR
                  {
                    @1.last_column = @4.last_column;
                    $$ = $3;
                  }
;

control_statement : frame_check          { /* does nothing */ }
                  | cancel_statement     { /* does nothing */ }
                  | if_statement         { /* does nothing */ }
                  | for_statement        { /* does nothing */ }
                  | fpart1_statement     { /* does nothing */ }
;

frame_check       : FCHK LPAR rvalue RPAR SEMI  { /* does nothing */ }
;

cancel_statement : CANC
                 {
                   t_hex_value slot = gen_tmp_value(c, &@1, "insn->slot", 32);
                   OUT(c, &@1, "gen_cancel(", &slot, ");\n");
                 }
;

if_statement : if_stmt
             {
               /* Fix else label */
               OUT(c, &@1, "gen_set_label(if_label_", &$1, ");\n");
             }
             | if_stmt ELSE
             {
               @1.last_column = @2.last_column;
               /* Generate label to jump if else is not verified */
               OUT(c, &@1, "TCGLabel *if_label_", &c->if_count, " = gen_new_label();\n");
               $2 = c->if_count;
               c->if_count++;
               /* Jump out of the else statement */
               OUT(c, &@1, "tcg_gen_br(if_label_", &$2, ");\n");
               /* Fix the else label */
               OUT(c, &@1, "gen_set_label(if_label_", &$1, ");\n");
             }
             statement
             {
               OUT(c, &@1, "gen_set_label(if_label_", &$2, ");\n");
             }
;

for_statement : FOR LPAR IMM ASSIGN IMM SEMI IMM LT IMM SEMI IMM PLUSPLUS RPAR
              {
                @1.last_column = @13.last_column;
                OUT(c, &@1, "for(int ", &$3, " = ", &$5, "; ", &$7, " < ", &$9);
                OUT(c, &@1, "; ", &$11, "++) {\n");
              }
              code_block
              {
                OUT(c, &@1, "}\n");
              }
;

for_statement : FOR LPAR IMM ASSIGN IMM SEMI IMM LT IMM SEMI IMM INC IMM RPAR
              {
                @1.last_column = @14.last_column;
                OUT(c, &@1, "for(int ", &$3, " = ", &$5, "; ", &$7, " < ", &$9);
                OUT(c, &@1, "; ", &$11, " += ", &$13, ") {\n");
              }
              code_block
              {
                OUT(c, &@1, "}\n");
              }
;

fpart1_statement : PART1
                 {
                    OUT(c, &@1, "if (insn->part1) {\n");
                 }
                 LPAR statements RPAR
                 {
                    @1.last_column = @3.last_column;
                    OUT(c, &@1, "return; }\n");
                 }
;

if_stmt      : IF
             {
               /* Generate an end label, if false branch to that label */
               OUT(c, &@1, "TCGLabel *if_label_", &c->if_count, " = gen_new_label();\n");
             }
             LPAR rvalue RPAR
             {
               @1.last_column = @3.last_column;
               rvalue_materialize(c, &@1, &$4);
               char *bit_suffix = ($4.bit_width == 64) ? "i64" : "i32";
               OUT(c, &@1, "tcg_gen_brcondi_", bit_suffix, "(TCG_COND_EQ, ", &$4,
                   ", 0, if_label_", &c->if_count, ");\n");
               rvalue_free(c, &@1, &$4);
               $1 = c->if_count;
               c->if_count++;
             }
             statement
             {
               $$ = $1;
             }
;

rvalue            : assign_statement            { /* does nothing */ }
                  | REG
                  {
                    $$ = gen_read_creg(c, &@1, &$1);
                  }
                  | IMM
                  {
                    $$ = $1;
                  }
                  | CONSTLL LPAR IMM RPAR
                  {
                    $3.is_unsigned = false;
                    $3.bit_width = 64;
                    $$ = $3;
                  }
                  | CONSTULL LPAR IMM RPAR
                  {
                    $3.is_unsigned = true;
                    $3.bit_width = 64;
                    $$ = $3;
                  }
                  | pre
                  {
                    if(is_direct_predicate(&$1)) {
                        bool is_dotnew = $1.is_dotnew;
                        char predicate_id[2] = {$1.pre.id, '\0'};
                        char *pre_str = (char *) &predicate_id;
                        $1 = gen_tmp_value(c, &@1, "0", 32);
                        if (is_dotnew) {
                            OUT(c, &@1, "tcg_gen_mov_i32(", &$1, ", hex_new_pred_value[");
                            OUT(c, &@1, pre_str, "]);\n");
                        } else {
                            OUT(c, &@1, "gen_read_preg(", &$1, ", ", pre_str, ");\n");
                        }
                    }
                    $$ = $1;
                  }
                  | PC
                  {
                    /* Read PC from the CR */
                    $$ = gen_tmp(c, &@1, 32);
                    OUT(c, &@1, "tcg_gen_mov_i32(", &$$, ", hex_gpr[HEX_REG_PC]);\n");
                  }
                  | NPC
                  {
                    yyassert(c, &@1, 1 == 0,
                             "Reading npc from disascontext is still not supported\n");
                    /* Extract program counter into a temporary */
                    $$ = gen_tmp(c, &@1, 32);
                    t_hex_value pc = gen_tmp_value(c, &@1, "dc->npc", 32);
                    OUT(c, &@1, "tcg_gen_mov_i32(", &$$, ", ", &pc, ");\n");
                  }
                  | CONSTEXT
                  {
                    t_hex_value rvalue;
                    rvalue.type = IMMEDIATE;
                    rvalue.imm.type = IMM_CONSTEXT;
                    rvalue.is_unsigned = true;
                    rvalue.is_dotnew = false;
                    rvalue.is_symbol = false;
                    $$ = rvalue;
                  }
                  | VAR
                  {
                    /* Assign correct bit width and signedness */
                    bool found = false;
                    for(int i = 0; i < c->allocated_count; i++) {
                        if(!strcmp($1.var.name, c->allocated[i].name)) {
                            found = true;
                            free(c->allocated[i].name);
                            c->allocated[i].name = $1.var.name;
                            $1.bit_width = c->allocated[i].bit_width;
                            $1.is_unsigned = c->allocated[i].is_unsigned;
                            break;
                        }
                    }
                    yyassert(c, &@1, found, "Undefined symbol!\n");
                    $$ = $1;
                  }
                  | MPY LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @6.last_column;
                    $3.is_unsigned = $1.first_unsigned;
                    $5.is_unsigned = $1.second_unsigned;
                    $3 = gen_cast_op(c, &@1, &$3, $1.first_bit_width * 2);
                    /* Handle fMPTY3216.. */
                    if ($1.first_bit_width == 32)
                        $5 = gen_cast_op(c, &@1, &$5, 64);
                    else
                        $5 = gen_cast_op(c, &@1, &$5, $1.second_bit_width * 2);
                    $$ = gen_bin_op(c, &@1, MUL_OP, &$3, &$5);
                    /* Handle special cases required by the language */
                    if ($1.first_bit_width == 16 && $1.second_bit_width == 16) {
                        t_hex_value src_width = gen_imm_value(c, &@1, 32, 32);
                        t_hex_value dst_width = gen_imm_value(c, &@1, 64, 32);
                        $$ = gen_extend_op(c, &@1, &src_width, &dst_width, &$$,
                                           $1.first_unsigned && $1.second_unsigned);
                    }
                  }
                  | rvalue PLUS rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ADD_OP, &$1, &$3);
                  }
                  | rvalue MINUS rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, SUB_OP, &$1, &$3);
                  }
                  | rvalue MUL rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, MUL_OP, &$1, &$3);
                  }
                  | rvalue POW rvalue
                  {
                    @1.last_column = @3.last_column;
                    /* We assume that this is a shorthand for a shift */
                    yyassert(c, &@1, $1.type == IMMEDIATE && $1.imm.value == 2,
                           "Exponentiation is not a left shift!\n");
                    t_hex_value one = gen_imm_value(c, &@1, 1, 32);
                    t_hex_value shift = gen_bin_op(c, &@1, SUB_OP, &$3, &one);
                    $$ = gen_bin_op(c, &@1, ASL_OP, &$1, &shift);
                    rvalue_free(c, &@1, &one);
                    rvalue_free(c, &@1, &shift);
                  }
                  | rvalue DIV rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, DIV_OP, &$1, &$3);
                  }
                  | rvalue MOD rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, MOD_OP, &$1, &$3);
                  }
                  | rvalue ASL rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ASL_OP, &$1, &$3);
                  }
                  | rvalue ASR rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ASR_OP, &$1, &$3);
                  }
                  | rvalue LSR rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, LSR_OP, &$1, &$3);
                  }
                  | rvalue AND rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ANDB_OP, &$1, &$3);
                  }
                  | rvalue OR rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ORB_OP, &$1, &$3);
                  }
                  | rvalue XOR rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, XORB_OP, &$1, &$3);
                  }
                  | rvalue ANDL rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, ANDL_OP, &$1, &$3);
                  }
                  | MIN LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, MINI_OP, &$3, &$5);
                  }
                  | MAX LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_op(c, &@1, MAXI_OP, &$3, &$5);
                  }
                  | NOT rvalue
                  {
                    @1.last_column = @2.last_column;
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, &@1, "int", &bit_width, "_t ", &res, " = ~", &$2, ";\n");
                        c->qemu_tmp_count++;
                    } else {
                        res = gen_tmp(c, &@1, bit_width);
                        OUT(c, &@1, "tcg_gen_not_", bit_suffix, "(", &res,
                            ", ", &$2, ");\n");
                        rvalue_free(c, &@1, &$2);
                    }
                    $$ = res;
                  }
                  | NOTL rvalue
                  {
                    @1.last_column = @2.last_column;
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, &@1, "int", &bit_width, "_t ", &res, " = !", &$2, ";\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, &@1, bit_width);
                        t_hex_value zero = gen_tmp_value(c, &@1, "0", bit_width);
                        t_hex_value one = gen_tmp_value(c, &@1, "0xff", bit_width);
                        OUT(c, &@1, "tcg_gen_movcond_", bit_suffix);
                        OUT(c, &@1, "(TCG_COND_EQ, ", &res, ", ", &$2, ", ", &zero);
                        OUT(c, &@1, ", ", &one, ", ", &zero, ");\n");
                        rvalue_free(c, &@1, &$2);
                        rvalue_free(c, &@1, &zero);
                        rvalue_free(c, &@1, &one);
                        $$ = res;
                    }
                  }
                  | SAT LPAR IMM COMMA rvalue RPAR
                  {
                    @1.last_column = @6.last_column;
                    yyassert(c, &@1, $3.imm.value < $5.bit_width, "To compute overflow, "
                             "source width must be greater than saturation width!");
                    t_hex_value res = gen_tmp(c, &@1, $5.bit_width);
                    const char *bit_suffix = ($5.bit_width == 64) ? "i64" : "i32";
                    const char *overflow_str = ($1.set_overflow) ? "true" : "false";
                    const char *unsigned_str = ($1.is_unsigned) ? "u" : "";
                    OUT(c, &@1, "gen_sat", unsigned_str, "_", bit_suffix, "(", &res, ", ");
                    OUT(c, &@1, &$5, ", ", &$3.imm.value, ", ", overflow_str, ");\n");
                    $$ = res;
                  }
                  | CAST rvalue
                  {
                    @1.last_column = @2.last_column;
                    /* Assign target signedness */
                    $2.is_unsigned = $1.is_unsigned;
                    $$ = gen_cast_op(c, &@1, &$2, $1.bit_width);
                  }
                  | rvalue LSQ rvalue RSQ
                  {
                    @1.last_column = @4.last_column;
                    t_hex_value one = gen_imm_value(c, &@1, 1, $3.bit_width);
                    t_hex_value tmp = gen_bin_op(c, &@1, ASR_OP, &$1, &$3);
                    $$ = gen_bin_op(c, &@1, ANDB_OP, &tmp, &one);
                  }
                  | rvalue EQ rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_cmp(c, &@1, "TCG_COND_EQ", &$1, &$3);
                  }
                  | rvalue NEQ rvalue
                  {
                    @1.last_column = @3.last_column;
                    $$ = gen_bin_cmp(c, &@1, "TCG_COND_NE", &$1, &$3);
                  }
                  | rvalue LT rvalue
                  {
                    @1.last_column = @3.last_column;
                    if ($1.is_unsigned || $3.is_unsigned)
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_LTU", &$1, &$3);
                    else
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_LT", &$1, &$3);
                  }
                  | rvalue GT rvalue
                  {
                    @1.last_column = @3.last_column;
                    if ($1.is_unsigned || $3.is_unsigned)
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_GTU", &$1, &$3);
                    else
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_GT", &$1, &$3);
                  }
                  | rvalue LTE rvalue
                  {
                    @1.last_column = @3.last_column;
                    if ($1.is_unsigned || $3.is_unsigned)
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_LEU", &$1, &$3);
                    else
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_LE", &$1, &$3);
                  }
                  | rvalue GTE rvalue
                  {
                    @1.last_column = @3.last_column;
                    if ($1.is_unsigned || $3.is_unsigned)
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_GEU", &$1, &$3);
                    else
                        $$ = gen_bin_cmp(c, &@1, "TCG_COND_GE", &$1, &$3);
                  }
                  | rvalue QMARK rvalue COLON rvalue
                  {
                    @1.last_column = @5.last_column;
                    bool is_64bit = ($3.bit_width == 64) || ($5.bit_width == 64);
                    int bit_width = (is_64bit) ? 64 : 32;
                    if (is_64bit) {
                        rvalue_extend(c, &@1, &$1);
                        rvalue_extend(c, &@1, &$3);
                        rvalue_extend(c, &@1, &$5);
                    } else {
                        rvalue_truncate(c, &@1, &$1);
                    }
                    rvalue_materialize(c, &@1, &$1);
                    rvalue_materialize(c, &@1, &$3);
                    rvalue_materialize(c, &@1, &$5);
                    t_hex_value res = gen_local_tmp(c, &@1, bit_width);
                    t_hex_value zero = gen_tmp_value(c, &@1, "0", bit_width);
                    OUT(c, &@1, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, &@1, "(TCG_COND_NE, ", &res, ", ", &$1, ", ", &zero);
                    OUT(c, &@1, ", ", &$3, ", ", &$5, ");\n");
                    rvalue_free(c, &@1, &zero);
                    rvalue_free(c, &@1, &$1);
                    rvalue_free(c, &@1, &$3);
                    rvalue_free(c, &@1, &$5);
                    $$ = res;
                  }
                  | FSCR LPAR rvalue RPAR
                  {
                    @1.last_column = @4.last_column;
                    t_hex_value key = gen_tmp(c, &@1, 64);
                    t_hex_value res = gen_tmp(c, &@1, 64);
                    rvalue_extend(c, &@1, &$3);
                    t_hex_value frame_key = gen_tmp(c, &@1, 32);
                    OUT(c, &@1, "READ_REG(", &frame_key, ", HEX_REG_FRAMEKEY);\n");
                    OUT(c, &@1, "tcg_gen_concat_i32_i64(", &key,", ", &frame_key, ", ", &frame_key, ");\n");
                    OUT(c, &@1, "tcg_gen_xor_i64(", &res, ", ", &$3,", ", &key, ");\n");
                    $$ = res;
                  }
                  | SXT LPAR IMM COMMA IMM COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    $$ = gen_extend_op(c, &@1, &$3, &$5, &$7, false);
                  }
                  | ZXT LPAR IMM COMMA IMM COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    $$ = gen_extend_op(c, &@1, &$3, &$5, &$7, true);
                  }
                  | LPAR rvalue RPAR
                  {
                    $$ = $2;
                  }
                  | ABS rvalue
                  {
                    @1.last_column = @2.last_column;
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, &@1, "int", &bit_width, "_t ", &res, " = abs(", &$2, ");\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, &@1, bit_width);
                        t_hex_value zero = gen_tmp_value(c, &@1, "0", bit_width);
                        OUT(c, &@1, "tcg_gen_neg_", bit_suffix, "(", &res, ", ",
                            &$2, ");\n");
                        OUT(c, &@1, "tcg_gen_movcond_i", &bit_width);
                        OUT(c, &@1, "(TCG_COND_GT, ", &res, ", ", &$2, ", ", &zero);
                        OUT(c, &@1, ", ", &$2, ", ", &res, ");\n");
                        rvalue_free(c, &@1, &$2);
                        $$ = res;
                    }
                  }
                  | CROUND LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @6.last_column;
                    $$ = gen_convround(c, &@1, &$3, &$5);
                  }
                  | CROUND LPAR rvalue RPAR
                  {
                    @1.last_column = @4.last_column;
                    /* When is not specified assume mask = 1 */
                    t_hex_value one = gen_imm_value(c, &@1, 1, 32);
                    $$ = gen_convround(c, &@1, &$3, &one);
                  }
                  | ROUND LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @6.last_column;
                    /* Add .5 only if .5 bit is set */
                    yyassert(c, &@1, $3.bit_width <= 32,
                           "Convround not implemented for bit widths > 32!");
                    t_hex_value one = gen_imm_value(c, &@1, 1, 32);
                    t_hex_value remainder = gen_bin_op(c, &@1, ANDB_OP, &$3, &$5);
                    t_hex_value res = gen_bin_op(c, &@1, ADD_OP, &$3, &remainder);
                    /* Zero out trailing bits */
                    t_hex_value mask = gen_bin_op(c, &@1, ASL_OP, &$5, &one);
                    mask = gen_bin_op(c, &@1, SUB_OP, &mask, &one);
                    rvalue_materialize(c, &@1, &mask);
                    OUT(c, &@1, "tcg_gen_not_i32(", &mask, ", ", &mask, ");\n");
                    res = gen_bin_op(c, &@1, ANDB_OP, &res, &mask);
                    rvalue_free(c, &@1, &$3);
                    rvalue_free(c, &@1, &$5);
                    $$ = res;
                  }
                  | MINUS rvalue
                  {
                    @1.last_column = @2.last_column;
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, &@1, "int", &bit_width, "_t ", &res, " = -", &$2, ";\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, &@1, bit_width);
                        OUT(c, &@1, "tcg_gen_neg_", bit_suffix, "(", &res, ", ",
                            &$2, ");\n");
                        rvalue_free(c, &@1, &$2);
                        $$ = res;
                    }
                  }
                  | ICIRC LPAR rvalue RPAR ASL IMM
                  {
                    @1.last_column = @6.last_column;
                    $$ = gen_tmp(c, &@1, 32);
                    OUT(c, &@1, "gen_read_ireg(", &$$, ", ", &$3, ", ", &$6, ");\n");
                  }
                  | CIRCADD LPAR rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    $$ = gen_circ_op(c, &@1, &$3, &$5, &$7);
                  }
                  | LOCNT LPAR rvalue RPAR
                  {
                    @1.last_column = @4.last_column;
                    /* Leading ones count */
                    $$ = gen_bitcnt_op(c, &@1, &$3, true, false);
                  }
                  | LOCNT LPAR BREV LPAR rvalue RPAR RPAR
                  {
                    @1.last_column = @7.last_column;
                    /* Trailing ones count */
                    $$ = gen_bitcnt_op(c, &@1, &$5, true, true);
                  }
                  | LOCNT LPAR NOT BREV LPAR rvalue RPAR RPAR
                  {
                    @1.last_column = @8.last_column;
                    /* Trailing zeroes count */
                    $$ = gen_bitcnt_op(c, &@1, &$6, false, true);
                  }
                  | COUNTONES LPAR rvalue RPAR
                  {
                    @1.last_column = @4.last_column;
                    /* Ones count */
                    $$ = gen_ctpop_op(c, &@1, &$3);

                  }
                  | LPCFG
                  {
                    $$ = gen_tmp_value(c, &@1, "0", 32);
                    OUT(c, &@1, "tcg_gen_extract_tl(", &$$, ", hex_gpr[HEX_REG_USR], ");
                    OUT(c, &@1, "reg_field_info[USR_LPCFG].offset, ");
                    OUT(c, &@1, "reg_field_info[USR_LPCFG].width);\n");
                  }
                  | EXTRACT LPAR rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @6.last_column;
                    $$ = gen_extract_op(c, &@1, &$5, &$3, &$1);
                  }
                  | EXTBITS LPAR rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    yyassert(c, &@1, $5.type == IMMEDIATE &&
                                     $5.imm.type == VALUE &&
                                     $7.type == IMMEDIATE &&
                                     $7.imm.type == VALUE,
                             "Range extract needs immediate values!\n");
                    $1.begin = $7.imm.value;
                    $1.end = $5.imm.value - 1 + $7.imm.value;
                    $$ = gen_rextract_op(c, &@1, &$3, &$1);
                  }
                  | EXTRANGE LPAR rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    @1.last_column = @8.last_column;
                    yyassert(c, &@1, $5.type == IMMEDIATE &&
                                     $5.imm.type == VALUE &&
                                     $7.type == IMMEDIATE &&
                                     $7.imm.type == VALUE,
                             "Range extract needs immediate values!\n");
                    $1.begin = $7.imm.value;
                    $1.end = $5.imm.value;
                    $$ = gen_rextract_op(c, &@1, &$3, &$1);
                  }
;

pre               : PRE
                  {
                    $$ = $1;
                  }
                  | pre NEW
                  {
                    $$ = $1;
                    $$.is_dotnew = true;
                  }
;

lvalue            : REG
                  {
                    $$ = $1;
                  }
                  | VAR
                  {
                    $$ = $1;
                  }
;

%%

int main(int argc, char **argv)
{
    if (argc < 3) {
        fprintf(stderr, "Semantics: Hexagon ISA to tinycode generator compiler\n\n");
        fprintf(stderr, "Copyright (c) 2017 Alessandro Di Federico, ");
        fprintf(stderr, "rev.ng Srls Unipersonale\n");
        fprintf(stderr, "Author: Niccolò Izzo <n@izzo.sh>\n\n");
        fprintf(stderr, "Usage: ./semantics META-INSTRUCTIONS-CSV DEFINES\n");
        return 1;
    }

    puts("#include \"qemu/osdep.h\"");
    puts("#include \"qemu/log.h\"");
    puts("#include \"cpu.h\"");
    puts("#include \"internal.h\"");
    puts("#include \"tcg/tcg-op.h\"");
    puts("#include \"insn.h\"");
    puts("#include \"opcodes.h\"");
    puts("#include \"translate.h\"");
    puts("#include \"genptr_helpers.h\"");

    FILE *defines_file = fopen(argv[2], "w");
    assert(defines_file != NULL);
    fputs("#ifndef TCG_AUTO_GEN\n", defines_file);
    fputs("#define TCG_AUTO_GEN\n", defines_file);
    fputs("\n", defines_file);
    fputs("#include \"insn.h\"\n\n", defines_file);

    int total_insn = 0, implemented_insn = 0;
    CsvParser *csvparser = CsvParser_new(argv[1], ",", 0);
    CsvRow *row;
    while ((row = CsvParser_getRow(csvparser)) ) {
        context_t context = { 0 };
        context.defines_file = defines_file;
        total_insn++;
        const char **rowFields = CsvParser_getFields(row);
        if (CsvParser_getNumFields(row) < 2) {
            fprintf(stderr, "Error: malformed csv!\n");
            return 1;
        }
        /* Extract field and initialize buffer */
        context.inst_name = rowFields[0];
        context.inst_code = rowFields[1];
        size_t in_buffer_size = strlen(context.inst_code) + 2;
        char * in_buffer = (char *) calloc(in_buffer_size, sizeof(char));
        memcpy(in_buffer, context.inst_code, in_buffer_size - 2);
        in_buffer[in_buffer_size - 2] = '\0';
        in_buffer[in_buffer_size - 1] = '\0';
        char * out_buffer = (char *) calloc(OUT_BUF_LEN, sizeof(char));
        char * signature_buffer = (char *) calloc(SIGNATURE_BUF_LEN, sizeof(char));
        context.out_buffer = out_buffer;
        context.signature_buffer = signature_buffer;
        fprintf(stderr, "Compiling: %s\n", context.inst_name);
        emit_header(&context);
        yylex_init(&context.scanner);
        yy_scan_buffer(in_buffer, in_buffer_size, context.scanner);
        /* Start the parsing procedure */
        yyparse(context.scanner, &context);
        if (context.error_count != 0) {
            fprintf(stderr, "Parsing of instruction %s generated %d errors!\n",
                    context.inst_name,
                    context.error_count);
            context.out_c += snprintf(context.out_buffer+context.out_c, OUT_BUF_LEN-context.out_c,
                "assert(false && \"This instruction is not implemented!\");");
        } else {
            implemented_insn++;
            emit_footer(&context);
            commit(&context);
        }
        /* Cleanup */
        yylex_destroy(context.scanner);
        CsvParser_destroy_row(row);
        free(in_buffer);
        free(out_buffer);
        free(signature_buffer);
        for(int i = 0; i < context.allocated_count; i++) {
            free((char *)context.allocated[i].name);
        }
    }
    CsvParser_destroy(csvparser);
    fprintf(stderr, "%d/%d meta instructions have been implemented!\n", implemented_insn, total_insn);
    fputs("#endif /* TCG_AUTO_GEN */\n", defines_file);
    fclose(defines_file);
    return 0;
}
