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

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "semantics_struct.h"
#include "semantics.tab.h"
#include "lex.yy.h"
#include "csvparser.h"

// TODO: Use this instead of void* declaration when we'll compile with std=c11
// Break circular header dependency
//typedef void* yyscan_t;

#if __STDC_VERSION__ >= 201112L
#define C11
#endif

// Decomment this to disable yyasserts
//#define NDEBUG

void yyerror(yyscan_t scanner __attribute__((unused)),
             context_t *c, const char *s)               
{                                   
    fprintf(stderr, "WARNING: '%s'\n", s);
    c->error_count++;                  
}                                   

#ifndef NDEBUG
#define yyassert(context, condition, msg) \
    if (!(condition)) { \
        yyerror((context)->scanner, (context), (msg)); \
    }
#endif

/* Print functions */
void str_print(context_t *c  __attribute__((unused)), char *string) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%s", string);
}

void uint64_print(context_t *c __attribute__((unused)), uint64_t *num) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%" PRIu64, *num);
}

void int_print(context_t *c __attribute__((unused)), int *num) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%d", *num);
}

void tmp_print(context_t *c __attribute__((unused)), t_hex_tmp *tmp) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "tmp_");
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%d", tmp->index);
}

void pre_print(context_t *c, t_hex_pre *pre, bool is_dotnew) {
    char suffix = is_dotnew ? 'N' : 'V';
    c->out_c += snprintf(c->out_buffer+c->out_c,
                         OUT_BUF_LEN,
                         "P%c%c",
                         pre->id,
                         suffix);
}

void reg_print(context_t *c, t_hex_reg *reg, bool is_dotnew) {
  char reg_id[5] = { 0 };

  yyassert(c, reg->type != SYSTEM || !is_dotnew,
         "System registers can't be .new!");

  switch (reg->type) {
    case GENERAL_PURPOSE:
        reg_id[0] = 'R';
        break;
    case CONTROL:
        reg_id[0] = 'C';
        break;
    case SYSTEM:
        reg_id[0] = 'S';
        break;
    case MODIFIER:
        reg_id[0] = 'M';
  }
  switch (reg->bit_width) {
    case 32:
        reg_id[1] = reg->id;
        reg_id[2] = 'V';
        break;
    case 64:
        reg_id[1] = reg->id;
        reg_id[2] = reg->id;
        reg_id[3] = 'V';
        break;
    default:
        yyassert(c, false, "Unhandled register bit width!\n");
  }
  c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%s", reg_id);
}

void imm_print(context_t *c, t_hex_imm *imm) {
    switch(imm->type) {
        case VARIABLE:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%ciV", imm->id);
            break;
        case VALUE:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%" PRIu64, imm->value);
            break;
        case QEMU_TMP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "qemu_tmp_%" PRIu64, imm->index);
            break;
        case IMM_PC:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "dc->pc");
            break;
        case IMM_CONSTEXT:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "dc->extender_present");
            break;
        default:
            yyassert(c, false, "Cannot print this expression!");
    }
}

void extra_print(context_t *c, t_hex_extra *extra) {
    switch (extra->type) {
        case EA_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "EA");
            break;
        case LPCFG_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "LPCFG");
            break;
        case LC_T:
            yyassert(c, false, "TODO: Implement LC control register access\n");
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "LC[%d]", extra->index); 
            break;
        case SA_T:
            yyassert(c, false, "TODO: Implement SA control register access\n");
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "SA[%d]", extra->index); 
            break;
        case WIDTH_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "width");
            break;
        case OFFSET_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "offset");
            break;
        case SHAMT_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "shamt");
            break;
        case ADDR_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "addr");
            break;
        case SUMR_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "sumr");
            break;
        case SUMI_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "sumi");
            break;
        case CTRL_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "control");
            break;
        case TMPR_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "tmpr");
            break;
        case TMPI_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "tmpi");
            break;
        case X0_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "x0");
            break;
        case X1_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "x1");
            break;
        case Y0_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "y0");
            break;
        case Y1_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "y1");
            break;
        case PROD0_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "prod0");
            break;
        case PROD1_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "prod1");
            break;
        case MAX_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "max");
            break;
        case MIN_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "min");
            break;
        case TMP_T:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "tmp");
            break;
        default:
            yyassert(c, false, "Malformed extra type!");
    }
}

void var_print(context_t *c, t_hex_var *var) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%s", var->name);
}

void rvalue_out(context_t *c, void *pointer) {
  t_hex_value *rvalue = (t_hex_value *) pointer;
  switch (rvalue->type) {
      case REGISTER:
          reg_print(c, &rvalue->reg, rvalue->is_dotnew);
          break;
      case TEMP:
          tmp_print(c, &rvalue->tmp);
          break;
      case IMMEDIATE:
          imm_print(c, &rvalue->imm);
          break;
      case EXTRA:
          extra_print(c, &rvalue->extra);
          break;
      case VARID:
          var_print(c, &rvalue->var);
          break;
      case PREDICATE:
          pre_print(c, &rvalue->pre, rvalue->is_dotnew);
          break;
      default:
          yyassert(c, false, "Cannot print this expression!");
  }
}

void cmp_out(context_t *c, void *pointer) {
    enum cmp_type *type = (enum cmp_type *) pointer;
    switch(*type) {
        case EQ_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_EQ");
            break;
        case NEQ_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_NE");
            break;
        case OPTEQ_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "not");
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "%d", c->not_count);
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, " ? TCG_COND_NE : TCG_COND_EQ");
            c->not_count++;
            break;
        case LT_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_LT");
            break;
        case LTU_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_LTU");
            break;
        case GT_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_GT");
            break;
        case GTU_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_GTU");
            break;
        case LTE_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_LE");
            break;
        case LEU_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_LEU");
            break;
        case GTE_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_GE");
            break;
        case GEU_OP:
            c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "TCG_COND_GEU");
            break;
        default:
            yyassert(c, false, "Unhandled comparison operator!");
    }
}

/* Copy output code buffer into stdout */
void commit(context_t *c) {
    printf("#ifdef fAUTO_GEN_TCG_%s\n", c->inst_name);
    fwrite(c->signature_buffer, sizeof(char), c->signature_c, stdout);
    fwrite(c->out_buffer, sizeof(char), c->out_c, stdout);
    puts("#endif\n");

    fwrite(c->signature_buffer, sizeof(char), c->signature_c, c->defines_file);
    fprintf(c->defines_file, ";\n");
    c->out_c = 0;
}

#define OUT_IMPL(c, x)                                                     \
  do {                                                                  \
    if (__builtin_types_compatible_p (typeof (*x), char))               \
      str_print((c), (char *) x);                                            \
    else if (__builtin_types_compatible_p (typeof (*x), uint64_t))      \
      uint64_print((c), (uint64_t *) x);                                     \
    else if (__builtin_types_compatible_p (typeof (*x), int))           \
      int_print((c), (int *) x);                                             \
    else if (__builtin_types_compatible_p (typeof (*x), t_hex_value))     \
      rvalue_out((c), (t_hex_value *) x);                                         \
    else if (__builtin_types_compatible_p (typeof (*x), enum cmp_type)) \
      cmp_out((c), (enum cmp_type *) x);                                     \
    else                                                                \
      yyassert(c, false, "Unhandled print type!");                         \
  } while(0);

// Make a FOREACH macro
#define FE_1(c, WHAT, X) WHAT(c, X)
#define FE_2(c, WHAT, X, ...) WHAT(c, X)FE_1(c, WHAT, __VA_ARGS__)
#define FE_3(c, WHAT, X, ...) WHAT(c, X)FE_2(c, WHAT, __VA_ARGS__)
#define FE_4(c, WHAT, X, ...) WHAT(c, X)FE_3(c, WHAT, __VA_ARGS__)
#define FE_5(c, WHAT, X, ...) WHAT(c, X)FE_4(c, WHAT, __VA_ARGS__)
#define FE_6(c, WHAT, X, ...) WHAT(c, X)FE_5(c, WHAT, __VA_ARGS__)
#define FE_7(c, WHAT, X, ...) WHAT(c, X)FE_6(c, WHAT, __VA_ARGS__)
#define FE_8(c, WHAT, X, ...) WHAT(c, X)FE_7(c, WHAT, __VA_ARGS__)
#define FE_9(c, WHAT, X, ...) WHAT(c, X)FE_8(c, WHAT, __VA_ARGS__)
//... repeat as needed

#define GET_MACRO(_1, _2, _3, _4, _5, _6, _7, _8, _9, NAME, ...) NAME

#define FOR_EACH(c, action, ...)                \
  do {                                          \
    GET_MACRO(__VA_ARGS__,                      \
              FE_9,                             \
              FE_8,                             \
              FE_7,                             \
              FE_6,                             \
              FE_5,                             \
              FE_4,                             \
              FE_3,                             \
              FE_2,                             \
              FE_1)(c, action,                     \
                    __VA_ARGS__)                \
  } while (0)

#define OUT(c, ...) FOR_EACH((c), OUT_IMPL, __VA_ARGS__)

enum cmp_type cmp_swap(context_t *c, enum cmp_type type) {
    switch(type) {
        case EQ_OP:
            return EQ_OP;
            break;
        case NEQ_OP:
            return NEQ_OP;
            break;
        case LT_OP:
            return GT_OP;
            break;
        case LTU_OP:
            return GTU_OP;
            break;
        case GT_OP:
            return LT_OP;
            break;
        case GTU_OP:
            return LTU_OP;
            break;
        case LTE_OP:
            return GTE_OP;
            break;
        case LEU_OP:
            return GEU_OP;
            break;
        case GTE_OP:
            return LTE_OP;
            break;
        case GEU_OP:
            return LEU_OP;
            break;
        default:
            yyassert(c, false, "Unhandled comparison swap!");
    }
    return 0;
}

t_hex_value gen_extra(context_t *c __attribute__((unused)), enum rvalue_extra_type type, int index, bool temp) {
    t_hex_value rvalue;
    rvalue.type = EXTRA;
    rvalue.is_unsigned = false;
    rvalue.is_dotnew = false;
    rvalue.is_vectorial = false;
    rvalue.is_range = false;
    rvalue.is_symbol = false;
    rvalue.extra.type = type;
    rvalue.extra.index = index;
    rvalue.extra.temp = temp;
    rvalue.bit_width = (type == TMP_T) ? 64 : 32;
    return rvalue;
}

/* Temporary values creation */
t_hex_value gen_tmp(context_t *c, int bit_width) {
    t_hex_value rvalue;
    rvalue.type = TEMP;
    bit_width = (bit_width == 64) ? 64 : 32;
    rvalue.bit_width = bit_width;
    rvalue.is_unsigned = false;
    rvalue.is_dotnew = false;
    rvalue.is_vectorial = false;
    rvalue.is_range = false;
    rvalue.is_symbol = false;
    rvalue.tmp.index = c->tmp_count;
    OUT(c, "TCGv_i", &bit_width, " tmp_", &c->tmp_count, " = tcg_temp_new_i",
        &bit_width, "();\n");
    c->tmp_count++;
    return rvalue;
}

t_hex_value gen_local_tmp(context_t *c, int bit_width) {
    t_hex_value rvalue;
    rvalue.type = TEMP;
    bit_width = (bit_width == 64) ? 64 : 32;
    rvalue.bit_width = bit_width;
    rvalue.is_unsigned = false;
    rvalue.is_dotnew = false;
    rvalue.is_vectorial = false;
    rvalue.is_range = false;
    rvalue.is_symbol = false;
    rvalue.tmp.index = c->tmp_count;
    OUT(c, "TCGv_i", &bit_width, " tmp_", &c->tmp_count, " = tcg_temp_local_new_i",
        &bit_width, "();\n");
    c->tmp_count++;
    return rvalue;
}

t_hex_value gen_tmp_value(context_t *c, char * value, int bit_width) {
    t_hex_value rvalue;
    rvalue.type = TEMP;
    rvalue.bit_width = bit_width;
    rvalue.is_unsigned = false;
    rvalue.is_dotnew = false;
    rvalue.is_vectorial = false;
    rvalue.is_range = false;
    rvalue.is_symbol = false;
    rvalue.tmp.index = c->tmp_count;
    OUT(c, "TCGv_i", &bit_width, " tmp_", &c->tmp_count, " = tcg_const_i", &bit_width, "(",
        value, ");\n");
    c->tmp_count++;
    return rvalue;
}

t_hex_value gen_imm_value(context_t *c __attribute__((unused)), int value, int bit_width) {
    t_hex_value rvalue;
    rvalue.type = IMMEDIATE;
    rvalue.bit_width = bit_width;
    rvalue.is_unsigned = false;
    rvalue.is_dotnew = false;
    rvalue.is_vectorial = false;
    rvalue.is_range = false;
    rvalue.is_symbol = false;
    rvalue.imm.type = VALUE;
    rvalue.imm.value = value;
    return rvalue;
}

void rvalue_free(context_t *c, t_hex_value *rvalue) {
    if (rvalue->type == TEMP) {
        char * bit_suffix = (rvalue->bit_width == 64) ? "i64" : "i32";
        OUT(c, "tcg_temp_free_", bit_suffix, "(", rvalue, ");\n");
    }
}

void rvalue_materialize(context_t *c, t_hex_value *rvalue) {
    if (rvalue->type == IMMEDIATE) {
        t_hex_value tmp = gen_tmp(c, rvalue->bit_width);
        char * bit_suffix = (rvalue->bit_width == 64) ? "i64" : "i32";
        OUT(c, "tcg_gen_movi_", bit_suffix, "(", &tmp, ", ", rvalue, ");\n");
        tmp.is_symbol = rvalue->is_symbol;
        rvalue_free(c, rvalue);
        *rvalue = tmp;
    }
}

void rvalue_extend(context_t *c, t_hex_value *rvalue) {
    if (rvalue->type == IMMEDIATE)
        rvalue->bit_width = 64;
    else {
        if (rvalue->bit_width == 32) {
            t_hex_value tmp = gen_tmp(c, 64);
            char * sign_suffix = (rvalue->is_unsigned) ? "u" : "";
            OUT(c, "tcg_gen_ext", sign_suffix, "_i32_i64(", &tmp, ", ", rvalue, ");\n");
            rvalue_free(c, rvalue);
            *rvalue = tmp;
        }
    }
}

void rvalue_truncate(context_t *c, t_hex_value *rvalue) {
    if (rvalue->type == IMMEDIATE)
        rvalue->bit_width = 32;
    else {
        if (rvalue->bit_width == 64) {
            t_hex_value tmp = gen_tmp(c, 32);
            OUT(c, "tcg_gen_trunc_i64_tl(", &tmp, ", ", rvalue, ");\n");
            rvalue_free(c, rvalue);
            *rvalue = tmp;
        }
    }
}

void ea_free(context_t *c) {
    OUT(c, "tcg_temp_free(EA);\n");
}

/* Code generation functions */
t_hex_value gen_bin_op(context_t *c,
                       enum op_type type,
                       t_hex_value *op1,
                       t_hex_value *op2)
{
#define IMM_IMM 0
#define IMM_REG 1
#define REG_IMM 2
#define REG_REG 3

    int op_types = (op1->type != IMMEDIATE) << 1 | (op2->type != IMMEDIATE);

    /* Find bit width of the two operands,
       if at least one is 64 bit use a 64bit operation,
       eventually extend 32bit operands. */
    bool op_is64bit = op1->bit_width == 64 || op2->bit_width == 64;
    /* Multiplication is always 64 bits wide */
    if (type == MULTIPLY)
        op_is64bit = true;
    /* Shift greater than 32 are 64 bits wide */
    if (type == ASHIFTL && op2->type == IMMEDIATE &&
        op2->imm.type == VALUE && op2->imm.value >= 32)
        op_is64bit = true;
    char * bit_suffix = op_is64bit ? "i64" : "i32";
    int bit_width = (op_is64bit) ? 64 : 32;
    /* TODO: Handle signedness */
    if (op_is64bit) {
        switch(op_types) {
            case IMM_REG:
                rvalue_extend(c, op2);
                break;
            case REG_IMM:
                rvalue_extend(c, op1);
                break;
            case REG_REG:
                rvalue_extend(c, op1);
                rvalue_extend(c, op2);
                break;
        }
    }
    t_hex_value res;
    if (op_types != IMM_IMM) {
        /* TODO: If one of the operands is a temp reuse it and don't free it */
        res = gen_tmp(c, bit_width);
        res.type = TEMP;
    } else {
        res.type = IMMEDIATE;
        res.is_unsigned = false;
        res.is_dotnew = false;
        res.is_vectorial = false;
        res.is_range = false;
        res.is_symbol = false;
        res.imm.type = QEMU_TMP;
        res.imm.index = c->qemu_tmp_count;
    }

    switch(type) {
        case ADD:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " + ", op2, ";\n");
                    break;
                case IMM_REG:
                    OUT(c, "tcg_gen_addi_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_addi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "tcg_gen_add_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case SUBTRACT:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " - ", op2, ";\n");
                    break;
                case IMM_REG:
                    OUT(c, "tcg_gen_subfi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_subi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "tcg_gen_sub_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ADDSUB:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = plus_minus ? (");
                    OUT(c, op1, " + ", op2, ") : (", op1, " - ", op2, ");\n");
                    break;
                case IMM_REG:
                    OUT(c, "if (plus_minus)\n");
                    OUT(c, "tcg_gen_addi_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_subi_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "if (plus_minus)\n");
                    OUT(c, "tcg_gen_addi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_subi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "if (plus_minus)\n");
                    OUT(c, "tcg_gen_add_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_sub_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case MULTIPLY:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int64_t ", &res, " = ", op1, " * ", op2, ";\n");
                    break;
                case IMM_REG:
                    rvalue_extend(c, op2);
                    OUT(c, "tcg_gen_muli_i64(", &res, ", ", op2, ", (int64_t)", op1, ");\n");
                    break;
                case REG_IMM:
                    rvalue_extend(c, op1);
                    OUT(c, "tcg_gen_muli_i64(", &res, ", ", op1, ", (int64_t)", op2, ");\n");
                    break;
                case REG_REG:
                    rvalue_extend(c, op1);
                    rvalue_extend(c, op2);
                    OUT(c, "tcg_gen_mul_i64(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case DIVIDE:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int64_t ", &res, " = ", op1, " / ", op2, ";\n");
                    break;
                case IMM_REG:
                case REG_IMM:
                case REG_REG:
                    OUT(c, &res, " = gen_helper_divu(cpu_env, ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ASHIFTL:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " << ", op2, ";\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_shli_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    /* fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_shl_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            if (op_types != IMM_IMM) {
                /* Handle left shift by 64 which hexagon-sim expects to clear out register */
                t_hex_value edge = gen_tmp_value(c, "64", bit_width);
                t_hex_value zero = gen_tmp_value(c, "0", bit_width);
                if (op_is64bit)
                    rvalue_extend(c, op2);
                rvalue_materialize(c, op1);
                rvalue_materialize(c, op2);
                op2->is_symbol = true;
                rvalue_materialize(c, &edge);
                OUT(c, "tcg_gen_movcond_i", &bit_width);
                if (op_types == REG_REG || op_types == IMM_REG)
                    OUT(c, "(TCG_COND_EQ, ", &res, ", ", op2, ", ", &edge);
                else
                    OUT(c, "(TCG_COND_EQ, ", &res, ", ", op2, ", ", &edge);
                OUT(c, ", ", &zero, ", ", &res, ");\n");
                rvalue_free(c, &edge);
                rvalue_free(c, &zero);
            }
            break;
        }
        case ASHIFTR:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " >> ", op2, ";\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_sari_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    /* fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_sar_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case LSHIFTR:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " >> ", op2, ";\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_shri_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    /* fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_shr_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ROTATE:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " >> ", op2, ";\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_rotli_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    /* fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_rotl_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ANDB:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " & ", op2, ";\n");
                    break;
                case IMM_REG:
                    OUT(c, "tcg_gen_andi_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_andi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "tcg_gen_and_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ORB:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " & ", op2, ";\n");
                    break;
                case IMM_REG:
                    OUT(c, "tcg_gen_ori_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_ori_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "tcg_gen_or_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case ANDORB:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = and_or ? (");
                    OUT(c, op1, " & ", op2, ") : (", op1, " | ", op2, ");\n");
                    break;
                case IMM_REG:
                    OUT(c, "if (and_or)\n");
                    OUT(c, "tcg_gen_andi_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_ori_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "if (and_or)\n");
                    OUT(c, "tcg_gen_andi_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_ori_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "if (and_or)\n");
                    OUT(c, "tcg_gen_and_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    OUT(c, "else\n");
                    OUT(c, "tcg_gen_or_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case XORB:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = ", op1, " & ", op2, ";\n");
                    break;
                case IMM_REG:
                    OUT(c, "tcg_gen_xori_", bit_suffix, "(", &res, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    OUT(c, "tcg_gen_xori_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_REG:
                    OUT(c, "tcg_gen_xor_", bit_suffix, "(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case MINI:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = (", op1, " <= ");
                    OUT(c, op2, ") ? ", op1, " : ", op2, ";\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    OUT(c, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, "(TCG_COND_LE, ", &res, ", ", op1, ", ", op2);
                    OUT(c, ", ", op1, ", ", op2, ");\n");
                    break;
                case REG_IMM:
                    rvalue_materialize(c, op2);
                    /* Fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, "(TCG_COND_LE, ", &res, ", ", op1, ", ", op2);
                    OUT(c, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case MAXI:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int", &bit_width, "_t ", &res, " = (", op1, " <= ");
                    OUT(c, op2, ") ? ", op2, " : ", op1, ";\n");
                    break;
                case IMM_REG:
                    rvalue_materialize(c, op1);
                    OUT(c, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, "(TCG_COND_LE, ", &res, ", ", op1, ", ", op2);
                    OUT(c, ", ", op2, ", ", op1, ");\n");
                    break;
                case REG_IMM:
                    rvalue_materialize(c, op2);
                    /* Fallthrough */
                case REG_REG:
                    OUT(c, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, "(TCG_COND_LE, ", &res, ", ", op1, ", ", op2);
                    OUT(c, ", ", op2, ", ", op1, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
        case MODULO:
        {
            switch(op_types) {
                case IMM_IMM:
                    OUT(c, "int64_t ", &res, " = ", op1, " % ", op2, ";\n");
                    break;
                case IMM_REG:
                case REG_IMM:
                case REG_REG:
                    OUT(c, "gen_helper_mod(", &res, ", ", op1, ", ", op2, ");\n");
                    break;
                default:
                    fprintf(stderr, "Error in evalutating immediateness!");
                    abort();
            }
            break;
        }
    }
    /* Free operands only if they are unnamed */
    if (!op1->is_symbol)
        rvalue_free(c, op1);
    if (!op2->is_symbol)
        rvalue_free(c, op2);
    if (op_types == IMM_IMM)
        c->qemu_tmp_count++;
    return res;

#undef IMM_IMM
#undef IMM_REG
#undef REG_IMM
#undef REG_REG
}

t_hex_value gen_bin_cmp(context_t *c,
                        enum cmp_type type,
                        t_hex_value *op1,
                        t_hex_value *op2)
{
#define IMM_IMM 0
#define IMM_REG 1
#define REG_IMM 2
#define REG_REG 3

    int op_types = (op1->type != IMMEDIATE) << 1 | (op2->type != IMMEDIATE);

    /* Find bit width of the two operands,
       if at least one is 64 bit use a 64bit operation,
       eventually extend 32bit operands. */
    bool op_is64bit = op1->bit_width == 64 || op2->bit_width == 64;
    char * bit_suffix = op_is64bit ? "i64" : "i32";
    int bit_width = (op_is64bit) ? 64 : 32;
    /* TODO: Handle signedness */
    if (op_is64bit) {
        switch(op_types) {
            case IMM_REG:
                rvalue_extend(c, op2);
                break;
            case REG_IMM:
                rvalue_extend(c, op1);
                break;
            case REG_REG:
                rvalue_extend(c, op1);
                rvalue_extend(c, op2);
                break;
        }
    }

    t_hex_value res = gen_tmp(c, bit_width);

    switch(op_types) {
        case IMM_IMM:
        {
            OUT(c, "tcg_gen_movi_", bit_suffix, "(", &res, ", ", op1, " == ", op2, ");\n");
            break;
        }
        case IMM_REG:
        {
            t_hex_value swp = *op2;
            *op2 = *op1;
            *op1 = swp;
            /* Swap comparison direction */
            type = cmp_swap(c, type);
            /* fallthrough */
        }
        case REG_IMM:
        {
            OUT(c, "tcg_gen_setcondi_", bit_suffix, "(");
            OUT(c, &type, ", ", &res, ", ", op1, ", ", op2, ");\n");
            break;
        }
        case REG_REG:
        {
            OUT(c, "tcg_gen_setcond_", bit_suffix, "(");
            OUT(c, &type, ", ", &res, ", ", op1, ", ", op2, ");\n");
            break;
        }
        default:
        {
            fprintf(stderr, "Error in evalutating immediateness!");
            abort();
        }
    }
    /* Free operands */
    /* TODO: Try to eliminate double free */
    rvalue_free(c, op1);
    rvalue_free(c, op2);

    return res;

#undef IMM_IMM
#undef IMM_REG
#undef REG_IMM
#undef REG_REG
}

t_hex_value gen_extract(context_t *c, t_hex_value *source) {
    if (!source->is_vectorial) {
        /* Handle range extraction */
        if (source->is_range) {
            int bit_width = (source->bit_width == 64) ? 64 : 32;
            int begin = source->range.begin;
            int end = source->range.end;
            int width = end - begin + 1;
            t_hex_value res = gen_tmp(c, bit_width);
            OUT(c, "tcg_gen_extract_i", &bit_width, "(", &res, ", ", &source);
            OUT(c, ", ", &begin, ", ", &width, ");\n");
            *source = res;
        } else {
            return *source;
        }
    }
    t_hex_vec access = source->vec;
    int width = access.width;
    t_hex_value res = gen_tmp(c, source->bit_width);
    /* Generating string containing access offset */
    char offset_string[OFFSET_STR_LEN];
    int offset_value = access.index * width;
    snprintf(offset_string, OFFSET_STR_LEN, "%d", offset_value);
    char * offset = offset_string;
    if (access.iter_type != NO_ITER) {
        /* All iteration types */
        switch(access.iter_type) {
            case I_ITER:
                snprintf(offset, OFFSET_STR_LEN, "i * %d", width);
                break;
            case I2_ITER:
                snprintf(offset, OFFSET_STR_LEN, "i*2 * %d", width);
                break;
            case I2PLUS1_ITER:
                snprintf(offset, OFFSET_STR_LEN, "(i*2+1) * %d", width);
                break;
            case IPLUS1_ITER:
                snprintf(offset, OFFSET_STR_LEN, "(i+1) * %d", width);
                break;
            case IPLUS4_ITER:
                snprintf(offset, OFFSET_STR_LEN, "(i+4) * %d", width);
                break;
            case IMM_ITER:
                snprintf(offset, OFFSET_STR_LEN, "%ciV", access.id);
            default:
                yyassert(c, false, "Unhandled iterator enum type!\n");
        }
    }
    /* Sanity check that offset is positive */
    yyassert(c, offset[0] != '-', "Offset is negative, fix lexer!\n");
    if (source->type == REGISTER) {
        char * increment = "";
        /* Handle write to 64 bit registers */
        if (offset_value >= 32) {
            offset_value -= 32;
            snprintf(offset_string, OFFSET_STR_LEN, "%d", offset_value);
            increment = " + 1";
        } else {
            OUT(c, "tcg_gen_extract_i32(", &res, ", R");
            OUT(c, &(source->reg.id), "V", increment);
            OUT(c, ", ", offset, ", ", &width, ");\n");
        }
    } else {
        if (source->bit_width == 64)
            rvalue_extend(c, source);
        rvalue_materialize(c, source);
        int bit_width = (source->bit_width == 64) ? 64 : 32;
        OUT(c, "tcg_gen_extract_i", &bit_width, "(", &res, ", ", source);
        OUT(c, ", ", offset, ", ", &width, ");\n");
    }
    /* Handle vectorial+range extraction */
    if (source->is_range) {
        int bit_width = (source->bit_width == 64) ? 64 : 32;
        int begin = source->range.begin;
        int end = source->range.end;
        int width = end - begin + 1;
        OUT(c, "tcg_gen_extract_i", &bit_width, "(", &res, ", ", &res);
        OUT(c, ", ", &begin, ", ", &width, ");\n");
    }
    rvalue_free(c, source);
    /* Apply source properties */
    res.vec = source->vec;
    res.is_vectorial = source->is_vectorial;
    res.is_unsigned = source->is_unsigned;
    return res;
}

void gen_deposit(context_t *c,
                 t_hex_value *dest,
                 t_hex_value *value) {
    /* TODO: Implement register deposit */
    t_hex_vec access = dest->vec;
    int width = access.width;
    /* Generating string containing access offset */
    char offset_string[OFFSET_STR_LEN] = { 0 };
    char increment_string[OFFSET_STR_LEN] = { 0 };
    char * offset = offset_string;
    char * increment = increment_string;
    int offset_value = access.index * width;
    snprintf(offset_string, OFFSET_STR_LEN, "%d", offset_value);
    if (access.iter_type != NO_ITER) {
        /* All iteration types */
        switch(access.iter_type) {
            case I_ITER:
                offset = "i";
                break;
            case I2_ITER:
                offset = "i*2";
                break;
            case I2PLUS1_ITER:
                offset = "i*2+1";
                break;
            case IPLUS1_ITER:
                offset = "i+1";
                break;
            case IPLUS4_ITER:
                offset = "i+4";
                break;
            default:
                yyassert(c, false, "Unhandled iterator enum type!\n");
        }
    }
    if (dest->type == REGISTER) {
        /* Handle runtime 64bit register selection by i iterator */
        if (access.iter_type != NO_ITER) {
            // deposit is broken when saving to 64bit registers
            // Algorithm is: offset = <iterator> % (32 / width)
            //            increment = <iterator> / (32 / width)
            // If width is 32 it is fine, if width is 16 we have to store
            // split up the deposit
            snprintf(increment_string, OFFSET_STR_LEN, "+ %s / (32 / %d)", offset, width);
            snprintf(offset_string, OFFSET_STR_LEN, "%s * %d", offset, width);
            offset = offset_string;
            // If the destination value is 32, truncate the source, otherwise extend
            if (dest->bit_width == 32)
                rvalue_truncate(c, value);
            else
                rvalue_extend(c, value);
            rvalue_materialize(c, value);
            char * bit_suffix = (dest->bit_width == 64) ? "i64" : "i32";
            OUT(c, "tcg_gen_deposit_", bit_suffix, "(", dest, ", ", dest, ", ", value);
            OUT(c, ", ", offset, ", ", &width, ");\n");
        } else {
            rvalue_truncate(c, value);
            rvalue_materialize(c, value);
            OUT(c, "if (GET_USED_REG(regs, ", &(dest->reg.id), increment, "))\n");
            OUT(c, "tcg_gen_deposit_i32(GPR_new[", &(dest->reg.id), increment);
            OUT(c, "], GPR_new[", &(dest->reg.id), increment, "], ", value);
            OUT(c, ", ", offset, ", ", &width, ");\n");
            OUT(c, "else\n");
            OUT(c, "tcg_gen_deposit_i32(GPR_new[", &(dest->reg.id), increment);
            OUT(c, "], GPR[", &(dest->reg.id), increment, "], ", value);
            OUT(c, ", ", offset, ", ", &width, ");\n");
        }
    } else {
        if (dest->extra.temp) {
            if (!c->is_extra_created[dest->extra.type]) {
                OUT(c, "TCGv_i", &dest->bit_width, " ", dest,
                    " = tcg_temp_new_i", &dest->bit_width, "();\n");
            }
        }
        OUT(c, "tcg_gen_deposit_i32(", dest, ", ", dest, ", ");
        OUT(c, value, ", ", offset, ", ", &width, ");\n");
    }
    rvalue_free(c, value);
}

void gen_assign(context_t *c, t_hex_value *dest, t_hex_value *value) {
    dest->is_symbol = true;
    value->is_symbol = true;

    int bit_width = dest->bit_width;
    if (dest->is_vectorial) {
        gen_deposit(c, dest, value);
        return;
    }
    if (dest->type == EXTRA) {
        if (dest->bit_width == 64)
            rvalue_extend(c, value);
        else
            rvalue_truncate(c, value);
        if (dest->extra.temp) {
            yyassert(c, !(dest->extra.type == EA_T && c->is_extra_created[EA_T]),
                   "EA assigned multiple times!");
            if (!c->is_extra_created[dest->extra.type]) {
                /* EA must be a tmp_local because it might cross a branch */
                OUT(c, "TCGv_i", &bit_width, " ", dest,
                    " = tcg_temp_local_new_i", &bit_width, "();\n");
                c->is_extra_created[dest->extra.type] = true;
            }
        }
        if (value->type == IMMEDIATE)
            OUT(c, "tcg_gen_movi_i", &bit_width, "(", dest, ", ", value, ");\n");
        else
            OUT(c, "tcg_gen_mov_i", &bit_width, "(", dest, ", ", value, ");\n");
        rvalue_free(c, value); /* Free temporary value */
        return;
    } else if (dest->type == VARID) {
        /* Declare TCGv variable if it has not been previously declared */
        bool already_alloc = false;
        for (int i = 0; i < ALLOC_LIST; i++) {
            already_alloc |= (strncmp(dest->var.name,
                                      c->allocated[i],
                                      VAR_BUF_LEN) == 0);
        }
        /* TODO: output something like TCGv_i32 varname = tcg_temp_local_new_i32();
                                       tcg_gen_movi_i32(varname, value); */
        if (!already_alloc) {
            ;
        }
        return;
    }
    if (dest->bit_width == 64) {
        rvalue_extend(c, value);
        rvalue_materialize(c, value);
        yyassert(c, value->bit_width == 64,
               "Bit width mismatch in assignment!");
        OUT(c, "tcg_gen_mov_i64(", dest, ", ", value, ");\n");
        /* TODO assert that no one is using this value as Nt */
    } else if (dest->bit_width == 32){
        if (value->type == IMMEDIATE)
            OUT(c, "tcg_gen_movi_tl(");
        else {
            if (value->bit_width == 64)
                OUT(c, "tcg_gen_trunc_i64_tl(");
            else
                OUT(c, "tcg_gen_mov_tl(");
        }
        t_hex_value reg_new = *dest;
        if (dest->reg.type != SYSTEM)
            reg_new.is_dotnew = true;
        OUT(c, &reg_new, ", ", value, ");\n");
    } else
        yyassert(c, false, "Unhandled bit width!");
    rvalue_free(c, value);
}


t_hex_value gen_cast_op(context_t *c,
                        t_hex_value *source,
                        unsigned target_width) {
    // Bit width sanity check
    //yyassert(c, (source->bit_width == 32 || source->bit_width == 64) &&
    //       (target_width == 32 || target_width == 64),
    //       "Unhandled cast operation!");
    if (source->bit_width == target_width)
        return *source;
    else if (source->type == IMMEDIATE) {
        source->bit_width = target_width;
        source->imm.value %= target_width;
        return *source;
    } else {
        t_hex_value res = gen_tmp(c, target_width);
        // Truncate
        if (source->bit_width > target_width)
            OUT(c, "tcg_gen_trunc_i64_tl(", &res, ", ", source, ");\n");
        // Extend unsigned
        else if (source->is_unsigned)
            OUT(c, "tcg_gen_extu_i32_i64(", &res, ", ", source, ");\n");
        // Extend signed
        else
            OUT(c, "tcg_gen_ext_i32_i64(", &res, ", ", source, ");\n");
        rvalue_free(c, source);
        return res;
    }
}

t_hex_value gen_zxt_op(context_t *c,
                       t_hex_value *source,
                       t_hex_value *source_width,
                       t_hex_value *target_width) {
    /* Handle weird destination widths */
    if (target_width->imm.value > 32)
        target_width->imm.value = 64;
    t_hex_value tmp = *source;
    if (source_width->type != IMMEDIATE || source_width->imm.value != source->bit_width) {
        // Cast source_width width to source bit width
        *source_width = gen_cast_op(c, source_width, source->bit_width);
        rvalue_materialize(c, source_width);
        /* First zero-out unwanted bits */
        t_hex_value one = gen_imm_value(c, 1, source->bit_width);
        t_hex_value tmp_mask = gen_bin_op(c, ASHIFTL, &one, source_width);
        one = gen_imm_value(c, 1, source->bit_width);
        t_hex_value mask = gen_bin_op(c, SUBTRACT, &tmp_mask, &one);
        tmp = gen_bin_op(c, ANDB, source, &mask);
    }
    /* 32 bit constants are already zero extended */
    if (target_width->imm.value == 32)
        return tmp;
    else if (target_width->imm.value == 64) {
        tmp.is_unsigned = true;
        rvalue_extend(c, &tmp);
    } else
        yyassert(c, false, "Unhandled destination bit width!");
    return tmp;
}

t_hex_value gen_convround(context_t *c, t_hex_value *source, t_hex_value *round_bit) {
    round_bit->is_symbol = true;
    /* Round bit is given in one hot encoding */
    /* If input is 64 bit cast it to 32 (used for vavgw) */
    *source = gen_cast_op(c, source, 32);
    source->is_symbol = true;
    /* Add .5 if > .5 but not if is == .5 and value is even */
    yyassert(c, source->bit_width <= 32,
           "Convround not implemented for bit widths > 32!");
    t_hex_value zero = gen_tmp_value(c, "0", 32);
    t_hex_value one = gen_imm_value(c, 1, 32);
    t_hex_value two = gen_imm_value(c, 2, 32);
    t_hex_value remainder = gen_bin_op(c, ANDB, source, round_bit);
    t_hex_value tmp_mask = gen_bin_op(c, ASHIFTL, round_bit, &two);
    t_hex_value mask = gen_bin_op(c, SUBTRACT, &tmp_mask, &one);
    t_hex_value masked_value = gen_bin_op(c, ANDB, source, &mask);
    rvalue_materialize(c, &masked_value);
    rvalue_materialize(c, round_bit);
    /* If value is even and == .5 do not round */
    t_hex_value new_remainder = gen_tmp(c, 32);
    OUT(c, "tcg_gen_movcond_i32(TCG_COND_EQ, ", &new_remainder);
    OUT(c, ", ", &masked_value, ", ", round_bit, ", ");
    OUT(c, &zero, ", ", &remainder, ");\n");
    t_hex_value res = gen_bin_op(c, ADD, source, &new_remainder);
    /* Zero out trailing bits */
    mask = gen_bin_op(c, ASHIFTL, round_bit, &one);
    mask = gen_bin_op(c, SUBTRACT, &mask, &one);
    t_hex_value new_mask = gen_tmp(c, 32);
    OUT(c, "tcg_gen_not_i32(", &new_mask, ", ", &mask, ");\n");
    res = gen_bin_op(c, ANDB, &res, &new_mask);
    rvalue_free(c, &remainder);
    rvalue_free(c, &masked_value);
    rvalue_free(c, &mask);
    rvalue_free(c, &zero);
    rvalue_free(c, source);
    rvalue_free(c, round_bit);
    return res;
}

/* Circular addressing mode with auto-increment */
t_hex_value gen_circ_op(context_t *c,
                        t_hex_value *addr,
                        t_hex_value *increment,
                        t_hex_value *modifier) {
    t_hex_value cs = gen_tmp(c, 32);
    rvalue_materialize(c, increment);
    OUT(c, "READ_REG(", &cs, ", HEX_REG_CS0 + MuN);\n");
    OUT(c, "gen_fcircadd(", addr, ", ", increment, ", ", modifier);
    OUT(c, ", ", &cs, ");\n");
    rvalue_free(c, &cs);
    rvalue_free(c, increment);
    return *addr;
}

t_hex_value gen_bitcnt_op(context_t *c, t_hex_value *source,
                          bool negate,
                          bool reverse)
{
    char * bit_suffix = source->bit_width == 64 ? "64" : "32";
    t_hex_value res = gen_tmp(c, source->bit_width == 64 ? 64 : 32);
    res.type = TEMP;
    /* TODO: use native c primitive if we deal with immediates */
    rvalue_materialize(c, source);
    switch(negate << 1 | reverse) {
        case 0b00:
            OUT(c, "tcg_gen_clzi_i", bit_suffix, "(", &res, ", ", source, ", ");
            OUT(c, bit_suffix, ");");
            break;
        case 0b01:
            OUT(c, "tcg_gen_ctzi_i", bit_suffix, "(", &res, ", ", source, ", ");
            OUT(c, bit_suffix, ");");
            break;
        case 0b10:
            OUT(c, "tcg_gen_not_i", bit_suffix, "(", &res, ", ", source, ");\n");
            OUT(c, "tcg_gen_clzi_i", bit_suffix, "(", &res, ", ", &res, ", ");
            OUT(c, bit_suffix, ");");
            break;
        case 0b11:
            OUT(c, "tcg_gen_not_i", bit_suffix, "(", &res, ", ", source, ");\n");
            OUT(c, "tcg_gen_clzi_i", bit_suffix, "(", &res, ", ", &res, ", ");
            OUT(c, bit_suffix, ");");
            break;
    }
    rvalue_free(c, source);
    return res;
}

%}

%lex-param {void *scanner}
%parse-param {void *scanner}
%parse-param {context_t *c}

%define parse.error verbose
%define parse.lac full
%define api.pure full

%union {
    t_hex_value rvalue;
    t_hex_vec vec;
    t_hex_cast cast;
    t_hex_range range;
    bool is_unsigned;
    int index;
}


/* Tokens */
%start code

//%expect 1

%token DREG DIMM DPRE DEA RREG WREG FREG FIMM RPRE WPRE FPRE FWRAP FEA
%token DMOD RMOD FMOD DCTR RCTR FCTR PREDUSE USCORE VAR
%token LBR RBR LPAR RPAR LSQ RSQ LARR
%token SEMI COLON PLUS MINUS PMINUS MUL POW DIV MOD ABS CROUND ROUND CIRCADD
%token AND OR ANDOR XOR NOT OPTSHIFT NSHIFT
%token ASSIGN INC DEC INCDECA ANDA ORA XORA ANDORA PLUSPLUS
%token LT GT ASL ASR LSR ROL EQ NEQ OPTEQ LTE GTE MIN MAX
%token ANDL ORL NOTL OPTNOTL
%token COMMA FOR I ICIRC IF MUN
%token MAPPED EXT FSCR FCHK TLB IPEND DEBUG MODECTL
%token SXT ZXT NEW OPTNEW ZEROONE CONSTEXT LOCNT BREV U64 SIGN
%token HASH EA PC FP GP NPC LPCFG STAREA WIDTH OFFSET SHAMT ADDR SUMR SUMI CTRL
%token TMPR TMPI X0 X1 Y0 Y1 PROD0 PROD1 TMP QMARK TRAP0 TRAP1 CAUSE EX INT NOP
%token DCKILL DCLEAN DCINVA DZEROA DFETCH ICKILL L2KILL ISYNC BRKPT SYNCHT LOCK

%token <index> SA
%token <index> LC
%token <rvalue> REG
%token <rvalue> IMM
%token <rvalue> PRE
%token <index> ELSE
%token <vec> VEC
%token <cast> CAST
%token <range> RANGE
%type <rvalue> rvalue
%type <rvalue> lvalue
%type <rvalue> VAR
%type <rvalue> assign_statement
%type <rvalue> pre
%type <rvalue> reg
%type <rvalue> DREG
%type <rvalue> DIMM
%type <rvalue> DPRE
%type <rvalue> DMOD
%type <rvalue> extra
%type <index> if_stmt
%type <index> IF
%type <is_unsigned> SIGN

/* Operator Precedences */
%left MIN MAX
%left LPAR
%right CAST
%right INT
%left COMMA
%left ASSIGN
%right CIRCADD
%right INC DEC INCDECA ANDA ORA XORA ANDORA
%left QMARK COLON
%left ORL
%left ANDL
%left OR
%left XOR ANDOR
%left AND
%left EQ NEQ OPTEQ
%left LT GT LTE GTE
%left ASL ASR LSR ROL
%right ABS CROUND
%left MINUS PLUS PMINUS
%left POW
%left MUL DIV MOD
%right NOT NOTL OPTNOTL
%left LSQ
%left NEW OPTNEW ZEROONE
%left VEC OPTSHIFT NSHIFT
%right EXT LOCNT BREV

/* Bison Grammar */
%%

/* Return the modified registers list */
code  : LBR
      {
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     "void emit_%s(DisasContext *ctx, "
                                     "insn_t *insn, packet_t *pkt",
                                     c->inst_name);
      }
      decls
      {
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     ")");
          OUT(c, "\n{");
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
          bool is64 = ($1.bit_width == 64);
          const char *type = is64 ? "TCGv_i64" : "TCGv_i32";
          char buffer[3] = { $1.reg.id, is64 ? $1.reg.id : 0 , 0 };
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     ", %s R%sV",
                                     type,
                                     buffer);
      }
      | DIMM
      {
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     ", int %ciV",
                                     $1.imm.id);
      }
      | DPRE
      {
          char suffix = $1.is_dotnew ? 'N' : 'V';
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     ", TCGv P%c%c",
                                     $1.pre.id,
                                     suffix);
      }
      | DMOD
      {
          c->signature_c += snprintf(c->signature_buffer + c->signature_c,
                                     SIGNATURE_BUF_LEN,
                                     ", TCGv M%cV, int M%cN",
                                     $1.reg.id, $1.reg.id);
      }
      | DEA
      | RREG
      | RMOD
      | WREG
      | FREG
      | FIMM
      | RPRE
      | FPRE
      | FMOD
      | FEA
;

code_block : LBR statements RBR            { /* does nothing */ }
           | LBR RBR                       { /* does nothing */ }
;

/* A list of one or more statements */
statements  : statements statement         { /* does nothing */ }
            | statement                    { /* does nothing */ }
;

// TODO: putting code_block here unexpectedly solves the dangling else problem
// check that if-then-else statements semantics are preserved
/* Statements can be assignment, control or memory statements */
statement   : control_statement            { /* does nothing */ }
            | rvalue SEMI                  { /* does nothing */ }
            | code_block                   { /* does nothing */ }
;

/* Add this to the modified registers list */
assign_statement  : lvalue ASSIGN rvalue
                  {
                    gen_assign(c, &$1, &$3);
                    $$ = $1;
                  }
                  | lvalue INC rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, ADD, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue DEC rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, SUBTRACT, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue INCDECA rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, ADDSUB, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue ANDA rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, ANDB, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue ORA rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, ORB, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue XORA rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, XORB, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | lvalue ANDORA rvalue
                  {
                    t_hex_value tmp = gen_bin_op(c, ANDORB, &$1, &$3);
                    gen_assign(c, &$1, &tmp);
                    $$ = $1;
                  }
                  | pre ASSIGN rvalue
                  {
                    /* Write to predicate register */
                    OUT(c, "int pre_index", &c->predicate_count, " = "); /* Get predicate index */
                    OUT(c, &($1.pre.id), ";\n");
                    rvalue_truncate(c, &$3);
                    rvalue_materialize(c, &$3);
                    if (!c->no_track_regs) {
                        OUT(c, "TCGv p_reg", &c->p_reg_count);
                        OUT(c, " = (GET_WRITTEN_ANY_PRE(dc)) ? ");
                        OUT(c, "CR_new[CR_P] : CR[CR_P];\n");
                    }
                    /* Bitwise predicate assignment */
                    if ($1.pre.is_bit_iter) {
                         /* Extract lsb */
                         OUT(c, "tcg_gen_andi_i32(", &$3, ", ", &$3, ", 1);\n");
                         /* Shift to reach predicate and bit offset */
                         OUT(c, "tcg_gen_shli_i32(", &$3, ", ", &$3, ", 8 * pre_index",
                             &c->predicate_count, " + i);\n");
                         /* Clear previous predicate value */
                         t_hex_value mask = gen_tmp(c, 32);
                         t_hex_value one = gen_tmp_value(c, "1", 32);
                         OUT(c, "tcg_gen_shli_i32(", &mask);
                         OUT(c, ", ", &one, ", 8 * pre_index", &c->predicate_count, " + i);\n");
                         OUT(c, "tcg_gen_not_i32(", &mask, ", ", &mask, ");\n");
                         OUT(c, "tcg_gen_and_i32(CR_new[CR_P], p_reg", &c->p_reg_count, ", ",
                              &mask, ");\n");
                         c->p_reg_count++;
                         /* Store new predicate value */
                         if (c->no_track_regs) {
                            if ($3.type == IMMEDIATE)
                                OUT(c, "tcg_gen_ori_i32(CR[CR_P], CR[CR_P], ", &$3, ");\n");
                            else
                                OUT(c, "tcg_gen_or_i32(CR[CR_P], CR[CR_P], ", &$3, ");\n");
                         } else {
                            if ($3.type == IMMEDIATE)
                                OUT(c, "tcg_gen_ori_i32(CR_new[CR_P], CR_new[CR_P], ", &$3, ");\n");
                            else
                                OUT(c, "tcg_gen_or_i32(CR_new[CR_P], CR_new[CR_P], ", &$3, ");\n");
                         }
                    /* Range-based predicate assignment */
                    } else if ($1.is_range) {
                        /* (bool) ? 0xff : 0x00 */
                        t_hex_value tmp = gen_tmp(c, 32);
                        t_hex_value zero = gen_tmp_value(c, "0x0", 32);
                        t_hex_value ff = gen_tmp_value(c, "0xff", 32);
                        OUT(c, "tcg_gen_movcond_i32");
                        OUT(c, "(TCG_COND_EQ, ", &tmp, ", ", &$3, ", ", &zero);
                        OUT(c, ", ", &zero, ", ", &ff, ");\n");
                        /* Deposit into range */
                        int begin = $1.range.begin;
                        int end = $1.range.end;
                        int width = end - begin + 1;
                        OUT(c, "tcg_gen_deposit_i32(CR_new[CR_P], p_reg");
                        OUT(c, &c->p_reg_count, ", ");
                        c->p_reg_count++;
                        OUT(c, &tmp, ", 8 * pre_index", &c->predicate_count, " + ");
                        OUT(c, &begin, ", ", &width, ");\n");
                        rvalue_free(c, &zero);
                        rvalue_free(c, &ff);
                        rvalue_free(c, &tmp);
                    /* Standard bytewise predicate assignment */
                    } else {
                         
                         /* Extract first 8 bits */
                         OUT(c, "tcg_gen_andi_i32(", &$3, ", ", &$3, ", 0xff);\n");
                         /* Shift to reach predicate */
                         OUT(c, "tcg_gen_shli_i32(", &$3, ", ", &$3, ", 8 * pre_index",
                             &c->predicate_count, ");\n");
                        if (!c->no_track_regs) {
                            /* If predicate was already assigned just perform
                               the logical AND between the two assignments */
                            OUT(c, "if (GET_WRITTEN_PREV_PRE(dc, pre_index0)) {");
                            /* Filter out other predicate bytes */
                            OUT(c, "tcg_gen_ori_i32(", &$3, ", ", &$3, ", ");
                            OUT(c, "pred_whitening[pre_index", &c->predicate_count, "]);\n");
                            if ($3.type == IMMEDIATE)
                                OUT(c, "tcg_gen_andi_i32(CR_new[CR_P], p_reg", &c->p_reg_count, ", ", &$3, ");\n");
                            else
                                OUT(c, "tcg_gen_and_i32(CR_new[CR_P], p_reg", &c->p_reg_count, ", ", &$3, ");\n");
                            /* Otherwise replace the old value completely */
                            OUT(c, "} else {\n");
                            /* Clear previous predicate value */
                            OUT(c, "tcg_gen_andi_i32(CR_new[CR_P], p_reg", &c->p_reg_count, ", "
                                    "pred_whitening[pre_index", &c->predicate_count, "]);\n");
                            c->p_reg_count++;
                            /* Store new predicate value */
                            if ($3.type == IMMEDIATE)
                                OUT(c, "tcg_gen_ori_i32(CR_new[CR_P], CR_new[CR_P], ", &$3, ");\n");
                            else
                                OUT(c, "tcg_gen_or_i32(CR_new[CR_P], CR_new[CR_P], ", &$3, ");\n");
                                OUT(c, "}\n");
                        /* endloop0 special handling */
                        } else {
                            /* Clear previous predicate value */
                            OUT(c, "tcg_gen_andi_i32(CR[CR_P], CR[CR_P], "
                                "pred_whitening[pre_index", &c->predicate_count, "]);\n");
                            /* Store new predicate value */
                            if ($3.type == IMMEDIATE)
                                OUT(c, "tcg_gen_ori_i32(CR[CR_P], CR[CR_P], ", &$3, ");\n");
                            else
                                OUT(c, "tcg_gen_or_i32(CR[CR_P], CR[CR_P], ", &$3, ");\n");
                        }
                    }
                    if (!c->no_track_regs) {
                        OUT(c, "SET_WRITTEN_PRE(dc, pre_index", &c->predicate_count, ");\n");
                    }
                    rvalue_free(c, &$3);  /* Free temporary value */
                    c->predicate_count++;
                    $$ = $1;
                  }
                  | IMM ASSIGN rvalue
                  {
                    yyassert(c, $3.type == IMMEDIATE,
                           "Cannot assign non-immediate to immediate!");
                    yyassert(c, $1.imm.type == VARIABLE,
                           "Cannot assign to non-variable!");
                    /* Assign to the function argument */
                    OUT(c, &$1, " = ", &$3, ";\n");
                    $$ = $1;
                  }
                  | PC ASSIGN rvalue
                  {
                    rvalue_materialize(c, &$3);
                    OUT(c, "gen_write_new_pc(", &$3, ");\n");
                    rvalue_free(c, &$3); /* Free temporary value */
                  }
                  | STAREA ASSIGN rvalue /* Store primitive */
                  {
                    /* Select memop width according to rvalue bit width */
                    int mem_width = ($3.is_vectorial) ? $3.vec.width/8 :
                        $3.bit_width/8;
                    rvalue_materialize(c, &$3);
                    OUT(c, "gen_store", &mem_width, "(cpu_env, EA, ", &$3);
                    OUT(c, ", ctx, insn->slot);\n");
                    rvalue_free(c, &$3);
                  }
                  | CAUSE ASSIGN IMM
                  {
                    /* TODO: Sync PC and flags between translator and runtime */
                  }
                  | EX ASSIGN IMM
                  {
                    /* TODO: Implement exception register */
                  }
                  | LOCK ASSIGN IMM
                  {
                    /* Do nothing since multithread lock is not implemented */
                  }
;

control_statement : frame_check          { /* does nothing */ }
                  | ckill_statement      { /* does nothing */ }
                  | tlb_write            { /* does nothing */ }
                  | clear_interrupts     { /* does nothing */ }
                  | stop_statement       { /* does nothing */ }
                  | trap_statement       { /* does nothing */ }
                  | if_statement         { /* does nothing */ }
                  | for_statement        { /* does nothing */ }
                  | ISYNC SEMI           { /* does nothing */ }
                  | BRKPT SEMI           { /* does nothing */ }
                  | SYNCHT SEMI          { /* does nothing */ }
                  | NOP SEMI             { /* does nothing */ }
                  | PREDUSE SEMI         { /* does nothing */ }
                  | SEMI                 { /* does nothing */ }
;

frame_check       : FCHK LPAR rvalue RPAR SEMI  { /* does nothing */ }
;

ckill_statement  : DCKILL LPAR RPAR SEMI        { /* does nothing */ }
                 | ICKILL LPAR RPAR SEMI        { /* does nothing */ }
                 | L2KILL LPAR RPAR SEMI        { /* does nothing */ }
                 | DCLEAN LPAR rvalue RPAR SEMI { /* does nothing */ }
                 | DCINVA LPAR rvalue RPAR SEMI { /* does nothing */ }
                 | DZEROA LPAR rvalue RPAR SEMI { /* does nothing */ }
                 | DFETCH LPAR rvalue RPAR SEMI { /* does nothing */ }
;

tlb_write        : TLB LSQ rvalue RSQ ASSIGN rvalue SEMI
                 {
                    /* We are not emulating the TLB, since we are
                       only performing userspace emulation */
                 }
;

clear_interrupts : IPEND ANDA rvalue SEMI { /* does nothing */ }
;

stop_statement : IF DEBUG MODECTL ASSIGN IMM SEMI { /* does nothing */ }
;

trap_statement    : TRAP0 SEMI
                  {
                    /* TODO: Implement trap statement */
                    t_hex_value tmp = gen_tmp_value(c, "j", 32);
                    /* Put next program counter in ELR register */
                    OUT(c, "tcg_gen_movi_i32(SR[3], dc->pc + 4);\n");
                    /* Jump to interrupt handler */
                    t_hex_value handler_pc = gen_tmp(c, 32);
                    OUT(c, "tcg_gen_addi_i32(", &handler_pc, ", SR[16], 0x1c);\n");
                    OUT(c, "tcg_gen_mov_i32(CR[CR_PC], ", &handler_pc, ");\n");
                    OUT(c, "gen_helper_handle_trap(cpu_env, ", &tmp, ");\n");
                    rvalue_free(c, &tmp);
                  }
                  | TRAP1 SEMI
                  {
                    t_hex_value tmp = gen_tmp_value(c, "j", 32);
                    /* Put next program counter in ELR register */
                    OUT(c, "tcg_gen_movi_i32(SR[3], dc->pc + 4);\n");
                    /* Jump to interrupt handler */
                    t_hex_value handler_pc = gen_tmp(c, 32);
                    OUT(c, "tcg_gen_addi_i32(", &handler_pc, ", SR[16], 0x20);\n");
                    OUT(c, "tcg_gen_mov_i32(CR[CR_PC], ", &handler_pc, ");\n");
                    OUT(c, "gen_helper_handle_trap(cpu_env, ", &tmp, ");\n");
                    rvalue_free(c, &tmp);
                  }
;

if_statement : if_stmt
             {
                /* Fix else label */
               OUT(c, "gen_set_label(if_label_", &$1, ");\n");
             }
             | if_stmt ELSE
             {
               /* Generate label to jump if else is not verified */
               OUT(c, "TCGLabel *if_label_", &c->if_count, " = gen_new_label();\n");
               $2 = c->if_count;
               c->if_count++;
               /* Jump out of the else statement */
               OUT(c, "tcg_gen_br(if_label_", &$2, ");\n");
               /* Fix the else label */
               OUT(c, "gen_set_label(if_label_", &$1, ");\n");
             }
             code_block
             {
               OUT(c, "gen_set_label(if_label_", &$2, ");\n");
             }
;

for_statement : FOR LPAR I ASSIGN IMM SEMI I LT IMM SEMI I PLUSPLUS RPAR
              {
                OUT(c, "for(int i = ", &$5, "; i < ", &$9, "; i++) {\n");
              }
              code_block
              {
                OUT(c, "}\n");
              }
;

for_statement : FOR LPAR I ASSIGN IMM SEMI I LT IMM SEMI I INC IMM RPAR
              {
                OUT(c, "for(int i = ", &$5, "; i < ", &$9, "; i += ", &$13, ") {\n");
              }
              code_block
              {
                OUT(c, "}\n");
              }
;

if_stmt      : IF
             {
               /* Generate an end label, if false branch to that label */
               OUT(c, "TCGLabel *if_label_", &c->if_count, " = gen_new_label();\n");
             }
             LPAR rvalue RPAR
             {
               rvalue_materialize(c, &$4);
               char *bit_suffix = ($4.bit_width == 64) ? "i64" : "i32";
               OUT(c, "tcg_gen_brcondi_", bit_suffix, "(TCG_COND_EQ, ", &$4,
                   ", 0, if_label_", &c->if_count, ");\n");
               rvalue_free(c, &$4);
               $1 = c->if_count;
               c->if_count++;
             }
             code_block
             {
               $$ = $1;
             }
;

rvalue            : assign_statement            { /* does nothing */ }
                  | reg
                  {
                    $$ = gen_extract(c, &$1);
                  }
                  | IMM
                  {
                    $$ = $1;
                  }
                  | extra
                  {
                    $$ = gen_extract(c, &$1);
                  }
                  | pre
                  {
                    $$ = $1;
                  }
                  | PC
                  {
                    /* TODO: get PC as immediate from DisasContext */
                    //t_hex_value rvalue;
                    //rvalue.type = IMMEDIATE;
                    //rvalue.imm.type = IMM_PC;
                    //rvalue.is_unsigned = true;
                    //rvalue.is_dotnew = false;
                    //rvalue.is_vectorial = false;
                    //rvalue.is_range = false;
                    //rvalue.is_symbol = false;
                    //$$ = rvalue;
                    /* Meanwhile we are reading it from the CR */
                    $$ = gen_tmp(c, 32);
                    OUT(c, "tcg_gen_mov_i32(", &$$, ", hex_gpr[HEX_REG_PC]);\n");
                  }
                  | NPC
                  {
                    /* Extract program counter into a temporary */
                    $$ = gen_tmp(c, 32);
                    t_hex_value pc = gen_tmp_value(c, "dc->npc", 32);
                    OUT(c, "tcg_gen_mov_i32(", &$$, ", ", &pc, ");\n");
                  }
                  | CONSTEXT
                  {
                    t_hex_value rvalue;
                    rvalue.type = IMMEDIATE;
                    rvalue.imm.type = IMM_CONSTEXT;
                    rvalue.is_unsigned = true;
                    rvalue.is_dotnew = false;
                    rvalue.is_vectorial = false;
                    rvalue.is_range = false;
                    rvalue.is_symbol = false;
                    $$ = rvalue;
                  }
                  | VAR
                  {
                    $$ = $1;
                  }
                  | rvalue PLUS rvalue
                  {
                    $$ = gen_bin_op(c, ADD, &$1, &$3);
                  }
                  | rvalue MINUS rvalue
                  {
                    $$ = gen_bin_op(c, SUBTRACT, &$1, &$3);
                  }
                  | rvalue PMINUS rvalue
                  {
                    $$ = gen_bin_op(c, ADDSUB, &$1, &$3);
                  }
                  | rvalue MUL rvalue
                  {
                    $$ = gen_bin_op(c, MULTIPLY, &$1, &$3);
                  }
                  | rvalue POW rvalue
                  {
                    /* We assume that this is a shorthand for a shift */
                    yyassert(c, $1.type == IMMEDIATE && $1.imm.value == 2,
                           "Exponentiation is not a left shift!\n");
                    t_hex_value one = gen_imm_value(c, 1, 32);
                    t_hex_value shift = gen_bin_op(c, SUBTRACT, &$3, &one);
                    $$ = gen_bin_op(c, ASHIFTL, &$1, &shift);
                    rvalue_free(c, &one);
                    rvalue_free(c, &shift);
                  }
                  | rvalue DIV rvalue
                  {
                    $$ = gen_bin_op(c, DIVIDE, &$1, &$3);
                  }
                  | rvalue MOD rvalue
                  {
                    $$ = gen_bin_op(c, MODULO, &$1, &$3);
                  }
                  | rvalue ASL rvalue
                  {
                    $$ = gen_bin_op(c, ASHIFTL, &$1, &$3);
                  }
                  | rvalue ASR rvalue
                  {
                    $$ = gen_bin_op(c, ASHIFTR, &$1, &$3);
                  }
                  | rvalue LSR rvalue
                  {
                    $$ = gen_bin_op(c, LSHIFTR, &$1, &$3);
                  }
                  | rvalue ROL rvalue
                  {
                    $$ = gen_bin_op(c, ROTATE, &$1, &$3);
                  }
                  | rvalue AND rvalue
                  {
                    $$ = gen_bin_op(c, ANDB, &$1, &$3);
                  }
                  | rvalue OR rvalue
                  {
                    $$ = gen_bin_op(c, ORB, &$1, &$3);
                  }
                  | rvalue ANDOR rvalue
                  {
                    $$ = gen_bin_op(c, ANDORB, &$1, &$3);
                  }
                  | rvalue XOR rvalue
                  {
                    $$ = gen_bin_op(c, XORB, &$1, &$3);
                  }
                  | MIN LPAR rvalue COMMA rvalue RPAR
                  {
                    $$ = gen_bin_op(c, MINI, &$3, &$5);
                  }
                  | MAX LPAR rvalue COMMA rvalue RPAR
                  {
                    $$ = gen_bin_op(c, MAXI, &$3, &$5);
                  }
                  | NOT rvalue
                  {
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_vectorial = false;
                    res.is_range = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, "int", &bit_width, "_t ", &res, " = ~", &$2, ";\n");
                        c->qemu_tmp_count++;
                    } else {
                        res = gen_tmp(c, bit_width);
                        OUT(c, "tcg_gen_not_", bit_suffix, "(", &res,
                            ", ", &$2, ");\n");
                        rvalue_free(c, &$2);
                    }
                    $$ = res;
                  }
                  | NOTL rvalue
                  {
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_vectorial = false;
                    res.is_range = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, "int", &bit_width, "_t ", &res, " = !", &$2, ";\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, bit_width);
                        t_hex_value zero = gen_tmp_value(c, "0", bit_width);
                        t_hex_value one = gen_tmp_value(c, "0xff", bit_width);
                        OUT(c, "tcg_gen_movcond_", bit_suffix);
                        OUT(c, "(TCG_COND_EQ, ", &res, ", ", &$2, ", ", &zero);
                        OUT(c, ", ", &one, ", ", &zero, ");\n");
                        rvalue_free(c, &$2);
                        rvalue_free(c, &zero);
                        rvalue_free(c, &one);
                        $$ = res;
                    }
                  }
                  | OPTNOTL rvalue
                  {
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    OUT(c, "if (not", &c->not_count, ") {\n");
                    t_hex_value zero = gen_tmp_value(c, "0", 32);
                    t_hex_value one = gen_tmp_value(c, "0xff", 32);
                    OUT(c, "tcg_gen_movcond_", bit_suffix);
                    OUT(c, "(TCG_COND_EQ, ", &$2, ", ", &$2, ", ", &zero);
                    OUT(c, ", ", &one, ", ", &zero, ");\n");
                    rvalue_free(c, &zero);
                    rvalue_free(c, &one);
                    OUT(c, "}\n");
                    c->not_count++;
                    $$ = $2;
                  }
                  | VEC rvalue
                  {
                    $2.vec = $1;
                    $$ = $2;
                  }
                  | CAST rvalue
                  {
                    /* Assign target signedness */
                    $2.is_unsigned = $1.is_unsigned;
                    $$ = gen_cast_op(c, &$2, $1.width);
                  }
                  | LPAR rvalue RPAR VEC
                  {
                    $2.vec = $4;
                    $$ = $2;
                  }
                  | rvalue LSQ rvalue RSQ
                  {
                    t_hex_value one = gen_imm_value(c, 1, $3.bit_width);
                    t_hex_value tmp = gen_bin_op(c, ASHIFTR, &$1, &$3);
                    $$ = gen_bin_op(c, ANDB, &tmp, &one);
                  }
                  | rvalue EQ rvalue
                  {
                    $$ = gen_bin_cmp(c, EQ_OP, &$1, &$3);
                  }
                  | rvalue NEQ rvalue
                  {
                    $$ = gen_bin_cmp(c, NEQ_OP, &$1, &$3);
                  }
                  | rvalue OPTEQ rvalue
                  {
                    $$ = gen_bin_cmp(c, OPTEQ_OP, &$1, &$3);
                  }
                  | rvalue LT rvalue
                  {
                    if ($1.is_unsigned && $3.is_unsigned)
                        $$ = gen_bin_cmp(c, LTU_OP, &$1, &$3);
                    else if (!$1.is_unsigned && !$3.is_unsigned)
                        $$ = gen_bin_cmp(c, LT_OP, &$1, &$3);
                    else {
                        if (c->mem_unsigned)
                            $$ = gen_bin_cmp(c, LTU_OP, &$1, &$3);
                        else
                            $$ = gen_bin_cmp(c, LT_OP, &$1, &$3);
                    }
                  }
                  | rvalue GT rvalue
                  {
                    if ($1.is_unsigned && $3.is_unsigned)
                        $$ = gen_bin_cmp(c, GTU_OP, &$1, &$3);
                    else if (!$1.is_unsigned && !$3.is_unsigned)
                        $$ = gen_bin_cmp(c, GT_OP, &$1, &$3);
                    else {
                        if (c->mem_unsigned)
                            $$ = gen_bin_cmp(c, GTU_OP, &$1, &$3);
                        else
                            $$ = gen_bin_cmp(c, GT_OP, &$1, &$3);
                    }
                  }
                  | rvalue LTE rvalue
                  {
                    if ($1.is_unsigned && $3.is_unsigned)
                        $$ = gen_bin_cmp(c, LEU_OP, &$1, &$3);
                    else if (!$1.is_unsigned && !$3.is_unsigned)
                        $$ = gen_bin_cmp(c, LTE_OP, &$1, &$3);
                    else {
                        if (c->mem_unsigned)
                            $$ = gen_bin_cmp(c, LEU_OP, &$1, &$3);
                        else
                            $$ = gen_bin_cmp(c, LTE_OP, &$1, &$3);
                    }
                  }
                  | rvalue GTE rvalue
                  {
                    if ($1.is_unsigned && $3.is_unsigned)
                        $$ = gen_bin_cmp(c, GEU_OP, &$1, &$3);
                    else if (!$1.is_unsigned && !$3.is_unsigned)
                        $$ = gen_bin_cmp(c, GTE_OP, &$1, &$3);
                    else {
                        if (c->mem_unsigned)
                            $$ = gen_bin_cmp(c, GEU_OP, &$1, &$3);
                        else
                            $$ = gen_bin_cmp(c, GTE_OP, &$1, &$3);
                    }
                  }
                  | rvalue QMARK rvalue COLON rvalue
                  {
                    bool is_64bit = ($3.bit_width == 64) || ($5.bit_width == 64);
                    int bit_width = (is_64bit) ? 64 : 32;
                    if (is_64bit) {
                        rvalue_extend(c, &$1);
                        rvalue_extend(c, &$3);
                        rvalue_extend(c, &$5);
                    } else {
                        rvalue_truncate(c, &$1);
                    }
                    rvalue_materialize(c, &$1);
                    rvalue_materialize(c, &$3);
                    rvalue_materialize(c, &$5);
                    t_hex_value res = gen_local_tmp(c, bit_width);
                    t_hex_value zero = gen_tmp_value(c, "0", bit_width);
                    OUT(c, "tcg_gen_movcond_i", &bit_width);
                    OUT(c, "(TCG_COND_NE, ", &res, ", ", &$1, ", ", &zero);
                    OUT(c, ", ", &$3, ", ", &$5, ");\n");
                    rvalue_free(c, &zero);
                    rvalue_free(c, &$1);
                    rvalue_free(c, &$3);
                    rvalue_free(c, &$5);
                    $$ = res;
                  }
                  | FSCR LPAR rvalue RPAR
                  {
                    t_hex_value key = gen_tmp(c, 64);
                    t_hex_value res = gen_tmp(c, 64);
                    rvalue_extend(c, &$3);
                    OUT(c, "tcg_gen_concat_i32_i64(", &key,", CR[17], CR[17]);\n");
                    OUT(c, "tcg_gen_xor_i64(", &res, ", ", &$3,", ", &key, ");\n");
                    $$ = res;
                  }
                  | SXT IMM LARR IMM LPAR rvalue RPAR
                  {
                    /* Handle weird destination widths */
                    if ($4.imm.value > 32)
                        $4.imm.value = 64;
                    /* 32 bit constants are already sign extended */
                    if ($4.imm.value == 32)
                        $$ = $6;
                    else if ($4.imm.value == 64) {
                        $6.is_unsigned = false;
                        rvalue_extend(c, &$6);
                    } else
                        yyassert(c, false, "Unhandled destination bit width!");
                    $$ = $6;
                  }
                  | ZXT USCORE LBR rvalue LARR IMM RBR LPAR rvalue RPAR
                  {
                    $$ = gen_zxt_op(c, &$9, &$4, &$6);
                  }
                  | EXT LPAR IMM RPAR
                  {
                    $$ = $3;
                  }
                  | INT rvalue
                  {
                    $$ = $2;
                  }
                  | LPAR rvalue SIGN RPAR STAREA /* Load primitive */
                  {
                    /* memop width and sign are propagated from instruction description */
                    int bit_width = ($2.imm.value > 4) ? 64 : 32;
                    char *sign_suffix = ($2.imm.value > 4) ? "" : (($3) ? "u" : "s");
                    char *helper_suffix = ($3) ? "u" : "s";
                    char size_suffix[4] = { 0 };
                    snprintf(size_suffix, 4, "%" PRIu64, $2.imm.value * 8);
                    t_hex_value tmp = gen_tmp(c, bit_width);
                    OUT(c, "tcg_gen_qemu_ld", size_suffix, sign_suffix);
                    OUT(c, "(", &tmp, ", EA, 0);\n");
                    OUT(c, "if (insn->slot == 0 && pkt->pkt_has_store_s1) {\n");
                    OUT(c, "gen_helper_merge_inflight_store", &$2.imm.value);
                    OUT(c, helper_suffix, "(", &tmp, ", cpu_env, EA, ", &tmp, ");\n");
                    OUT(c, "}\n");
                    tmp.is_unsigned = $3;
                    $$ = tmp;
                  }
                  | LPAR rvalue RPAR
                  {
                    $$ = $2;
                  }
                  | ABS rvalue
                  {
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_vectorial = false;
                    res.is_range = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, "int", &bit_width, "_t ", &res, " = abs(", &$2, ");\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, bit_width);
                        t_hex_value zero = gen_tmp_value(c, "0", bit_width);
                        OUT(c, "tcg_gen_neg_", bit_suffix, "(", &res, ", ",
                            &$2, ");\n");
                        OUT(c, "tcg_gen_movcond_i", &bit_width);
                        OUT(c, "(TCG_COND_GT, ", &res, ", ", &$2, ", ", &zero);
                        OUT(c, ", ", &$2, ", ", &res, ");\n");
                        rvalue_free(c, &$2);
                        $$ = res;
                    }
                  }
                  | CROUND LPAR rvalue COMMA rvalue RPAR
                  {
                    $$ = gen_convround(c, &$3, &$5);
                  }
                  | CROUND LPAR rvalue RPAR
                  {
                    /* When is not specified assume mask = 1 */
                    t_hex_value one = gen_imm_value(c, 1, 32);
                    $$ = gen_convround(c, &$3, &one);
                  }
                  | ROUND LPAR rvalue COMMA rvalue RPAR
                  {
                    /* Add .5 only if .5 bit is set */
                    yyassert(c, $3.bit_width <= 32,
                           "Convround not implemented for bit widths > 32!");
                    t_hex_value one = gen_imm_value(c, 1, 32);
                    t_hex_value remainder = gen_bin_op(c, ANDB, &$3, &$5);
                    t_hex_value res = gen_bin_op(c, ADD, &$3, &remainder);
                    /* Zero out trailing bits */
                    t_hex_value mask = gen_bin_op(c, ASHIFTL, &$5, &one);
                    mask = gen_bin_op(c, SUBTRACT, &mask, &one);
                    rvalue_materialize(c, &mask);
                    OUT(c, "tcg_gen_not_i32(", &mask, ", ", &mask, ");\n");
                    res = gen_bin_op(c, ANDB, &res, &mask);
                    rvalue_free(c, &$3);
                    rvalue_free(c, &$5);
                    $$ = res;
                  }
                  | MINUS rvalue
                  {
                    char * bit_suffix = ($2.bit_width == 64) ? "i64" : "i32";
                    int bit_width = ($2.bit_width == 64) ? 64 : 32;
                    t_hex_value res;
                    res.is_unsigned = $2.is_unsigned;
                    res.is_dotnew = false;
                    res.is_vectorial = false;
                    res.is_range = false;
                    res.is_symbol = false;
                    if ($2.type == IMMEDIATE) {
                        res.type = IMMEDIATE;
                        res.imm.type = QEMU_TMP;
                        res.imm.index = c->qemu_tmp_count;
                        OUT(c, "int", &bit_width, "_t ", &res, " = -", &$2, ";\n");
                        c->qemu_tmp_count++;
                        $$ = res;
                    } else {
                        res = gen_tmp(c, bit_width);
                        OUT(c, "tcg_gen_neg_", bit_suffix, "(", &res, ", ",
                            &$2, ");\n");
                        rvalue_free(c, &$2);
                        $$ = res;
                    }
                  }
                  | rvalue OPTSHIFT
                  {
                    char * bit_suffix = ($1.bit_width == 64) ? "i64" : "i32";
                    OUT(c, "if (opt_shift) {\n");
                    OUT(c, "tcg_gen_shli_", bit_suffix, "(", &$1, ", ", &$1, ", 1);\n");
                    OUT(c, "}\n");
                  }
                  | rvalue NSHIFT
                  {
                    t_hex_value N = gen_tmp_value(c, "N", 32);
                    $$ = gen_bin_op(c, ASHIFTL, &$1, &N);
                  }
                  | CIRCADD LPAR rvalue COMMA rvalue COMMA rvalue RPAR
                  {
                    $$ = gen_circ_op(c, &$3, &$5, &$7);
                  }
                  | CIRCADD LPAR rvalue COMMA ICIRC ASL IMM COMMA rvalue RPAR
                  {
                    t_hex_value I = gen_tmp(c, 32);
                    OUT(c, "tcg_gen_extract_i32(", &I, ", ", &$9, ", 17, 7);\n");
                    I = gen_bin_op(c, ASHIFTL, &I, &$7);
                    $$ = gen_circ_op(c, &$3, &I, &$9);
                  }
                  | LOCNT LPAR rvalue RPAR
                  {
                    /* Leading ones count */
                    $$ = gen_bitcnt_op(c, &$3, true, false);
                  }
                  | LOCNT LPAR BREV LPAR rvalue RPAR RPAR
                  {
                    /* Trailing ones count */
                    $$ = gen_bitcnt_op(c, &$5, true, true);
                  }
                  | LOCNT LPAR NOT BREV LPAR rvalue RPAR RPAR
                  {
                    /* Trailing zeroes count */
                    $$ = gen_bitcnt_op(c, &$6, false, true);
                  }
                  | LOCK
                  {
                    $$ = gen_tmp_value(c, "true", 32);
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
                  | pre VEC
                  {
                    yyassert(c, $2.width == 1, "Not-bitwise access to predicate!");
                    $$ = $1;
                    $$.pre.is_bit_iter = true;
                  }
                  | pre RANGE
                  {
                    $$ = $1;
                    $$.range = $2;
                    $$.is_range = true;
                  }
;

lvalue            : reg
                  {
                    $$ = $1;
                  }
                  | extra
                  {
                    $$ = $1;
                  }
                  | VAR
                  {
                    $$ = $1;
                  }
;

reg               : REG
                  {
                    $$ = $1;
                  }
                  | reg NEW
                  {
                    $$ = $1;
                    $$.is_dotnew = true;
                  }
                  | reg VEC
                  {
                    $$ = $1;
                    $$.vec = $2;
                    $$.is_vectorial = true;
                    $$.is_unsigned = $2.is_unsigned;
                  }
                  | reg RANGE
                  {
                    $$ = $1;
                    $$.range = $2;
                    $$.is_range = true;
                  }
                  | reg U64
                  {
                    $$ = $1;
                    $$.is_unsigned = true;
                  }
;

extra             : LPCFG
                  {
                    $$ = gen_extra(c, LPCFG_T, 0, false);
                  }
                  | LC
                  {
                    $$ = gen_extra(c, LC_T, $1, false);
                  }
                  | SA
                  {
                    $$ = gen_extra(c, SA_T, $1, false);
                  }
                  | EA
                  {
                    $$ = gen_extra(c, EA_T, 0, true);
                  }
                  | WIDTH
                  {
                    $$ = gen_extra(c, WIDTH_T, 0, true);
                  }
                  | OFFSET
                  {
                    $$ = gen_extra(c, OFFSET_T, 0, true);
                  }
                  | SHAMT
                  {
                    $$ = gen_extra(c, SHAMT_T, 0, true);
                  }
                  | ADDR
                  {
                    $$ = gen_extra(c, ADDR_T, 0, true);
                  }
                  | SUMR
                  {
                    $$ = gen_extra(c, SUMR_T, 0, true);
                  }
                  | SUMI
                  {
                    $$ = gen_extra(c, SUMI_T, 0, true);
                  }
                  | CTRL
                  {
                    $$ = gen_extra(c, CTRL_T, 0, true);
                  }
                  | TMPR
                  {
                    $$ = gen_extra(c, TMPR_T, 0, true);
                  }
                  | TMPI
                  {
                    $$ = gen_extra(c, TMPI_T, 0, true);
                  }
                  | X0
                  {
                    $$ = gen_extra(c, X0_T, 0, true);
                  }
                  | X1
                  {
                    $$ = gen_extra(c, X1_T, 0, true);
                  }
                  | Y0
                  {
                    $$ = gen_extra(c, Y0_T, 0, true);
                  }
                  | Y1
                  {
                    $$ = gen_extra(c, Y1_T, 0, true);
                  }
                  | PROD0
                  {
                    $$ = gen_extra(c, PROD0_T, 0, true);
                  }
                  | PROD1
                  {
                    $$ = gen_extra(c, PROD1_T, 0, true);
                  }
                  | MAX
                  {
                    $$ = gen_extra(c, MAX_T, 0, true);
                  }
                  | MIN
                  {
                    $$ = gen_extra(c, MIN_T, 0, true);
                  }
                  | TMP
                  {
                    $$ = gen_extra(c, TMP_T, 0, true);
                  }
                  | extra VEC
                  {
                    $$ = $1;
                    $$.vec = $2;
                    $$.is_vectorial = true;
                    $$.is_unsigned = $2.is_unsigned;
                  }
                  | I
                  {
                    $$ = gen_tmp_value(c, "i", 32);
                  }
;

%%

#ifndef TOKEN_DEBUG
void emit_header(context_t *c) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "/* %s */\n", c->inst_name);
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "/* %s */\n", c->inst_code);
}

void emit_footer(context_t *c) {
    c->out_c += snprintf(c->out_buffer+c->out_c, OUT_BUF_LEN, "}\n");
}

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
        // TODO: Set here sane defaults for all the instruction modifiers
        context.mem_size = MEM_DOUBLE;
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
            context.out_c += snprintf(context.out_buffer+context.out_c, OUT_BUF_LEN,
                "assert(false && \"This instruction is not implemented!\");");
        } else {
            implemented_insn++;
            emit_footer(&context);
            commit(&context);
        }
        context.inst_index++;
        /* Cleanup */
        yylex_destroy(context.scanner);
        CsvParser_destroy_row(row);
        free(in_buffer);
        free(out_buffer);
        free(signature_buffer);
    }
    CsvParser_destroy(csvparser);
    fprintf(stderr, "%d/%d meta instructions have been implemented!\n", implemented_insn, total_insn);
    fputs("#endif /* TCG_AUTO_GEN */\n", defines_file);
    fclose(defines_file);
    return 0;
}
#endif
