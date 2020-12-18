/*
 * Copyright(c) 2019-2020 rev.ng Srls. All Rights Reserved.
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

#ifndef PARSER_HELPERS_H
#define PARSER_HELPERS_H

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "idef-parser.tab.h"
#include "idef-parser.yy.h"
#include "parser-helpers.h"
#include "idef-parser.h"

/* Decomment this to disable yyasserts */
/* #define NDEBUG */

#define ERR_LINE_CONTEXT 40

#define START_COMMENT "/" "*"
#define END_COMMENT "*" "/"

void yyerror(YYLTYPE *locp,
             yyscan_t scanner __attribute__((unused)),
             context_t *c,
             const char *s);

#ifndef NDEBUG
#define yyassert(context, locp, condition, msg) \
    if (!(condition)) { \
        yyerror(locp, (context)->scanner, (context), (msg)); \
    }
#endif

bool is_direct_predicate(t_hex_value *value);

/* Print functions */
void str_print(context_t *c, YYLTYPE *locp, char *string);

void uint64_print(context_t *c, YYLTYPE *locp, uint64_t *num);

void int_print(context_t *c, YYLTYPE *locp, int *num);

void tmp_print(context_t *c, YYLTYPE *locp, t_hex_tmp *tmp);

void pre_print(context_t *c, YYLTYPE *locp, t_hex_pre *pre, bool is_dotnew);

void reg_compose(context_t *c, YYLTYPE *locp, t_hex_reg *reg, char reg_id[5]);

void reg_print(context_t *c, YYLTYPE *locp, t_hex_reg *reg);

void imm_print(context_t *c, YYLTYPE *locp, t_hex_imm *imm);

void var_print(context_t *c, YYLTYPE *locp, t_hex_var *var);

void rvalue_out(context_t *c, YYLTYPE *locp, void *pointer);

/* Copy output code buffer into stdout */
void commit(context_t *c);

#define OUT_IMPL(c, locp, x)                                            \
    do {                                                                \
        if (__builtin_types_compatible_p (typeof (*x), char)) {         \
            str_print((c), (locp), (char *) x);                         \
        } else if (__builtin_types_compatible_p (typeof (*x), uint64_t)) { \
            uint64_print((c), (locp), (uint64_t *) x);                  \
        } else if (__builtin_types_compatible_p (typeof (*x), int)) {   \
            int_print((c), (locp), (int *) x);                          \
        } else if (__builtin_types_compatible_p (typeof (*x), t_hex_value)) { \
            rvalue_out((c), (locp), (t_hex_value *) x);                 \
        } else {                                                        \
                yyassert(c, locp, false, "Unhandled print type!");      \
        }                                                               \
    } while (0);

/* Make a FOREACH macro */
#define FE_1(c, locp, WHAT, X) WHAT(c, locp, X)
#define FE_2(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_1(c, locp, WHAT, __VA_ARGS__)
#define FE_3(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_2(c, locp, WHAT, __VA_ARGS__)
#define FE_4(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_3(c, locp, WHAT, __VA_ARGS__)
#define FE_5(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_4(c, locp, WHAT, __VA_ARGS__)
#define FE_6(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_5(c, locp, WHAT, __VA_ARGS__)
#define FE_7(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_6(c, locp, WHAT, __VA_ARGS__)
#define FE_8(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_7(c, locp, WHAT, __VA_ARGS__)
#define FE_9(c, locp, WHAT, X, ...) \
    WHAT(c, locp, X)FE_8(c, locp, WHAT, __VA_ARGS__)
/* repeat as needed */

#define GET_MACRO(_1, _2, _3, _4, _5, _6, _7, _8, _9, NAME, ...) NAME

#define FOR_EACH(c, locp, action, ...)          \
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
              FE_1)(c, locp, action,            \
                    __VA_ARGS__)                \
  } while (0)

#define OUT(c, locp, ...) FOR_EACH((c), (locp), OUT_IMPL, __VA_ARGS__)

const char *cmp_swap(context_t *c, YYLTYPE *locp, const char *type);

/* Temporary values creation */
t_hex_value gen_tmp(context_t *c, YYLTYPE *locp, int bit_width);

t_hex_value gen_local_tmp(context_t *c, YYLTYPE *locp, int bit_width);

t_hex_value gen_tmp_value(context_t *c,
                          YYLTYPE *locp,
                          const char *value,
                          int bit_width);

t_hex_value gen_imm_value(context_t *c __attribute__((unused)),
                          YYLTYPE *locp,
                          int value,
                          int bit_width);

void rvalue_free(context_t *c, YYLTYPE *locp, t_hex_value *rvalue);

void rvalue_materialize(context_t *c, YYLTYPE *locp, t_hex_value *rvalue);

void rvalue_extend(context_t *c, YYLTYPE *locp, t_hex_value *rvalue);

void rvalue_truncate(context_t *c, YYLTYPE *locp, t_hex_value *rvalue);

void varid_allocate(context_t *c,
                    YYLTYPE *locp,
                    t_hex_value *varid,
                    int width,
                    bool is_unsigned);

void ea_free(context_t *c, YYLTYPE *locp);

t_hex_value gen_bin_cmp(context_t *c,
                        YYLTYPE *locp,
                        const char *type,
                        t_hex_value *op1,
                        t_hex_value *op2);

/* Code generation functions */
t_hex_value gen_bin_op(context_t *c,
                       YYLTYPE *locp,
                       enum op_type type,
                       t_hex_value *operand1,
                       t_hex_value *operand2);

t_hex_value gen_cast_op(context_t *c,
                        YYLTYPE *locp,
                        t_hex_value *source,
                        unsigned target_width);

t_hex_value gen_extend_op(context_t *c,
                          YYLTYPE *locp,
                          t_hex_value *src_width,
                          t_hex_value *dst_width,
                          t_hex_value *value,
                          bool is_unsigned);

void gen_rdeposit_op(context_t *c,
                           YYLTYPE *locp,
                           t_hex_value *dest,
                           t_hex_value *value,
                           t_hex_range *range);

void gen_deposit_op(context_t *c,
                           YYLTYPE *locp,
                           t_hex_value *dest,
                           t_hex_value *value,
                           t_hex_value *index,
                           t_hex_cast *cast);

t_hex_value gen_rextract_op(context_t *c,
                           YYLTYPE *locp,
                           t_hex_value *source,
                           t_hex_range *range);

t_hex_value gen_extract_op(context_t *c,
                           YYLTYPE *locp,
                           t_hex_value *source,
                           t_hex_value *index,
                           t_hex_cast *cast);

t_hex_value gen_read_creg(context_t *c, YYLTYPE *locp, t_hex_value *reg);

void gen_write_creg(context_t *c,
                           YYLTYPE *locp,
                           t_hex_value *reg,
                           t_hex_value *value);

void gen_assign(context_t *c,
                YYLTYPE *locp,
                t_hex_value *dest,
                t_hex_value *value);

t_hex_value gen_convround(context_t *c,
                          YYLTYPE *locp,
                          t_hex_value *source,
                          t_hex_value *round_bit);

t_hex_value gen_round(context_t *c,
                      YYLTYPE *locp,
                      t_hex_value *source,
                      t_hex_value *position);

/* Circular addressing mode with auto-increment */
t_hex_value gen_circ_op(context_t *c,
                        YYLTYPE *locp,
                        t_hex_value *addr,
                        t_hex_value *increment,
                        t_hex_value *modifier);

t_hex_value gen_bitcnt_op(context_t *c, YYLTYPE *locp, t_hex_value *source,
                          bool negate,
                          bool reverse);

t_hex_value gen_ctpop_op(context_t *c, YYLTYPE *locp, t_hex_value *source);

void gen_dbg_str(context_t *c,
                 YYLTYPE *locp,
                 const char *str,
                 t_hex_value *val);

bool reg_equal(t_hex_reg *r1, t_hex_reg *r2);

bool pre_equal(t_hex_pre *p1, t_hex_pre *p2);

bool rvalue_equal(t_hex_value *v1, t_hex_value *v2);

void emit_header(context_t *c);

void emit_footer(context_t *c);

#endif /* PARSER_HELPERS_h */
