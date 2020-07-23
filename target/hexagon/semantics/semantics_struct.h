/*
 * Semantics struct header
 *
 * Copyright (c) 2017 Alessandro Di Federico, rev.ng Srls Unipersonale
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef SEMANTICS_STRUCT_H
#define SEMANTICS_STRUCT_H

#define TCGV_NAME_SIZE 7
#define MAX_WRITTEN_REGS 32
#define OFFSET_STR_LEN 32
#define VAR_BUF_LEN 32
#define ALLOC_LIST 5
#define OUT_BUF_LEN 1024 * 1024
#define SIGNATURE_BUF_LEN 1024

enum reg_type {GENERAL_PURPOSE, CONTROL, SYSTEM, MODIFIER};

/* Semantic Records */
typedef struct t_hex_reg
{
    char id;
    bool is_const;
    enum reg_type type;
    int offset;
    unsigned bit_width;
} t_hex_reg;

typedef struct t_hex_tmp
{
    int index;
} t_hex_tmp;

enum imm_union_tag {VARIABLE, VALUE, QEMU_TMP, IMM_PC, IMM_CONSTEXT};

typedef struct t_hex_imm
{
    union {
        char id;
        uint64_t value;
        uint64_t index;
    };
    enum imm_union_tag type;
} t_hex_imm;

typedef struct t_hex_pre
{
    char id;
    bool is_zeroone;
    bool is_bit_iter;
} t_hex_pre;

enum rvalue_extra_type {EA_T, LPCFG_T, LC_T, SA_T, WIDTH_T, OFFSET_T,
                        SHAMT_T, ADDR_T, SUMR_T, SUMI_T, CTRL_T, TMPR_T,
                        TMPI_T, X0_T, X1_T, Y0_T, Y1_T, PROD0_T, PROD1_T,
                        MAX_T, MIN_T, TMP_T};

typedef struct t_hex_extra
{
    int index;
    enum rvalue_extra_type type;
    bool temp;
} t_hex_extra;

enum iterable_type {NO_ITER, I_ITER, I2_ITER, IPLUS1_ITER, IPLUS4_ITER,
                    I2PLUS1_ITER, IMM_ITER};

typedef struct t_hex_vec
{
    char id;
    int index;
    int width;
    bool is_unsigned;
    bool is_zeroone;
    enum iterable_type iter_type;
} t_hex_vec;

typedef struct t_hex_cast
{
    int width;
    bool is_unsigned;
} t_hex_cast;

typedef struct t_hex_range
{
    int begin;
    int end;
} t_hex_range;

typedef struct t_hex_var
{
    char name[VAR_BUF_LEN];
} t_hex_var;

enum rvalue_union_tag {REGISTER, TEMP, IMMEDIATE, PREDICATE, EXTRA, VARID};

typedef struct t_hex_value
{
    union {
        t_hex_reg reg;
        t_hex_tmp tmp;
        t_hex_imm imm;
        t_hex_pre pre;
        t_hex_var var;
        t_hex_extra extra;
    };
    enum rvalue_union_tag type;
    unsigned bit_width;
    bool is_unsigned;
    bool is_dotnew;
    bool is_optnew;
    bool is_vectorial;
    bool is_range;
    bool is_symbol;
    t_hex_vec vec;
    t_hex_range range;
    char *name;
} t_hex_value;

enum op_type {ADD, SUBTRACT, ADDSUB, MULTIPLY, DIVIDE,
              ASHIFTL, ASHIFTR, LSHIFTR, ROTATE, ANDB, ORB, ANDORB, XORB,
              MINI, MAXI, MODULO};
enum cmp_type {EQ_OP, NEQ_OP, OPTEQ_OP, LT_OP, LTU_OP, GT_OP, GTU_OP,
               LTE_OP, LEU_OP, GTE_OP, GEU_OP};
enum mem_type {MEM_BYTE, MEM_HALF, MEM_WORD, MEM_DOUBLE};

/* Translation Context */
typedef struct context_t {
    void *scanner; // reentrant parser state pointer
    int tmp_count;
    int qemu_tmp_count;
    int not_count;
    int zeroone_count;
    int predicate_count;
    int highlow_count;
    int p_reg_count;
    int if_count;
    bool is_extra_created[TMP_T - EA_T + 1];
    bool ea_declared;
    bool is_jump;
    bool is_stop;
    bool mem_unsigned;
    enum mem_type mem_size; 
    char written_regs[MAX_WRITTEN_REGS];
    int written_index;
    const char *inst_name;
    const char *inst_code;
    int error_count;
    int inst_index;
    char *out_buffer;
    int out_c;
    char *signature_buffer;
    int signature_c;
    const char allocated[ALLOC_LIST][VAR_BUF_LEN];
    FILE *defines_file;
} context_t;

#endif /* SEMANTICS_STRUCT_H */
