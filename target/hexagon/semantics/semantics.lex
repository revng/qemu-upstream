%option noyywrap noinput nounput
%option 8bit reentrant bison-bridge
%option warn nodefault
%option header-file="lex.yy.h"

%{
/*
 * Hexagon emulation for qemu: semantics lexer.
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
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses/>.
 */

#include <string.h>
#include <stdbool.h>
#include "semantics_struct.h"
#include "semantics.tab.h"
#include "csvparser.h"

//#define TOKEN_DEBUG

/* Global Error Counter */
int error_count = 0;
int fileno(FILE *stream);

%}

/* Definitions */
DIGIT                    [0-9]
LOWER_ID                 [a-z]
UPPER_ID                 [A-Z]
ID                       LOWER_ID|UPPER_ID
INST_NAME                [A-Z][0-9]_([a-z]|[0-9]|_)+ 
HEX_DIGIT                [0-9a-fA-F]
REG_ID_32                e|s|d|t|u|v|x|y
REG_ID_64                ee|ss|dd|tt|uu|vv|xx|yy
SYS_ID_32                s|d
SYS_ID_64                ss|dd
LOWER_IMM_S              s|m|r
LOWER_PRE                d|s|t|u|v|e|x|x
UPPER_IMM_S              S|M|R
ZERO_ONE                 0|1
IMM_ID                   r|s|S|u|U
VAR_ID                   [a-zA-Z_][a-zA-Z0-9_]*

/* Tokens */
%%

[ \t\f\v]+               { /* Ignore whitespaces. */ }
[\n\r]+                    { /* Ignore newlines. */ }

"DECL_RREG_"{REG_ID_32}"(R"{REG_ID_32}"V, R"{REG_ID_32}"N, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[10];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 32;
                           yylval->rvalue.is_dotnew = false;
                           return (DREG); }
"DECL_NEW_NREG_"{REG_ID_32}"(N"{REG_ID_32}"N, N"{REG_ID_32}"X, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[14];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 32;
                           yylval->rvalue.is_dotnew = true;
                           return (DREG); }
"DECL_RREG_"{REG_ID_64}"(R"{REG_ID_64}"V, R"{REG_ID_64}"N, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[10];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 64;
                           yylval->rvalue.is_dotnew = false;
                           return (DREG); }
"DECL_NEW_NREG_"{REG_ID_64}"(N"{REG_ID_64}"N, N"{REG_ID_64}"X, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[15];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 64;
                           yylval->rvalue.is_dotnew = true;
                           return (DREG); }
"DECL_IMM("{IMM_ID}"iV,"[0-9]");" {
                           yylval->rvalue.type = IMMEDIATE;
                           yylval->rvalue.is_unsigned = false;
                           yylval->rvalue.imm.type = VARIABLE;
                           yylval->rvalue.imm.id = yytext[9];
                           yylval->rvalue.is_dotnew = false;
                           return (DIMM); }
"DECL_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"V, P"{LOWER_PRE}"N, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = PREDICATE;
                           yylval->rvalue.pre.id = yytext[10];
                           yylval->rvalue.pre.is_bit_iter = false;
                           yylval->rvalue.bit_width = 8;
                           yylval->rvalue.is_dotnew = false;
                           return (DPRE); }
"DECL_NEW_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"N, P"{LOWER_PRE}"X, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = PREDICATE;
                           yylval->rvalue.pre.id = yytext[14];
                           yylval->rvalue.pre.is_bit_iter = false;
                           yylval->rvalue.bit_width = 8;
                           yylval->rvalue.is_dotnew = true;
                           return (DPRE); }
"DECL_MREG_u(MuV, MuN, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = 'u';
                           yylval->rvalue.reg.offset = 6;
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.bit_width = 32;
                           return (DMEM); }
"DECL_CREG_"{REG_ID_32}"(R"{REG_ID_32}"V, R"{REG_ID_32}"N, "[0-9]", "[0-9]");" {
                           yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = yytext[10];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 32;
                           yylval->rvalue.is_dotnew = false;
                           return (DCTR); }
"DECL_EA"                 { return (DEA); }
"READ_RREG_"{REG_ID_32}"(R"{REG_ID_32}"V, R"{REG_ID_32}"N);" {
                           return RREG; }
"WRITE_RREG_"{REG_ID_32}"(R"{REG_ID_32}"N, R"{REG_ID_32}"V);" {
                           return WREG; }
"FREE_RREG_"{REG_ID_32}"(R"{REG_ID_32}"V);" {
                           return FREG; }
"READ_RREG_"{REG_ID_64}"(R"{REG_ID_64}"V, R"{REG_ID_64}"N);" {
                           return RREG; }
"WRITE_RREG_"{REG_ID_64}"(R"{REG_ID_64}"N, R"{REG_ID_64}"V);" {
                           return WREG; }
"FREE_RREG_"{REG_ID_64}"(R"{REG_ID_64}"V);" {
                           return FREG; }
"READ_NEW_NREG_"{REG_ID_32}"(N"{REG_ID_32}"N, N"{REG_ID_32}"X);" {
                           return RREG; }
"FREE_NEW_NREG_"{REG_ID_32}"(N"{REG_ID_32}"N);" {
                           return FREG; }
"FREE_IMM("{IMM_ID}"iV);" {
                           return FIMM; }
"READ_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"V, P"{LOWER_PRE}"N);" {
                           return RPRE; }
"WRITE_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"N, P"{LOWER_PRE}"V);" {
                           return WPRE; }
"READ_NEW_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"N, P"{LOWER_PRE}"X);" {
                           return RPRE; }
"READ_MREG_u(MuV, MuN);"      { return RMEM; }
"READ_CREG_"{REG_ID_32}"(R"{REG_ID_32}"V, R"{REG_ID_32}"N);" {
                           return RCTR; }
"FREE_CREG_"{LOWER_PRE}"(R"{REG_ID_32}"V);" {
                           return FCTR; }
"FREE_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"V);" {
                           return FPRE; }
"FREE_NEW_PREG_"{LOWER_PRE}"(P"{LOWER_PRE}"N);" {
                           return FPRE; }
"FREE_MREG_u(MuV);"      { return FMEM; }
"FREE_EA"                { return FEA; }
"fGEN_TCG_"{INST_NAME}"(" { return FWRAP; }
"{"                      { return LBR; }
"}"                      { return RBR; }
"["                      { return LSQ; } 
"]"                      { return RSQ; }
"("                      { return LPAR; }
")"                      { return RPAR; }
";"                      { return SEMI; }
":"                      { return COLON; }
"+"                      { return PLUS; }
"-"                      { return MINUS; }
"_"                      { return USCORE; }
"[+-]"                   { return PMINUS; }
"*"                      { return MUL; }
"**"                     { return POW; }
"/"                      { return DIV; }
"%"                      { return MOD; }
"&"                      { return AND; }
"|"                      { return OR; }
"^"                      { return XOR; }
"[|&]"                   { return ANDOR; }
"[<<1]"                  { return OPTSHIFT; }
"[<<N]"                  { return NSHIFT; }
"~"                      { return NOT; }
"="                      { return ASSIGN; }
"+="                     { return INC; }
"-="                     { return DEC; }
"++"                     { return PLUSPLUS; }
"[+-]="                  { return INCDECA; }
"&="                     { return ANDA; }
"|="                     { return ORA; }
"^="                     { return XORA; }
"[|&]="                  { return ANDORA; }
"<"                      { return LT; }
">"                      { return GT; }
"<<"                     { return ASL; }
">>"                     { return ASR; }
"<<R"                    { return ROL; }
">>>"                    { return LSR; }
"=="                     { return EQ; }
"[!]="                   { return OPTEQ; }
"!="                     { return NEQ; }
"<="                     { return LTE; }
">="                     { return GTE; }
"->"                     { return LARR; }
"&&"                     { return ANDL; }
"||"                     { return ORL; }
"!"                      { return NOTL; }
","                      { return COMMA; }
"else"                   { return ELSE; }
"for"                    { return FOR; }
"i"                      { return I; }
"I"                      { return ICIRC; }
"if"                     { return IF; }
"Assembler mapped to:"   { return MAPPED; }
"apply_extension"        { return EXT; }
"dcache_inv_all"         { return DCKILL; }
"dcache_clean_addr"      { return DCLEAN; }
"dcache_cleaninv_addr"   { return DCINVA; }
"dcache_zero_addr"       { return DZEROA; }
"dcache_fetch"           { return DFETCH; }
"icache_inv_all"         { return ICKILL; }
"l2cache_inv_all"        { return L2KILL; }
"instruction_sync"       { return ISYNC; }
"frame_scramble"         { return FSCR; }
"frame_unscramble"       { return FSCR; }
"frame_check_limit"      { return FCHK; }
"Constant_extended"      { return CONSTEXT; }
"Enter debug mode"       { return BRKPT; }
"count_leading_ones"     { return LOCNT; }
"reverse_bits"           { return BREV; }
"brev"                   { return BREV; }
"memory_synch"           { return SYNCHT; }
"(!in_debug_mode)"       { return DEBUG; }
"lock_valid"             { return LOCK; }
"modectl[TNUM]"          { return MODECTL; }
"width"                  { return WIDTH; }
"offset"                 { return OFFSET; }
"shamt"                  { return SHAMT; }
"addr"                   { return ADDR; }
"sumr"                   { return SUMR; }
"sumi"                   { return SUMI; }
"control"                { return CTRL; }
"tmpr"                   { return TMPR; }
"tmpi"                   { return TMPI; }
"tmp"                    { return TMP; }
"tmpV"                   { return TMP; }
"src"                    { return TMP; }
"x0"                     { return X0; }
"x1"                     { return X1; }
"y0"                     { return Y0; }
"y1"                     { return Y1; }
"prod0"                  { return PROD0; }
"prod1"                  { return PROD1; }
"sxt"                    { return SXT; }
"zxt"                    { return ZXT; }
"min"                    { return MIN; }
"max"                    { return MAX; }
"ABS"                    { return ABS; }
"convround"              { return CROUND; }
"round"                  { return ROUND; }
"circ_add"               { return CIRCADD; }
".new"                   { return NEW; }
"[.new]"                 { return OPTNEW; }
"[!]"                    { return OPTNOTL; }
"[01]"                   { return ZEROONE; }
"sat"{DIGIT}+            { yylval->vec.width = atoi(yytext + 3);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
"sat_"{DIGIT}+           { yylval->vec.width = atoi(yytext + 4);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
"[sat"{DIGIT}+"]"        { yylval->vec.width = atoi(yytext + 4);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
"usat"{DIGIT}+           { yylval->vec.width = atoi(yytext + 4);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
"usat_"{DIGIT}+           { yylval->vec.width = atoi(yytext + 5);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
"[usat"{DIGIT}+"]"       { yylval->vec.width = atoi(yytext + 5);
                           yylval->vec.index = 0;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".u64"                   { return (U64); }
".i"                     { yylval->vec.width = 1;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".i*2"                   { yylval->vec.width = 1;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".i*2+1"                 { yylval->vec.width = 1;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".b["{DIGIT}"]"          { yylval->vec.width = 8;
                           yylval->vec.index = atoi(yytext + 3);
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".ub["{DIGIT}"]"         { yylval->vec.width = 8;
                           yylval->vec.index = atoi(yytext + 4);
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".h["{DIGIT}"]"          { yylval->vec.width = 16;
                           yylval->vec.index = atoi(yytext + 3);
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".h[01]"                 { yylval->vec.width = 16;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".uh["{DIGIT}"]"         { yylval->vec.width = 16;
                           yylval->vec.index = atoi(yytext + 4);
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".uh[01]"                { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".w["{DIGIT}"]"          { yylval->vec.width = 32;
                           yylval->vec.index = atoi(yytext + 3);
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".uw["{DIGIT}"]"         { yylval->vec.width = 32;
                           yylval->vec.index = atoi(yytext + 4);
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = NO_ITER;
                           return (VEC); }
".b[i]"                  { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".ub[i]"                 { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".ub["{IMM_ID}"iV]"      { yylval->vec.id = yytext[4];
                           yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = IMM_ITER;
                           return (VEC); }
".b[i+1]"                { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = IPLUS1_ITER;
                           return (VEC); }
".h[i]"                  { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".uh[i]"                 { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".w[i]"                  { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".uw[i]"                 { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I_ITER;
                           return (VEC); }
".b[i*2]"                { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".ub[i*2]"               { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".h[i*2]"                { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".uh[i*2]"               { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".w[i*2]"                { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".uw[i*2]"               { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2_ITER;
                           return (VEC); }
".b[i*2+1]"              { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".ub[i*2+1]"             { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".h[i*2+1]"              { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".uh[i*2+1]"             { yylval->vec.width = 16;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".w[i*2+1]"              { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".uw[i*2+1]"             { yylval->vec.width = 32;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = true;
                           yylval->vec.iter_type = I2PLUS1_ITER;
                           return (VEC); }
".b[i+4]"                { yylval->vec.width = 8;
                           yylval->vec.index = -1;
                           yylval->vec.is_unsigned = false;
                           yylval->vec.iter_type = IPLUS4_ITER;
                           return (VEC); }
"["{DIGIT}":"{DIGIT}"]"  { yylval->range.end = atoi(yytext + 1);
                           yylval->range.begin = atoi(yytext + 3);
                           return (RANGE); }
"(size8"[us]"_t)"          { yylval->cast.width = 8;
                           yylval->cast.is_unsigned = ((yytext[6]) == 'u');
                           return CAST; }
"(size16"[us]"_t)"         { yylval->cast.width = 16;
                           yylval->cast.is_unsigned = ((yytext[6]) == 'u');
                           return CAST; }
"#"                      { return HASH; }
"?"                      { return QMARK; }
"EA"                     { return EA; }
"PC"                     { return PC; }
"NPC"                    { return NPC; }
"*EA"                    { return STAREA; }
"TRAP \"0\""             { return TRAP0; }
"TRAP \"1\""             { return TRAP0; }
"USR.LPCFG"              { return LPCFG; }
"SSR.CAUSE"              { return CAUSE; }
"SSR.SSR_EX"             { return EX; }
"TLB"                    { return TLB; }
"IPEND"                  { return IPEND; }
"xv"                     { return TMP; }
"sv"                     { return TMP; }
"tv"                     { return TMP; }
"(int)"                  { return INT; }
"NOP"                    { return NOP; }
"PREDUSE_TIMING"         { return PREDUSE; }

"SA"{ZERO_ONE}           { yylval->index = atoi(yytext);
                           return SA; }
"LC"{ZERO_ONE}           { yylval->index = atoi(yytext);
                           return LC; }
"R"{REG_ID_32}"V"        { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"C"{REG_ID_32}           { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"C"{REG_ID_64}           { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 64;
                           return (REG); }
"R"{REG_ID_64}"V"        { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.bit_width = 64;
                           return (REG); }
"N"{LOWER_ID}            { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = false;
                           return (REG); }
"S"{SYS_ID_32}           { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = SYSTEM;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"S"{SYS_ID_64}           { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = SYSTEM;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.bit_width = 64;
                           return (REG); }
[rR]{DIGIT}+             { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = atoi(yytext + 1);
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"SGP"{DIGIT}             { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = SYSTEM;
                           yylval->rvalue.reg.id = atoi(yytext + 3);
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"SP"                     { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = 29;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"FP"                     { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = 30;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"LR"                     { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = GENERAL_PURPOSE;
                           yylval->rvalue.reg.id = 31;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"GP"                     { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = 11;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"MuV"                    { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = CONTROL;
                           yylval->rvalue.reg.id = yytext[1];
                           yylval->rvalue.reg.offset = 6;
                           yylval->rvalue.reg.is_const = false;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
"ELR"                    { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.reg.type = SYSTEM;
                           yylval->rvalue.reg.id = 3;
                           yylval->rvalue.reg.offset = 0;
                           yylval->rvalue.reg.is_const = true;
                           yylval->rvalue.bit_width = 32;
                           return (REG); }
[pP]{DIGIT}              { yylval->rvalue.type = PREDICATE;
                           yylval->rvalue.pre.id = yytext[1];
                           yylval->rvalue.pre.is_bit_iter = false;
                           yylval->rvalue.bit_width = 8;
                           return (PRE); }
"P"{LOWER_PRE}[NV]       { yylval->rvalue.type = PREDICATE;
                           yylval->rvalue.pre.id = yytext[1];
                           yylval->rvalue.pre.is_bit_iter = false;
                           yylval->rvalue.bit_width = 8;
                           return (PRE); }
"P"                      { yylval->rvalue.type = PREDICATE;
                           yylval->rvalue.bit_width = 8;
                           return (PRE); }
{IMM_ID}"iV"             { yylval->rvalue.type = IMMEDIATE;
                           yylval->rvalue.is_unsigned = false;
                           yylval->rvalue.imm.type = VARIABLE;
                           yylval->rvalue.imm.id = yytext[0];
                           return (IMM); }
"N"                      { yylval->rvalue.type = REGISTER;
                           yylval->rvalue.imm.type = VARIABLE;
                           yylval->rvalue.imm.id = 'N';
                           return (IMM); }
"prod"{DIGIT}            { return (-1); }
"acc"{DIGIT}+            { return (-1); }
{DIGIT}+                 { yylval->rvalue.type = IMMEDIATE;
                           yylval->rvalue.bit_width = 32;
                           yylval->rvalue.imm.type = VALUE;
                           yylval->rvalue.imm.value = atoi(yytext);
                           return (IMM); }
"0x"{HEX_DIGIT}+         { yylval->rvalue.type = IMMEDIATE;
                           yylval->rvalue.bit_width = 32;
                           yylval->rvalue.imm.type = VALUE;
                           yylval->rvalue.imm.value = strtol(yytext, NULL, 16);
                           return (IMM); }
{VAR_ID}                 { /* Variable name, we adopt the C names convention */
                           yylval->rvalue.type = VARID;
                           strncpy(yylval->rvalue.var.name, yytext, VAR_BUF_LEN);
                           return (VAR); }
.                        { fprintf(stderr, "Error: unexpected token \"%s\"\n", yytext);
                           error_count++;
                           return (-1); /* invalid token */
                         }

%%

#ifdef TOKEN_DEBUG
void emit_header(context_t *c) {
    printf("/* %s */\n", c->inst_name);
    printf("/* %s */\n", c->inst_code);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        printf("-------------------- LEXER DEBUG MODE -------------------\n\n");
        printf("Semantics: Hexagon ISA to tinycode generator compiler\n\n");
        printf("Copyright (c) 2017 Alessandro Di Federico, ");
        printf("rev.ng Srls Unipersonale\n");
        printf("Author: Niccolò Izzo <n@izzo.sh>\n\n");
        printf("Usage: ./semantics META-INSTRUCTIONS-CSV\n\n");
        printf("-------------------- LEXER DEBUG MODE -------------------");
        return 1;
    }

    printf("-------------------- LEXER DEBUG MODE -------------------");
    int token;
    CsvParser *csvparser = CsvParser_new(argv[1], ",", 0);
    CsvRow *row;
    while ((row = CsvParser_getRow(csvparser)) ) {
        context_t context = { 0 };
        YYSTYPE val = { 0 };
        YYSTYPE* yylvalp = &val;
        // TODO: Set here sane default for all the instruction modifiers
        context.mem_size = MEM_DOUBLE;
        const char **rowFields = CsvParser_getFields(row);
        if (CsvParser_getNumFields(row) < 2) {
            printf("Error: malformed csv!\n");
            return 1;
        }
        /* Extract field and initialize buffer */
        context.inst_name = rowFields[0];
        context.inst_code = rowFields[1];
        emit_header(&context);
        size_t buffer_size = strlen(context.inst_code) + 2;
        char * buffer = (char *) calloc(buffer_size, sizeof(char));
        memcpy(buffer, context.inst_code, buffer_size - 2);
        buffer[buffer_size - 2] = '\0';
        buffer[buffer_size - 1] = '\0';
        printf("Compiling: %s\n", context.inst_name);
        yylex_init(&context.scanner);
        yy_scan_buffer(buffer, buffer_size, context.scanner);
        // Redefine state to enable the use of built-in macros
        struct yyguts_t * yyg = (struct yyguts_t*)context.scanner;
        /* Start the lexing procedure */
        while ((token = yylex(yylvalp, context.scanner)) != 0)
            printf("Token: %d (%s)\n", token, yytext);
        /* Cleanup */
        yylex_destroy(context.scanner);
        CsvParser_destroy_row(row);
        free(buffer);
    }
    CsvParser_destroy(csvparser);
    return 0;
} 
#endif
