/*
 * QEMU TCG JIT using LLVM
 *
 * Copyright (C) 2020, 2021, Zhang Boyang <zhangboyang.id@gmail.com>
 *
 * License: GNU GPL, version 2 or later.
 *   See the COPYING file in the top-level directory.
 */

#include "qemu/osdep.h"
#include "qemu/log.h"
#include "exec/exec-all.h"
#include "exec/log.h"
#include "tcg/tcg.h"
#include "tcg/tcg-internal.h"
#include "tcg/tcg-llvm.h"

#include <llvm-c/Analysis.h>
#include <llvm-c/Transforms/Utils.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm-c/Transforms/IPO.h>
#include <llvm-c/Support.h>

#ifdef CONFIG_SOFTMMU
#error LLVM + SOFTMMU Not supported
#endif

#define _CAT(x, y) x ## y
#define CAT(x, y) _CAT(x, y)
#define CAT3(x, y, z) CAT(CAT(x, y), z)

#define MAX_SYMNAME 128


static void serialize(GByteArray *buf, const void *mem, size_t size)
{
    g_byte_array_append(buf, mem, size);
}
static void deserialize(const guint8 **buf, void *mem, size_t size)
{
    memcpy(mem, *buf, size);
    *buf += size;
}

static void serialize_temp(GByteArray *temp_buf, TCGContext *s, TCGTemp *ts)
{
    TCGTempKind kind = ts->kind;
    TCGType type = ts->type;
    int mem_base;
    intptr_t mem_offset;
    int name_id;
    int64_t val;
    serialize(temp_buf, &kind, sizeof(kind));
    serialize(temp_buf, &type, sizeof(type));
    switch (kind) {
    case TEMP_GLOBAL: case TEMP_FIXED:
        mem_base = ts->mem_base ? temp_idx(ts->mem_base) : -1;
        serialize(temp_buf, &mem_base, sizeof(mem_base));
        mem_offset = ts->mem_offset;
        serialize(temp_buf, &mem_offset, sizeof(mem_offset));
        serialize(temp_buf, ts->name, strlen(ts->name) + 1);
        break;
    case TEMP_LOCAL: case TEMP_NORMAL:
        name_id = temp_idx(ts) - s->nb_globals;
        serialize(temp_buf, &name_id, sizeof(name_id));
        break;
    case TEMP_CONST:
        val = ts->val;
        serialize(temp_buf, &val, sizeof(val));
        break;
    }
}
static void deserialize_temp(const guint8 **temp_buf, TCGLLVMTemp *t)
{
    int name_id;
    deserialize(temp_buf, &t->kind, sizeof(t->kind));
    deserialize(temp_buf, &t->type, sizeof(t->type));
    switch (t->kind) {
    case TEMP_GLOBAL: case TEMP_FIXED:
        deserialize(temp_buf, &t->mem_base, sizeof(t->mem_base));
        deserialize(temp_buf, &t->mem_offset, sizeof(t->mem_offset));
        snprintf(t->name, sizeof(t->name), "%s", (char *) *temp_buf);
        *temp_buf += strlen((char *) *temp_buf) + 1;
        break;
    case TEMP_LOCAL: case TEMP_NORMAL:
        deserialize(temp_buf, &name_id, sizeof(name_id));
        snprintf(t->name, sizeof(t->name), "%s%d",
            t->kind == TEMP_LOCAL ? "loc" : "tmp", name_id);
        break;
    case TEMP_CONST:
        deserialize(temp_buf, &t->val, sizeof(t->val));
        break;
    }
}

#define carg_is_label(opc, carg) ({ \
    static const bool lookup_table[NB_OPS][MAX_OPC_PARAM] = { \
        [INDEX_op_set_label][0] = true, \
        [INDEX_op_br][0] = true, \
        [INDEX_op_brcond_i32][1] = true, \
        [INDEX_op_brcond_i64][1] = true, \
        [INDEX_op_brcond2_i32][1] = true, \
    }; \
    lookup_table[opc][carg]; \
})
#define carg_is_helper(opc, carg) ((opc) == INDEX_op_call && (carg) == 0)
#define carg_is_exitcode(opc, carg) ((opc) == INDEX_op_exit_tb && (carg) == 0)
static void serialize_op(GByteArray *op_buf, TCGOp *op, bool *temp_used)
{
    TCGOpcode c = op->opc;
    const TCGOpDef *def = &tcg_op_defs[c];
    int nb_oargs = def->nb_oargs;
    int nb_iargs = def->nb_iargs;
    int nb_cargs = def->nb_cargs;
    int i, j;
    serialize(op_buf, &c, sizeof(c));
    if (c == INDEX_op_call) {
        nb_oargs = TCGOP_CALLO(op);
        nb_iargs = TCGOP_CALLI(op);
        serialize(op_buf, &nb_oargs, sizeof(nb_oargs));
        serialize(op_buf, &nb_iargs, sizeof(nb_iargs));
    }
    for (i = 0; i < nb_oargs + nb_iargs; i++) {
        int idx = temp_idx(arg_temp(op->args[i]));
        serialize(op_buf, &idx, sizeof(idx));
        temp_used[idx] = true;
    }
    for (j = 0; j < nb_cargs; i++, j++) {
        uint64_t carg = op->args[i];
        if (carg_is_label(c, j)) {
            carg = arg_label(op->args[i])->id;
        }
        if (carg_is_helper(c, j)) {
            TCGHelperInfo *info;
            info = g_hash_table_lookup(helper_table, (gpointer)op->args[i]);
            tcg_debug_assert(info);
            carg = info - all_helpers;
        }
        if (carg_is_exitcode(c, j)) {
            carg = carg & TB_EXIT_MASK;
        }
        serialize(op_buf, &carg, sizeof(carg));
    }
}
static void deserialize_op(const guint8 **op_buf,
    TCGOpcode *c, int *nb_oargs, int *nb_iargs, int *nb_cargs, uint64_t *args)
{
    const TCGOpDef *def;
    int i, j;
    deserialize(op_buf, c, sizeof(*c));
    def = &tcg_op_defs[*c];
    *nb_oargs = def->nb_oargs;
    *nb_iargs = def->nb_iargs;
    *nb_cargs = def->nb_cargs;
    if (*c == INDEX_op_call) {
        deserialize(op_buf, nb_oargs, sizeof(*nb_oargs));
        deserialize(op_buf, nb_iargs, sizeof(*nb_iargs));
    }
    for (i = 0; i < *nb_oargs + *nb_iargs; i++) {
        uint32_t idx;
        deserialize(op_buf, &idx, sizeof(idx));
        args[i] = idx;
    }
    for (j = 0; j < *nb_cargs; i++, j++) {
        deserialize(op_buf, &args[i], sizeof(args[i]));
    }
}

void tcg_llvm_serialize_tb(TCGContext *s, TranslationBlock *tb)
{
    TCGLLVMContext *l = s->llvm_ctx;
    TCGOp *op;
    bool temp_used[TCG_MAX_TEMPS] = {};
    int idx;
    GByteArray *temp_buf = tb->packed_tcg.temp_buf;
    GByteArray *op_buf = tb->packed_tcg.op_buf;

    QTAILQ_FOREACH(op, &s->ops, link) {
        serialize_op(op_buf, op, temp_used);
    }

    for (idx = 0; idx < TCG_MAX_TEMPS; idx++) {
        if (temp_used[idx]) {
            serialize(temp_buf, &idx, sizeof(idx));
            serialize_temp(temp_buf, s, &s->temps[idx]);
        }
    }

    g_checksum_reset(l->hasher);
    g_checksum_update(l->hasher, temp_buf->data, temp_buf->len);
    g_checksum_update(l->hasher, op_buf->data, op_buf->len);
    tb->packed_tcg.digest = g_strdup(g_checksum_get_string(l->hasher));
}




/* LLVM convenient macros */

/* HBITS: host word bits */
#define HBITS TCG_TARGET_REG_BITS
/* GBITS: guest word bits */
#define GBITS TARGET_LONG_BITS

/* Types */
#define VOIDTY LLVMVoidTypeInContext(l->ctx)
#define INTTY(bits) CAT3(LLVMInt, bits, TypeInContext)(l->ctx)
#define PTRTY(ty) LLVMPointerType(ty, 0)
#define ELETY(ptr) LLVMGetElementType(ptr)
#define TYOF(val) LLVMTypeOf(val)
#define FUNCTY(ret_ty, ...) ({ \
    LLVMTypeRef __functy_args[] = {__VA_ARGS__}; \
    LLVMFunctionType(ret_ty, __functy_args, ARRAY_SIZE(__functy_args), 0); \
})

/* Function params */
#define PARAM(n) LLVMGetParam(l->fn, n)

/* Alloca */
#define ALLOCAT(ty, name) LLVMBuildAlloca(l->bldr, ty, name)
#define ALLOCA(bits, name) ALLOCAT(INTTY(bits), name)
#define ALLOCAH(name) ALLOCA(HBITS, name)

/* Constant values */
#define CONSTT(ty, c) LLVMConstInt(ty, c, 0)
#define CONST(bits, c) CONSTT(INTTY(bits), c)
#define CONSTH(c) CONST(HBITS, c)

/* Load and Store */
#undef LD
#undef ST
#define LD(ptr) LLVMBuildLoad(l->bldr, ptr, "")
#define ST(val, ptr) LLVMBuildStore(l->bldr, val, ptr)
#define SET_VOLATILE(v) ({ \
    LLVMValueRef __set_volatile_v = (v); \
    LLVMSetVolatile(__set_volatile_v, 1); \
    __set_volatile_v; \
})

/* Arith Ops */
#define ADD(x, y) LLVMBuildAdd(l->bldr, x, y, "")
#define SUB(x, y) LLVMBuildSub(l->bldr, x, y, "")
#define MUL(x, y) LLVMBuildMul(l->bldr, x, y, "")
#define DIV(x, y, div_kind) CAT3(LLVMBuild, div_kind, Div)(l->bldr, x, y, "")
#define REM(x, y, div_kind) CAT3(LLVMBuild, div_kind, Rem)(l->bldr, x, y, "")
#define AND(x, y) LLVMBuildAnd(l->bldr, x, y, "")
#define OR(x, y) LLVMBuildOr(l->bldr, x, y, "")
#define SHL(x, y) LLVMBuildShl(l->bldr, x, y, "")
#define SHR(x, y, shr_kind) CAT3(LLVMBuild, shr_kind, Shr)(l->bldr, x, y, "")
#define LSHR(x, y) SHR(x, y, L)
#define ASHR(x, y) SHR(x, y, A)

/* Create a new basic block */
#define NEWBB(name) LLVMAppendBasicBlockInContext(l->ctx, l->fn, name)

/* Branch and Conditional branch */
#define BR(dst) LLVMBuildBr(l->bldr, dst)
#define CONDBR(i1, t, f) LLVMBuildCondBr(l->bldr, i1, t, f)
#define SELECT(i1, t, f) LLVMBuildSelect(l->bldr, i1, t, f, "")
#define INTCMP(x, y, tcg_cond) \
    LLVMBuildICmp(l->bldr, map_cond(tcg_cond), x, y, "")
#define CMPBR(x, y, tcg_cond, t, f) CONDBR(INTCMP(x, y, tcg_cond), t, f)

/* Convert pointers */
#define P2I(p) LLVMBuildPtrToInt(l->bldr, p, INTTY(HBITS), "")
#define I2P(i, ty) LLVMBuildIntToPtr(l->bldr, i, PTRTY(ty), "")

/* Get &env[offset], using GEP and bitcast */
#define ENV(off, ty, name) ({ \
        LLVMValueRef __env_off_value[] = {CONSTH(0), CONSTH(off)}; \
        LLVMBuildBitCast(l->bldr, \
            LLVMBuildInBoundsGEP2(l->bldr, \
                l->env_ty, \
                l->env, \
                __env_off_value, ARRAY_SIZE(__env_off_value), \
                "" \
            ), \
            PTRTY(ty), \
            name \
        ); \
    })

/* Extend or truncate integers */
#define TRUNC(val, dst_bits) LLVMBuildTrunc(l->bldr, val, INTTY(dst_bits), "")
#define EXTEND(val, dst_bits, ext_kind) \
    CAT3(LLVMBuild, ext_kind, Ext)(l->bldr, val, INTTY(dst_bits), "")
#define ZEXT(val, dst_bits) EXTEND(val, dst_bits, Z)
#define SEXT(val, dst_bits) EXTEND(val, dst_bits, S)
#define TZEXT(val, src_bits, dst_bits) ZEXT(TRUNC(val, src_bits), dst_bits)
#define TSEXT(val, src_bits, dst_bits) SEXT(TRUNC(val, src_bits), dst_bits)

/* Function call */
#define CALL(fn, ...) ({ \
    LLVMValueRef __call_args[] = {__VA_ARGS__}; \
    LLVMBuildCall(l->bldr, fn, __call_args, ARRAY_SIZE(__call_args), ""); \
})
#define INTRINSIC(name, ...) ({ \
    LLVMTypeRef __intrinsic_overload_ty[] = {__VA_ARGS__}; \
    LLVMGetIntrinsicDeclaration(l->mod, \
        LLVMLookupIntrinsicID(name, strlen(name)), \
        __intrinsic_overload_ty, ARRAY_SIZE(__intrinsic_overload_ty)); \
})




/* Map TCG Condition to LLVM Integer Predicate */
static LLVMIntPredicate map_cond(TCGCond tcg_cond)
{
    switch (tcg_cond) {
    case TCG_COND_EQ:  return LLVMIntEQ;
    case TCG_COND_NE:  return LLVMIntNE;
    case TCG_COND_LT:  return LLVMIntSLT;
    case TCG_COND_GE:  return LLVMIntSGE;
    case TCG_COND_LE:  return LLVMIntSLE;
    case TCG_COND_GT:  return LLVMIntSGT;
    case TCG_COND_LTU: return LLVMIntULT;
    case TCG_COND_GEU: return LLVMIntUGE;
    case TCG_COND_LEU: return LLVMIntULE;
    case TCG_COND_GTU: return LLVMIntUGT;
    default: tcg_abort();
    }
}

static LLVMTypeRef map_type(TCGLLVMContext *l, TCGType tcg_type)
{
    switch (tcg_type) {
    case TCG_TYPE_I32: return INTTY(32);
    case TCG_TYPE_I64: return INTTY(64);
    default: tcg_abort();
    }
}

static inline void check_error(LLVMErrorRef e)
{
    if (e) {
        char *msg = LLVMGetErrorMessage(e);
        qemu_log("%s\n", msg);
        LLVMDisposeErrorMessage(msg);
        exit(1);
    }
}

static inline void dump_module(LLVMModuleRef mod)
{
    //LLVMDumpModule(mod);
    char *str = LLVMPrintModuleToString(mod);
    qemu_log("%s", str);
    LLVMDisposeMessage(str);
}
static inline void write_module(LLVMModuleRef mod, const char *filename)
{
    char *msg = NULL;
    if (LLVMPrintModuleToFile(mod, filename, &msg)) {
        qemu_log("%s\n", msg);
        LLVMDisposeMessage(msg);
        tcg_abort();
    }
}

static void set_tb_func_attr(TCGLLVMContext *l, LLVMValueRef fn)
{
    LLVMSetFunctionCallConv(fn, l->tb_callconv);
    LLVMAddAttributeAtIndex(fn, l->nb_fastreg + 1, l->attr_noalias);
    LLVMAddAttributeAtIndex(fn, l->nb_fastreg + 1, l->attr_vaildenv);
    LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex, l->attr_nounwind);
}
static void set_tb_call_attr(TCGLLVMContext *l, LLVMValueRef instr)
{
    LLVMSetInstructionCallConv(instr, l->tb_callconv);
    LLVMAddCallSiteAttribute(instr, l->nb_fastreg + 1, l->attr_noalias);
    LLVMAddCallSiteAttribute(instr, l->nb_fastreg + 1, l->attr_vaildenv);
    LLVMAddCallSiteAttribute(instr, LLVMAttributeFunctionIndex,
        l->attr_nounwind);
    QLLVMSetMustTailCall(instr, 1);
}


/* Get basic block of tcg label */
#define fmt_label_name(array, label_id) \
    snprintf(array, sizeof(array), "L%u", label_id)
static LLVMBasicBlockRef get_label(TCGLLVMContext *l, unsigned label_id)
{
    if (!g_hash_table_contains(l->labels, GUINT_TO_POINTER(label_id))) {
        char buf[MAX_SYMNAME];
        fmt_label_name(buf, label_id);
        g_hash_table_insert(l->labels, GUINT_TO_POINTER(label_id), NEWBB(buf));
    }
    return g_hash_table_lookup(l->labels, GUINT_TO_POINTER(label_id));
}

/* Finish current block and switch builder to the given block */
static void switch_bb(TCGLLVMContext *l, LLVMBasicBlockRef next_bb)
{
    if (!LLVMGetBasicBlockTerminator(LLVMGetInsertBlock(l->bldr))) {
        LLVMBuildBr(l->bldr, next_bb);
    }
    LLVMPositionBuilderAtEnd(l->bldr, next_bb);
}


static LLVMValueRef get_epilogue(TCGLLVMContext *l)
{
    LLVMValueRef fn;
    fn = LLVMGetNamedFunction(l->mod, "epilogue");
    if (!fn) {
        fn = LLVMAddFunction(l->mod, "epilogue", l->tb_type);
        set_tb_func_attr(l, fn);
    }
    return fn;
}
static LLVMValueRef get_ret_pc(TCGLLVMContext *l)
{
    LLVMValueRef val;
    val = LLVMGetNamedGlobal(l->mod, "ret_pc");
    if (!val) {
        val = LLVMAddGlobal(l->mod, INTTY(64), "ret_pc");
    }
    return val;
}
static LLVMValueRef get_map_pc(TCGLLVMContext *l)
{
    LLVMValueRef fn;
    fn = LLVMGetNamedFunction(l->mod, "map_pc");
    if (!fn) {
        fn = LLVMAddFunction(l->mod, "map_pc",
            FUNCTY(PTRTY(l->tb_type), PTRTY(l->env_ty)));
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_nounwind);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_readonly);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_willreturn);
    }
    return fn;
}

#define fmt_helper_symbol(array, idx) \
    snprintf(array, sizeof(array), \
        "helper_%d_%s", idx, all_helpers[idx].name)
static LLVMValueRef get_helper(TCGLLVMContext *l, int idx)
{
    LLVMValueRef fn;
    char symname[MAX_SYMNAME];
    fmt_helper_symbol(symname, idx);
    fn = LLVMGetNamedFunction(l->mod, symname);
    if (!fn) {
        fn = LLVMAddFunction(l->mod, symname,
            LLVMFunctionType(INTTY(HBITS), NULL, 0, 1));
        LLVMSetLinkage(fn, LLVMExternalLinkage);
    }
    return fn;
}
static void set_helper_call_attr(TCGLLVMContext *l, LLVMValueRef instr, int idx)
{
    const TCGHelperInfo *info = &all_helpers[idx];
    LLVMAddCallSiteAttribute(instr, LLVMAttributeFunctionIndex,
        l->attr_nounwind);

    /* FIXME: safe? */
    if ((info->flags & TCG_CALL_NO_RWG_SE) == TCG_CALL_NO_RWG_SE) {
        LLVMAddCallSiteAttribute(instr, LLVMAttributeFunctionIndex,
            l->attr_readnone);
    } else if ((info->flags & TCG_CALL_NO_WG_SE) == TCG_CALL_NO_WG_SE) {
        LLVMAddCallSiteAttribute(instr, LLVMAttributeFunctionIndex,
            l->attr_readonly);
    } else if ((info->flags & TCG_CALL_NO_RWG) == TCG_CALL_NO_RWG) {
        LLVMAddCallSiteAttribute(instr, LLVMAttributeFunctionIndex,
            l->attr_inaccessiblememonly);
    } else if ((info->flags & TCG_CALL_NO_WG) == TCG_CALL_NO_WG) {
        /* no suitable attribute */
    }
}

static int load_cache_meta(TCGLLVMContext *l, target_ulong pc);

#define fmt_tb_symbol(array, prefix, pc) \
    snprintf(array, sizeof(array), prefix "_%016" PRIx64, (uint64_t) (pc))
static LLVMValueRef get_tb_func(TCGLLVMContext *l, target_ulong pc)
{
    LLVMValueRef fn;
    char symname[MAX_SYMNAME];
    fmt_tb_symbol(symname, "tb", pc);
    fn = LLVMGetNamedFunction(l->mod, symname);
    if (!fn) {
        fn = LLVMAddFunction(l->mod, symname, l->tb_type);
        set_tb_func_attr(l, fn);
        load_cache_meta(l, pc);
        if (!g_hash_table_contains(l->tb_compiled, (void *)(uintptr_t)pc)) {
            g_array_append_val(l->tb_fwdrefs, pc);
        }
    }
    return fn;
}

static void define_stub(TCGLLVMContext *l)
{
    char symname[MAX_SYMNAME];
    unsigned long i;
    target_ulong pc;
    LLVMValueRef fwdptr;

    for (i = 0; i < l->tb_fwdrefs->len; i++) {
        pc = g_array_index(l->tb_fwdrefs, target_ulong, i);

        fmt_tb_symbol(symname, "fwdptr", pc);
        fwdptr = LLVMAddGlobal(l->mod, PTRTY(l->tb_type), symname);
        
        /* Generate tb placeholder with weak linkage */
        l->fn = get_tb_func(l, pc);
        LLVMSetLinkage(l->fn, LLVMLinkOnceAnyLinkage);
        LLVMPositionBuilderAtEnd(l->bldr, 
            LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));
        {
            /* Call fwdptr */
            int i;
            LLVMValueRef args[l->nb_fastreg + 1];
            LLVMValueRef result;
            for (i = 0; i < l->nb_fastreg + 1; i++) {
                args[i] = PARAM(i);
            }
            result = LLVMBuildCall2(l->bldr,
                l->tb_type, LD(fwdptr), args, l->nb_fastreg + 1, "");
            set_tb_call_attr(l, result);
            LLVMBuildRetVoid(l->bldr);
        }
        LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);

        if (!g_hash_table_contains(l->tb_fwdptrs, (void *)(uintptr_t)pc)) {
            /* Generate tb stub, which may be replaced later */
            fmt_tb_symbol(symname, "stub", pc);
            l->fn = LLVMAddFunction(l->mod, symname, l->tb_type);
            set_tb_func_attr(l, l->fn);

            LLVMSetLinkage(fwdptr, LLVMLinkOnceAnyLinkage);
            LLVMSetInitializer(fwdptr, l->fn);
        
            LLVMSetLinkage(l->fn, LLVMLinkOnceODRLinkage);
            LLVMPositionBuilderAtEnd(l->bldr, 
                LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));

            /* Save pc to global var */
            ST(CONST(64, pc), get_ret_pc(l));

            /* Call epilogue */
            {
                int i;
                LLVMValueRef args[l->nb_fastreg + 1];
                LLVMValueRef result;
                for (i = 0; i < l->nb_fastreg + 1; i++) {
                    args[i] = PARAM(i);
                }
                result = LLVMBuildCall2(l->bldr,
                    l->tb_type, get_epilogue(l), args, l->nb_fastreg + 1, "");
                set_tb_call_attr(l, result);
                LLVMBuildRetVoid(l->bldr);
            }

            LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
        }
    }
}
static void internalize_tb_placeholder(TCGLLVMContext *l)
{
    unsigned long i;
    target_ulong pc;
    for (i = 0; i < l->tb_fwdrefs->len; i++) {
        pc = g_array_index(l->tb_fwdrefs, target_ulong, i);

        /* Set linkage to internal to avoid confilt,
           because weak symbol will become strong after materializing */
        LLVMSetLinkage(get_tb_func(l, pc), LLVMInternalLinkage);
    }
}

static LLVMValueRef get_qemu_ld(TCGLLVMContext *l, LLVMTypeRef ty)
{
    LLVMValueRef fn;
    char symname[MAX_SYMNAME];
    sprintf(symname, "qemu_ld_i%u", LLVMGetIntTypeWidth(ty));
    fn = LLVMGetNamedFunction(l->mod, symname);
    if (!fn) {
        fn = LLVMAddFunction(l->mod, symname, FUNCTY(ty, INTTY(HBITS)));
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_nounwind);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_inaccessiblememonly);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_willreturn);
        g_ptr_array_add(l->fn_qemu_ld, fn);
    }
    return fn;
}
static LLVMValueRef get_qemu_st(TCGLLVMContext *l, LLVMTypeRef ty)
{
    LLVMValueRef fn;
    char symname[MAX_SYMNAME];
    sprintf(symname, "qemu_st_i%u", LLVMGetIntTypeWidth(ty));
    fn = LLVMGetNamedFunction(l->mod, symname);
    if (!fn) {
        fn = LLVMAddFunction(l->mod, symname, FUNCTY(VOIDTY, ty, INTTY(HBITS)));
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_nounwind);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_inaccessiblememonly);
        LLVMAddAttributeAtIndex(fn, LLVMAttributeFunctionIndex,
            l->attr_willreturn);
        g_ptr_array_add(l->fn_qemu_st, fn);
    }
    return fn;
}

static void define_qemu_ld_st(TCGLLVMContext *l, bool internal)
{
    unsigned i;
    for (i = 0; i < l->fn_qemu_ld->len; i++) {
        l->fn = g_ptr_array_index(l->fn_qemu_ld, i);
        LLVMAddAttributeAtIndex(l->fn, LLVMAttributeFunctionIndex,
            l->attr_alwaysinline);
        LLVMRemoveEnumAttributeAtIndex(l->fn, LLVMAttributeFunctionIndex,
            LLVMGetEnumAttributeKind(l->attr_inaccessiblememonly));
        LLVMPositionBuilderAtEnd(l->bldr, 
            LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));
        LLVMBuildRet(l->bldr,
            SET_VOLATILE(LD(I2P(PARAM(0), LLVMGetReturnType(ELETY(TYOF(l->fn)))))));
        LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
        LLVMSetVisibility(l->fn, LLVMHiddenVisibility);
        if (internal) LLVMSetLinkage(l->fn, LLVMInternalLinkage);
    }
    for (i = 0; i < l->fn_qemu_st->len; i++) {
        l->fn = g_ptr_array_index(l->fn_qemu_st, i);
        LLVMAddAttributeAtIndex(l->fn, LLVMAttributeFunctionIndex,
            l->attr_alwaysinline);
        LLVMRemoveEnumAttributeAtIndex(l->fn, LLVMAttributeFunctionIndex,
            LLVMGetEnumAttributeKind(l->attr_inaccessiblememonly));
        LLVMPositionBuilderAtEnd(l->bldr, 
            LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));
        SET_VOLATILE(ST(PARAM(0), I2P(PARAM(1), TYOF(PARAM(0)))));
        LLVMBuildRetVoid(l->bldr);
        LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
        LLVMSetVisibility(l->fn, LLVMHiddenVisibility);
        if (internal) LLVMSetLinkage(l->fn, LLVMInternalLinkage);
    }
    g_ptr_array_set_size(l->fn_qemu_ld, 0);
    g_ptr_array_set_size(l->fn_qemu_st, 0);
}


static void sync_fastreg(TCGLLVMContext *l, bool use_undef)
{
    int i;
    for (i = 0; i < l->nb_fastreg; i++) {
        ST(
            use_undef ? LLVMGetUndef(l->regmap[i].ty) : PARAM(i),
            ENV(l->regmap[i].off, l->regmap[i].ty, "")
        );
    }
}


static void gen_code(TCGLLVMContext *l, TranslationBlock *tb)
{
    LLVMBasicBlockRef bb;
    const guint8 *buf, *buf_end;

    memset(l->temps, 0, sizeof(l->temps));
    g_hash_table_remove_all(l->labels);

    l->fn = get_tb_func(l, tb->pc);

    l->env = PARAM(l->nb_fastreg);
    bb = LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry");
    LLVMPositionBuilderAtEnd(l->bldr, bb);

#define is_env(idx) (strcmp(l->temps[idx].name, "env") == 0)
    buf = tb->packed_tcg.temp_buf->data;
    buf_end = buf + tb->packed_tcg.temp_buf->len;
    while (buf < buf_end) {
        int idx;
        TCGLLVMTemp *t;
        LLVMTypeRef ty;
        deserialize(&buf, &idx, sizeof(idx));
        t = &l->temps[idx];
        deserialize_temp(&buf, t);
        ty = map_type(l, t->type);

        switch (t->kind) {
        case TEMP_FIXED:
        case TEMP_GLOBAL:
            /* Assign initial value */
            if (is_env(idx)) {
                t->solt = ALLOCAH(t->name);
                /* env is a function argument */
                ST(P2I(l->env), t->solt);
            } else {
                if (!is_env(t->mem_base)) {
                    tcg_abort();
                }
                t->solt = ENV(t->mem_offset, ty, t->name);
            }
            break;
        case TEMP_LOCAL:
        case TEMP_NORMAL:
            t->solt = ALLOCAT(ty, t->name); break;
            break;
        case TEMP_CONST:
            t->solt = ALLOCAT(ty, "");
            ST(CONSTT(ty, t->val), t->solt);
            break;
        }
    }

    sync_fastreg(l, false);

    buf = tb->packed_tcg.op_buf->data;
    buf_end = buf + tb->packed_tcg.op_buf->len;
    while (buf < buf_end) {
        LLVMValueRef next_tb = NULL;
        LLVMBasicBlockRef switch_bb_delay = NULL;
        TCGOpcode c;
        const TCGOpDef *def;
        int nb_oargs, nb_iargs, nb_cargs;
        uint64_t op_args[MAX_OPC_PARAM] = {};

        deserialize_op(&buf, &c, &nb_oargs, &nb_iargs, &nb_cargs, op_args);
        def = &tcg_op_defs[c];

        switch (c) {

/* op args */
#define ARG0 op_args[0]
#define ARG1 op_args[1]
#define ARG2 op_args[2]
#define ARG3 op_args[3]
#define ARG4 op_args[4]
#define ARG5 op_args[5]
/* L-value of op args */
#define ARG0L  l->temps[ARG0].solt
#define ARG1L  l->temps[ARG1].solt
#define ARG2L  l->temps[ARG2].solt
#define ARG3L  l->temps[ARG3].solt
#define ARG4L  l->temps[ARG4].solt
#define ARG5L  l->temps[ARG5].solt
/* R-value of op args */
#define ARG0R  LD(ARG0L)
#define ARG1R  LD(ARG1L)
#define ARG2R  LD(ARG2L)
#define ARG3R  LD(ARG3L)
#define ARG4R  LD(ARG4L)
#define ARG5R  LD(ARG5L)
/* Const-value of op args */
#define ARG0C  CONSTH(ARG0)
#define ARG1C  CONSTH(ARG1)
#define ARG2C  CONSTH(ARG2)
#define ARG3C  CONSTH(ARG3)
#define ARG4C  CONSTH(ARG4)
#define ARG5C  CONSTH(ARG5)
/* Basic block of op args */
#define ARG0BB get_label(l, ARG0)
#define ARG1BB get_label(l, ARG1)
#define ARG2BB get_label(l, ARG2)
#define ARG3BB get_label(l, ARG3)
#define ARG4BB get_label(l, ARG4)
#define ARG5BB get_label(l, ARG5)
/* store to op args */
#define ST0(v) ST(v, ARG0L)
#define ST1(v) ST(v, ARG1L)

        case INDEX_op_discard:
            ST0(LLVMGetUndef(ELETY(TYOF(ARG0L))));
            break;
        
#define OP_LD(src_bits, dst_bits, ext_kind) \
do { \
    if (!is_env(ARG1)) tcg_abort(); \
    ST0(EXTEND(LD(ENV(ARG2, INTTY(src_bits), "")), dst_bits, ext_kind)); \
} while (0)
        case INDEX_op_ld8u_i32:   OP_LD( 8, 32, Z); break;
        case INDEX_op_ld8s_i32:   OP_LD( 8, 32, S); break;
        case INDEX_op_ld16u_i32:  OP_LD(16, 32, Z); break;
        case INDEX_op_ld16s_i32:  OP_LD(16, 32, S); break;
        case INDEX_op_ld_i32:     OP_LD(32, 32, Z); break;
#if HBITS == 64
        case INDEX_op_ld8u_i64:   OP_LD( 8, 64, Z); break;
        case INDEX_op_ld8s_i64:   OP_LD( 8, 64, S); break;
        case INDEX_op_ld16u_i64:  OP_LD(16, 64, Z); break;
        case INDEX_op_ld16s_i64:  OP_LD(16, 64, S); break;
        case INDEX_op_ld32u_i64:  OP_LD(32, 64, Z); break;
        case INDEX_op_ld32s_i64:  OP_LD(32, 64, S); break;
        case INDEX_op_ld_i64:     OP_LD(64, 64, Z); break;
#endif

#define OP_ST(bits) \
do { \
    if (!is_env(ARG1)) tcg_abort(); \
    ST(TRUNC(ARG0R, bits), ENV(ARG2, INTTY(bits), "")); \
} while (0)
        case INDEX_op_st8_i32:   OP_ST( 8); break;
        case INDEX_op_st16_i32:  OP_ST(16); break;
        case INDEX_op_st_i32:    OP_ST(32); break;
#if HBITS == 64
        case INDEX_op_st8_i64:   OP_ST( 8); break;
        case INDEX_op_st16_i64:  OP_ST(16); break;
        case INDEX_op_st32_i64:  OP_ST(32); break;
        case INDEX_op_st_i64:    OP_ST(64); break;
#endif

        case INDEX_op_mov_i32:  ST0(TRUNC(ARG1R, 32)); break;
#if HBITS == 64
        case INDEX_op_mov_i64:  ST0(ARG1R); break;
#endif

        case INDEX_op_ext8s_i32:    ST0(TSEXT(ARG1R,  8, 32)); break;
        case INDEX_op_ext16s_i32:   ST0(TSEXT(ARG1R, 16, 32)); break;
        case INDEX_op_ext8u_i32:    ST0(TZEXT(ARG1R,  8, 32)); break;
        case INDEX_op_ext16u_i32:   ST0(TZEXT(ARG1R, 16, 32)); break;

#if HBITS == 64
        case INDEX_op_ext8s_i64:    ST0(TSEXT(ARG1R,  8, 64)); break;
        case INDEX_op_ext16s_i64:   ST0(TSEXT(ARG1R, 16, 64)); break;
        case INDEX_op_ext32s_i64:   ST0(TSEXT(ARG1R, 32, 64)); break;
        case INDEX_op_ext_i32_i64:  ST0(TSEXT(ARG1R, 32, 64)); break;
        case INDEX_op_ext8u_i64:    ST0(TZEXT(ARG1R,  8, 64)); break;
        case INDEX_op_ext16u_i64:   ST0(TZEXT(ARG1R, 16, 64)); break;
        case INDEX_op_ext32u_i64:   ST0(TZEXT(ARG1R, 32, 64)); break;
        case INDEX_op_extu_i32_i64: ST0(TZEXT(ARG1R, 32, 64)); break;
#endif


#define OP_ARITH2(op2) \
    ST0(CAT(LLVMBuild, op2)(l->bldr, ARG1R, ARG2R, ""))
#define OP_ARITH1(op1) \
    ST0(CAT(LLVMBuild, op1)(l->bldr, ARG1R, ""))
#define OP_ARITH12(op1, op2) \
    ST0( \
        CAT(LLVMBuild, op1)(l->bldr, \
            CAT(LLVMBuild, op2)(l->bldr, ARG1R, ARG2R, ""), \
            "" \
        ) \
    )
#define OP_ARITH21(op2, op1) \
    ST0( \
        CAT(LLVMBuild, op2)(l->bldr, \
            ARG1R, \
            CAT(LLVMBuild, op1)(l->bldr, ARG2R, ""), \
            "" \
        ) \
    )
#if HBITS == 64
#define CASE_ARITH(tcg_op, stmt) \
    case CAT3(INDEX_op_, tcg_op, _i32): stmt; break; \
    case CAT3(INDEX_op_, tcg_op, _i64): stmt; break;
#else
#define CASE_ARITH(tcg_op, stmt) \
    case CAT3(INDEX_op_, tcg_op, _i32): stmt; break;
#endif
        CASE_ARITH(add, OP_ARITH2(Add))
        CASE_ARITH(sub, OP_ARITH2(Sub))
        CASE_ARITH(neg, OP_ARITH1(Neg))
        CASE_ARITH(mul, OP_ARITH2(Mul))
        CASE_ARITH(and, OP_ARITH2(And))
        CASE_ARITH(or , OP_ARITH2(Or))
        CASE_ARITH(xor, OP_ARITH2(Xor))
        CASE_ARITH(shl, OP_ARITH2(Shl))
        CASE_ARITH(shr, OP_ARITH2(LShr))
        CASE_ARITH(sar, OP_ARITH2(AShr))
        CASE_ARITH(not, OP_ARITH1(Not))
        CASE_ARITH(andc, OP_ARITH21(And, Not))

#define OP_MUL2(bits, bits2x, mul_kind) \
do { \
    LLVMValueRef product = MUL( \
        EXTEND(ARG2R, bits2x, mul_kind), \
        EXTEND(ARG3R, bits2x, mul_kind) \
    ); \
    ST0(TRUNC(product, bits)); \
    ST1(TRUNC(LSHR(product, CONST(bits2x, bits)), bits)); \
} while (0)
        case INDEX_op_mulu2_i32:  OP_MUL2(32, 64, Z); break;
        case INDEX_op_muls2_i32:  OP_MUL2(32, 64, S); break;
#if HBITS == 64
        case INDEX_op_mulu2_i64:  OP_MUL2(64, 128, Z); break;
        case INDEX_op_muls2_i64:  OP_MUL2(64, 128, S); break;
#endif

#define OP_DIV2(bits, bits2x, div_kind) \
do { \
    LLVMValueRef a = OR( \
        ZEXT(ARG2R, bits2x), \
        SHL(ZEXT(ARG3R, bits2x), CONST(bits2x, bits)) \
    ); \
    LLVMValueRef b = ZEXT(ARG4R, bits2x); \
    ST0(TRUNC(DIV(a, b, div_kind), bits)); \
    ST1(TRUNC(REM(a, b, div_kind), bits)); \
} while (0)
        case INDEX_op_divu2_i32:  OP_DIV2(32, 64, U); break;
        case INDEX_op_div2_i32:   OP_DIV2(32, 64, S); break;
#if HBITS == 64
        case INDEX_op_divu2_i64:  OP_DIV2(64, 128, U); break;
        case INDEX_op_div2_i64:   OP_DIV2(64, 128, S); break;
#endif

#define OP_ADDSUB2(op, bits, bits2x) \
do { \
    LLVMValueRef a = OR( \
        ZEXT(ARG2R, bits2x), \
        SHL(ZEXT(ARG3R, bits2x), CONST(bits2x, bits)) \
    ); \
    LLVMValueRef b = OR( \
        ZEXT(ARG4R, bits2x), \
        SHL(ZEXT(ARG5R, bits2x), CONST(bits2x, bits)) \
    ); \
    LLVMValueRef result = op(a, b); \
    ST0(TRUNC(result, bits)); \
    ST1(TRUNC(LSHR(result, CONST(bits2x, bits)), bits)); \
} while (0)
        case INDEX_op_add2_i32:  OP_ADDSUB2(ADD, 32, 64); break;
        case INDEX_op_sub2_i32:  OP_ADDSUB2(SUB, 32, 64); break;
#if HBITS == 64
        case INDEX_op_add2_i64:  OP_ADDSUB2(ADD, 64, 128); break;
        case INDEX_op_sub2_i64:  OP_ADDSUB2(SUB, 64, 128); break;
#endif

#define OP_DEPOSIT(bits) \
do { \
    unsigned long long mask = ((1LL << ARG4) - 1) << ARG3; \
    ST0( \
        OR( \
            AND(ARG1R, CONST(bits, ~mask)), \
            AND(SHL(ARG2R, CONST(bits, ARG3)), CONST(bits, mask)) \
        ) \
    ); \
} while (0)
        case INDEX_op_deposit_i32:  OP_DEPOSIT(32); break;
#if HBITS == 64
        case INDEX_op_deposit_i64:  OP_DEPOSIT(64); break;
#endif

#define OP_EXTRACT(bits, shr_kind) \
    ST0( \
        SHR(\
            SHL(ARG1R, CONST(bits, bits - ARG2 - ARG3)), \
            CONST(bits, bits - ARG3), \
            shr_kind \
        ) \
    )
        case INDEX_op_extract_i32:  OP_EXTRACT(32, L); break;
        case INDEX_op_sextract_i32: OP_EXTRACT(32, A); break;
#if HBITS == 64
        case INDEX_op_extract_i64:  OP_EXTRACT(64, L); break;
        case INDEX_op_sextract_i64: OP_EXTRACT(64, A); break;
#endif

#define OP_EXTRACT2(bits) \
    ST0( \
        CALL( \
            INTRINSIC("llvm.fshr", INTTY(bits)), \
            ARG2R, ARG1R, CONST(bits, ARG3) \
        ) \
    )
        case INDEX_op_extract2_i32:  OP_EXTRACT2(32); break;
#if HBITS == 64
        case INDEX_op_extract2_i64:  OP_EXTRACT2(64); break;
#endif

#define OP_BSWAP(swap_bits, dst_bits) \
    ST0( \
        ZEXT( \
            CALL( \
                INTRINSIC("llvm.bswap", INTTY(swap_bits)), \
                TRUNC(ARG1R, swap_bits) \
            ), \
            dst_bits \
        ) \
    )
        case INDEX_op_bswap16_i32: OP_BSWAP(16, 32); break;
        case INDEX_op_bswap32_i32: OP_BSWAP(32, 32); break;
#if HBITS == 64
        case INDEX_op_bswap16_i64: OP_BSWAP(16, 64); break;
        case INDEX_op_bswap32_i64: OP_BSWAP(32, 64); break;
        case INDEX_op_bswap64_i64: OP_BSWAP(64, 64); break;
#endif

#define OP_ROT(name, bits) \
    ST0( \
        CALL( \
            INTRINSIC(name, INTTY(bits)), \
            ARG1R, ARG1R, ARG2R \
        ) \
    )
        case INDEX_op_rotl_i32: OP_ROT("llvm.fshl", 32); break;
        case INDEX_op_rotr_i32: OP_ROT("llvm.fshr", 32); break;
#if HBITS == 64
        case INDEX_op_rotl_i64: OP_ROT("llvm.fshl", 64); break;
        case INDEX_op_rotr_i64: OP_ROT("llvm.fshr", 64); break;
#endif

#define OP_CNTZERO(name, bits) \
    ST0( \
        SELECT( \
            INTCMP(ARG1R, CONST(bits, 0), TCG_COND_NE), \
            CALL( \
                INTRINSIC(name, INTTY(bits)), \
                ARG1R, CONST(1, 0) \
            ), \
            ARG2R \
        ) \
    )
        case INDEX_op_clz_i32: OP_CNTZERO("llvm.ctlz", 32); break;
        case INDEX_op_ctz_i32: OP_CNTZERO("llvm.cttz", 32); break;
#if HBITS == 64
        case INDEX_op_clz_i64: OP_CNTZERO("llvm.ctlz", 64); break;
        case INDEX_op_ctz_i64: OP_CNTZERO("llvm.cttz", 64); break;
#endif

        case INDEX_op_br: {
            LLVMBuildBr(l->bldr, ARG0BB);
            break;
        }
        case INDEX_op_set_label: {
            switch_bb(l, ARG0BB);
            break;
        }

#define OP_BRCOND(bits) \
do { \
    LLVMBasicBlockRef next_bb = NEWBB(""), branch_bb = ARG3BB; \
    CMPBR( \
        ARG0R, ARG1R, ARG2, \
        branch_bb, next_bb \
    ); \
    switch_bb(l, next_bb); \
} while (0)
        case INDEX_op_brcond_i32: OP_BRCOND(32); break;
#if HBITS == 64
        case INDEX_op_brcond_i64: OP_BRCOND(64); break;
#endif

#define OP_MOVCOND(bits) \
    ST0( \
        SELECT( \
            INTCMP(ARG1R, ARG2R, ARG5), \
            ARG3R, \
            ARG4R \
        ) \
    )
        case INDEX_op_movcond_i32: OP_MOVCOND(32); break;
#if HBITS == 64
        case INDEX_op_movcond_i64: OP_MOVCOND(64); break;
#endif

#define OP_SETCOND(bits) \
    ST0(ZEXT(INTCMP(ARG1R, ARG2R, ARG3), bits))
        case INDEX_op_setcond_i32: OP_SETCOND(32); break;
#if HBITS == 64
        case INDEX_op_setcond_i64: OP_SETCOND(64); break;
#endif

// XXX: memory barrier!!! and endian swap
#define OP_QEMU_LD_HELPER(src_bits, dst_bits, ext_kind) \
    ST0( \
        EXTEND( \
            CALL(get_qemu_ld(l, INTTY(src_bits)), ARG1R), \
            dst_bits, \
        ext_kind) \
    )
#define OP_QEMU_LD(bits) \
do { \
    tcg_debug_assert(guest_base == 0); \
    switch (get_memop(ARG2) & (MO_BSWAP | MO_SSIZE)) { \
    case MO_UB:   OP_QEMU_LD_HELPER( 8, bits, Z); break; \
    case MO_SB:   OP_QEMU_LD_HELPER( 8, bits, S); break; \
    case MO_LEUW: OP_QEMU_LD_HELPER(16, bits, Z); break; \
    case MO_LESW: OP_QEMU_LD_HELPER(16, bits, S); break; \
    case MO_LEUL: OP_QEMU_LD_HELPER(32, bits, Z); break; \
    case MO_LESL: OP_QEMU_LD_HELPER(32, bits, S); break; \
    case MO_LEQ:  OP_QEMU_LD_HELPER(64, bits, Z); break; \
    default: tcg_abort(); \
    } \
} while (0)
        case INDEX_op_qemu_ld_i32: OP_QEMU_LD(32); break;
            
#if HBITS == 64
        case INDEX_op_qemu_ld_i64: OP_QEMU_LD(64); break;
#endif

#define OP_QEMU_ST_HELPER(bits) \
    CALL(get_qemu_st(l, INTTY(bits)), TRUNC(ARG0R, bits), ARG1R)
        case INDEX_op_qemu_st_i32:
#if HBITS == 64
        case INDEX_op_qemu_st_i64:
#endif
            tcg_debug_assert(guest_base == 0);
            switch (get_memop(ARG2) & (MO_BSWAP | MO_SSIZE)) {
            case MO_UB:   OP_QEMU_ST_HELPER( 8); break;
            case MO_LEUW: OP_QEMU_ST_HELPER(16); break;
            case MO_LEUL: OP_QEMU_ST_HELPER(32); break;
            case MO_LEQ:  OP_QEMU_ST_HELPER(64); break;
            default: tcg_abort();
            }
            break;

        case INDEX_op_call: {
            int i;
            LLVMTypeRef ret_ty;
            LLVMTypeRef args_ty[MAX_OPC_PARAM_IARGS];
            LLVMTypeRef fn_ty;
            LLVMValueRef args[MAX_OPC_PARAM_IARGS];
            LLVMValueRef fn, result;
            int helper_idx;

            helper_idx = op_args[nb_oargs + nb_iargs];

            if (nb_oargs > 1) {
                tcg_abort(); /* TODO */
            }
            
            for (i = 0; i < nb_iargs; i++) {
                LLVMValueRef lval = l->temps[op_args[nb_oargs + i]].solt;
                args_ty[i] = ELETY(TYOF(lval));
                args[i] = LD(lval);
            }
            
            ret_ty = nb_oargs ? ELETY(TYOF(ARG0L)) : VOIDTY;
            fn_ty = LLVMFunctionType(ret_ty, args_ty, nb_iargs, 0);
            fn = LLVMBuildBitCast(l->bldr,
                get_helper(l, helper_idx), PTRTY(fn_ty), "");

            result = LLVMBuildCall2(l->bldr, fn_ty, fn, args, nb_iargs, "");
            set_helper_call_attr(l, result, helper_idx);
            if (nb_oargs) {
                ST0(result);
            }
            break;
        }

        case INDEX_op_insn_start: break;

        case INDEX_op_goto_tb: {
            target_ulong next_pc = ARG1;
            next_tb = get_tb_func(l, next_pc);
            switch_bb_delay = NEWBB("dead");
            goto chain_next_tb;
        }
        case INDEX_op_goto_ptr: {
            next_tb = CALL(get_map_pc(l), l->env);
            goto chain_next_tb;
        }
        case INDEX_op_exit_tb: {
            next_tb = get_epilogue(l);
            goto chain_next_tb;
        }
        chain_next_tb: {
            int i;
            LLVMValueRef args[l->nb_fastreg + 1];
            LLVMValueRef result;
            for (i = 0; i < l->nb_fastreg; i++) {
                args[i] = LD(ENV(l->regmap[i].off, l->regmap[i].ty, ""));
            }
            args[l->nb_fastreg] = l->env;
            sync_fastreg(l, true);
            result = LLVMBuildCall2(l->bldr,
                l->tb_type, next_tb, args, l->nb_fastreg + 1, "");
            set_tb_call_attr(l, result);
            LLVMBuildRetVoid(l->bldr);
            break;
        }


        default:
            qemu_log("TODO: %s\n", def->name);
            /*dump_module(l->mod);*/
            tcg_abort();
            break;
        }

        if (switch_bb_delay) {
            switch_bb(l, switch_bb_delay);
        }
    }

    //dump_module(l->mod);
    LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
}

#define PC2FUNC_HASHSEED 17777  /* use a prime */
static __thread struct {
    target_ulong pc;
    void *func;
} map_pc_hashtbl[PC2FUNC_HASHSEED];
#define map_pc_item(pc) map_pc_hashtbl[(pc) % PC2FUNC_HASHSEED]

static void *map_pc(CPUArchState *env)
{
    TCGLLVMContext *l = tcg_ctx->llvm_ctx;
    target_ulong cs_base, pc;
    uint32_t flags;
    cpu_get_tb_cpu_state(env, &pc, &cs_base, &flags);
    if (likely(map_pc_item(pc).pc == pc)) {
        return map_pc_item(pc).func;
    } else {
        return l->epilogue;
    }
}

/* return 0 if not cached
   return -1 if has fwdptr
   return 1 if cached */
static int load_cache_meta(TCGLLVMContext *l, target_ulong pc)
{
    LLVMOrcJITTargetAddress dummy;
    char symname[MAX_SYMNAME];
    int ret = 0;

    /* must cache result to avoid result changing */
    if (g_hash_table_contains(l->cache_meta, GUINT_TO_POINTER(pc))) {
        return GPOINTER_TO_INT(
            g_hash_table_lookup(l->cache_meta, GUINT_TO_POINTER(pc)));
    }

    fmt_tb_symbol(symname, "fwdptr", pc);
    if (LLVMOrcLLJITLookup(l->jit, &dummy, symname) == 0) {
        /* has fwdptr */
        g_hash_table_add(l->tb_fwdptrs, (void *)(uintptr_t)pc);
        ret = -1;
    } else {
        fmt_tb_symbol(symname, "tb", pc);
        if (LLVMOrcLLJITLookup(l->jit, &dummy, symname) == 0) {
            /* in cache */
            g_hash_table_add(l->tb_compiled, (void *)(uintptr_t)pc);
            ret = 1;
        }
    }
    g_hash_table_insert(l->cache_meta,
        GUINT_TO_POINTER(pc), GINT_TO_POINTER(ret));
    return ret;
}

static void batch_compile(TCGLLVMContext *l)
{
    LLVMOrcThreadSafeModuleRef tsm;
    LLVMOrcJITTargetAddress func_addr, fwdptr_addr;
    char symname[MAX_SYMNAME];
    unsigned long i;

    qemu_log("batch compile!\n");

    unsigned long tb_cached = 0;
    for (i = 0; i < l->hot_tb->len; i++) {
        TranslationBlock *tb = g_ptr_array_index(l->hot_tb, i);
        if (load_cache_meta(l, tb->pc) > 0) {
            tb_cached++;
        }
    }

    if (l->hot_tb->len > tb_cached) {

        for (i = 0; i < l->hot_tb->len; i++) {
            TranslationBlock *tb = g_ptr_array_index(l->hot_tb, i);
            g_hash_table_add(l->tb_compiled, (void *)(uintptr_t)tb->pc);
        }

        l->mod = LLVMModuleCreateWithNameInContext("jit", l->ctx);
        g_array_set_size(l->tb_fwdrefs, 0);

        for (i = 0; i < l->hot_tb->len; i++) {
            TranslationBlock *tb = g_ptr_array_index(l->hot_tb, i);
            if (load_cache_meta(l, tb->pc) <= 0) {
                gen_code(l, tb);
            }
        }

        qemu_log("LLVMRunPassManager bgein! (n=%u)\n", l->hot_tb->len);
        //dump_module(l->mod);
        define_stub(l);
        if (1) {
            static int dumpid = 0;
            char dumpf[1000];
            do {
                sprintf(dumpf, TCG_LLVM_CACHE_DIR "/dump%d.ll", dumpid++);
            } while (access(dumpf, F_OK) == 0);
            write_module(l->mod, dumpf);
        }
        internalize_tb_placeholder(l);
        //dump_module(l->mod);
        LLVMRunPassManager(l->mpm_O2inline, l->mod);
        //dump_module(l->mod);
        define_qemu_ld_st(l, true);
        LLVMRunPassManager(l->mpm_alwaysinline, l->mod);
        LLVMRunPassManager(l->mpm_O2, l->mod);
        //dump_module(l->mod);
        qemu_log("LLVMRunPassManager end!\n");

        
        tsm = LLVMOrcCreateNewThreadSafeModule(l->mod, l->tsctx);
        check_error(LLVMOrcLLJITAddLLVMIRModule(l->jit, l->jd, tsm));

        for (i = 0; i < l->tb_fwdrefs->len; i++) {
            target_ulong pc = g_array_index(l->tb_fwdrefs, target_ulong, i);
            g_hash_table_add(l->tb_fwdptrs, (void *)(uintptr_t)pc);
        }
    }

    for (i = 0; i < l->hot_tb->len; i++) {
        TranslationBlock *tb = g_ptr_array_index(l->hot_tb, i);

        fmt_tb_symbol(symname, "tb", tb->pc);
        check_error(LLVMOrcLLJITLookup(l->jit, &func_addr, symname));
        tb->llvm_tc = (void *)func_addr;

        if (g_hash_table_contains(l->tb_fwdptrs, (void *)(uintptr_t)tb->pc)) {
            fmt_tb_symbol(symname, "fwdptr", tb->pc);
            check_error(LLVMOrcLLJITLookup(l->jit, &fwdptr_addr, symname));
            *(void **)fwdptr_addr = (void *)func_addr;
            g_hash_table_remove(l->tb_fwdptrs, (void *)(uintptr_t)tb->pc);
        }

        fmt_tb_symbol(symname, "tb", tb->pc);
        qemu_log("%s = %p; cached = %d; %" PRIu64 "; %s\n", symname, tb->llvm_tc, load_cache_meta(l, tb->pc), tb->exec_count, tb->packed_tcg.digest);
        /*unsigned char *ptr = tb->packed_tcg.op_buf->data;
        int len = tb->packed_tcg.op_buf->len;
        for (int p = 0; p < len; p++) {
            if (p % 16 == 0) {
                if (p) qemu_log("\n");
                qemu_log("%04x: ", p);
            }
            qemu_log("%02x ", ptr[p]);
        }
        qemu_log("\n");*/
    }

    qemu_log("compile done! (%lu compiled, %lu cached)\n", l->hot_tb->len - tb_cached, tb_cached);
    g_ptr_array_set_size(l->hot_tb, 0);
}

bool tcg_llvm_try_exec_tb(TCGContext *s, TranslationBlock *tb,
    CPUArchState *env, uintptr_t *ret)
{
    TCGLLVMContext *l = s->llvm_ctx;
    if (!tb->llvm_tc) {
        if (tb->exec_count == l->hot_limit1) {
            g_ptr_array_add(l->hot_tb, tb);
        }
        if (tb->exec_count < l->hot_limit2) {
            tb->exec_count++;
            return false;
        }
        batch_compile(l);
    }
    /*if (map_pc_item(tb->pc).pc && map_pc_item(tb->pc).pc != tb->pc) {
        qemu_log("pc hash conflit: %lu %lu\n", map_pc_item(tb->pc).pc, tb->pc);
    }*/
    map_pc_item(tb->pc).pc = tb->pc;
    map_pc_item(tb->pc).func = tb->llvm_tc;
    //qemu_log("llvm exec! begin\n");
    l->ret_pc = 0;
    l->prologue(tb->llvm_tc, env);
    if (l->ret_pc) {
        CPUState *cpu = env_cpu(env);
        CPUClass *cc = CPU_GET_CLASS(cpu);
        cc->set_pc(cpu, l->ret_pc);
    }
    *ret = 0;
    //qemu_log("pc=%p cc_src=%p\n", (void *)tb->pc, (void *)env->cc_src);
    //qemu_log("llvm exec! done; ret=%p\n", (void *)*ret);
    return true;
}

void tcg_llvm_init_tb(TCGContext *s, TranslationBlock *tb)
{
    tb->llvm_tc = NULL;
    tb->exec_count = 0;
    tb->packed_tcg.temp_buf = g_byte_array_new();
    tb->packed_tcg.op_buf = g_byte_array_new();
    tb->packed_tcg.digest = NULL;
}
void tcg_llvm_remove_tb(TCGContext *s, TranslationBlock *tb)
{
    qemu_log("tb %p removed!\n", tb);
    /* TODO: free tb->llvm_tc */
    g_byte_array_free(tb->packed_tcg.temp_buf, TRUE);
    g_byte_array_free(tb->packed_tcg.op_buf, TRUE);
    g_free(tb->packed_tcg.digest);
}

static void add_absolute_symbol(TCGLLVMContext *l, const char *name, void *addr)
{
    LLVMJITCSymbolMapPair sym;
    memset(&sym, 0, sizeof(sym));
    sym.Name = LLVMOrcExecutionSessionIntern(l->es, name);
    sym.Sym.Address = (LLVMOrcJITTargetAddress) addr;
    g_array_append_val(l->abssym, sym);
}
static void commit_absolute_symbol(TCGLLVMContext *l)
{
    check_error(LLVMOrcJITDylibDefine(l->jd,
        LLVMOrcAbsoluteSymbols((void *) l->abssym->data, l->abssym->len)));
    g_array_set_size(l->abssym, 0);
}

static void init_helpers(TCGLLVMContext *l)
{
    int idx;
    char symname[MAX_SYMNAME];
    for (idx = 0; all_helpers[idx].func; idx++) {
        fmt_helper_symbol(symname, idx);
        add_absolute_symbol(l, symname, all_helpers[idx].func);
    }
}

static void init_prologue(TCGLLVMContext *l)
{
    int i;
    LLVMValueRef args[l->nb_fastreg + 1];
    LLVMValueRef prologue_call;
    LLVMOrcThreadSafeModuleRef tsm;
    LLVMOrcJITTargetAddress func_addr;

    l->mod = LLVMModuleCreateWithNameInContext("prologue", l->ctx);

    l->fn = LLVMAddFunction(l->mod, "prologue", 
        FUNCTY(VOIDTY, PTRTY(INTTY(8)), PTRTY(l->env_ty)));
    LLVMPositionBuilderAtEnd(l->bldr, 
        LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));
    l->env = PARAM(1);
    for (i = 0; i < l->nb_fastreg; i++) {
        args[i] = LD(ENV(l->regmap[i].off, l->regmap[i].ty, ""));
    }
    args[l->nb_fastreg] = l->env;
    prologue_call = LLVMBuildCall2(l->bldr,
        l->tb_type,
        LLVMBuildBitCast(l->bldr, PARAM(0), PTRTY(l->tb_type), ""),
        args, l->nb_fastreg + 1, "");
    set_tb_call_attr(l, prologue_call);
    QLLVMSetMustTailCall(prologue_call, 0);
    LLVMBuildRetVoid(l->bldr);
    LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
    LLVMSetVisibility(l->fn, LLVMHiddenVisibility);

    l->fn = get_epilogue(l);
    LLVMPositionBuilderAtEnd(l->bldr,
        LLVMAppendBasicBlockInContext(l->ctx, l->fn, "entry"));
    l->env = PARAM(l->nb_fastreg);
    sync_fastreg(l, false);
    LLVMBuildRetVoid(l->bldr);
    LLVMVerifyFunction(l->fn, LLVMAbortProcessAction);
    LLVMSetVisibility(l->fn, LLVMHiddenVisibility);

    get_qemu_ld(l, INTTY( 8)); get_qemu_st(l, INTTY( 8));
    get_qemu_ld(l, INTTY(16)); get_qemu_st(l, INTTY(16));
    get_qemu_ld(l, INTTY(32)); get_qemu_st(l, INTTY(32));
    get_qemu_ld(l, INTTY(64)); get_qemu_st(l, INTTY(64));
    define_qemu_ld_st(l, false);
    
    LLVMRunPassManager(l->mpm_O2, l->mod);    
    //dump_module(l->mod);
    write_module(l->mod, TCG_LLVM_CACHE_DIR "/prologue.ll");
    
    tsm = LLVMOrcCreateNewThreadSafeModule(l->mod, l->tsctx);
    check_error(LLVMOrcLLJITAddLLVMIRModule(l->jit, l->jd, tsm));

    

    check_error(LLVMOrcLLJITLookup(l->jit, &func_addr, "prologue"));
    l->prologue = (void *)func_addr;
    //log_disas((void *)func_addr, 50);
    check_error(LLVMOrcLLJITLookup(l->jit, &func_addr, "epilogue"));
    l->epilogue = (void *)func_addr;
    //log_disas((void *)func_addr, 50);

}

static LLVMOrcObjectLayerRef create_oll_with_jitperf(
    void *ctx, LLVMOrcExecutionSessionRef es, const char *triple)
{
    /*TCGLLVMContext *l = Ctx;*/
    LLVMOrcObjectLayerRef ol;
    LLVMJITEventListenerRef perf;
    ol = LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es);
    perf = LLVMCreatePerfJITEventListener();
    if (!perf) {
        tcg_abort(); /* LLVM need to be compiled with LLVM_USE_PERF=On */
    }
    LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener(ol, perf);
    return ol;
}

void tcg_llvm_context_init(TCGContext *s)
{
    int i;
    LLVMOrcLLJITBuilderRef jb = NULL;
    LLVMPassManagerBuilderRef pmb;
    LLVMTypeRef *tb_args;
    TCGLLVMContext *l = g_malloc0(sizeof(*l));
    s->llvm_ctx = l;
    
    if (1) { /* use jit perf */
        jb = LLVMOrcCreateLLJITBuilder();
        LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(jb, 
            create_oll_with_jitperf, l);
    }

    check_error(LLVMOrcCreateLLJIT(&l->jit, jb));
    l->tsctx = LLVMOrcCreateNewThreadSafeContext();
    l->ctx = LLVMOrcThreadSafeContextGetContext(l->tsctx);
    l->bldr = LLVMCreateBuilderInContext(l->ctx);
    l->jd = LLVMOrcLLJITGetMainJITDylib(l->jit);
    l->es = LLVMOrcLLJITGetExecutionSession(l->jit);

    l->mpm_O2 = LLVMCreatePassManager();
    pmb = LLVMPassManagerBuilderCreate();
    LLVMPassManagerBuilderSetOptLevel(pmb, 2);
    LLVMPassManagerBuilderPopulateModulePassManager(pmb, l->mpm_O2);
    LLVMPassManagerBuilderDispose(pmb);

    l->mpm_O2inline = LLVMCreatePassManager();
    pmb = LLVMPassManagerBuilderCreate();
    LLVMPassManagerBuilderSetOptLevel(pmb, 2);
    LLVMPassManagerBuilderUseInlinerWithThreshold(pmb, 100000);
    LLVMPassManagerBuilderPopulateModulePassManager(pmb, l->mpm_O2inline);
    LLVMPassManagerBuilderDispose(pmb);

    l->mpm_alwaysinline = LLVMCreatePassManager();
    LLVMAddAlwaysInlinerPass(l->mpm_alwaysinline);
    

#define ATTR_KINDID(s) LLVMGetEnumAttributeKindForName(s, strlen(s))
    l->attr_noalias = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("noalias"), 0);
    l->attr_vaildenv = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("dereferenceable"), sizeof(CPUArchState));
    l->attr_nounwind = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("nounwind"), 0);
    l->attr_alwaysinline = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("alwaysinline"), 0);
    l->attr_readnone = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("readnone"), 0);
    l->attr_readonly = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("readonly"), 0);
    l->attr_inaccessiblememonly = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("inaccessiblememonly"), 0);
    l->attr_willreturn = LLVMCreateEnumAttribute(l->ctx,
        ATTR_KINDID("willreturn"), 0);
    
#define MD_KINDID(s) LLVMGetMDKindIDInContext(l->ctx, s, strlen(s))
    l->md_prof = MD_KINDID("prof");

#define MD_STRING(s) LLVMMDStringInContext2(l->ctx, s, strlen(s))
#define MD_NODE(...) ({ \
    LLVMMetadataRef __md_node_args[] = {__VA_ARGS__}; \
    LLVMMDNodeInContext2(l->ctx, __md_node_args, ARRAY_SIZE(__md_node_args)); \
})
#define MD2VAL(md) LLVMMetadataAsValue(l->ctx, md)
#define VAL2MD(val) LLVMValueAsMetadata(val)
    l->prof_likely = MD2VAL(MD_NODE(MD_STRING("branch_weights"),
        VAL2MD(CONST(32, 2000)), VAL2MD(CONST(32, 1))));

    l->hasher = g_checksum_new(G_CHECKSUM_SHA256);
    l->labels = g_hash_table_new(NULL, NULL);

    l->tb_compiled = g_hash_table_new(NULL, NULL);
    l->tb_fwdptrs = g_hash_table_new(NULL, NULL);
    l->tb_fwdrefs = g_array_new(FALSE, FALSE, sizeof(target_ulong));

    l->hot_tb = g_ptr_array_new();
    l->hot_limit1 = 1000;
    l->hot_limit2 = 10000;

    l->env_ty = LLVMArrayType(INTTY(8), sizeof(CPUArchState));

    l->fn_qemu_ld = g_ptr_array_new();
    l->fn_qemu_st = g_ptr_array_new();
    l->fn_dummy = g_ptr_array_new();

    l->tb_callconv = LLVMFastCallConv;
    l->nb_fastreg = 0;

    l->regmap = g_malloc_n(l->nb_fastreg, sizeof(*l->regmap));
    for (i = 0; i < l->nb_fastreg; i++) {
        l->regmap[i].off = i * (GBITS / 8),
        l->regmap[i].ty = INTTY(GBITS);
    }
    /*l->regmap[5].off = 152;
    l->regmap[6].off = 144;
    l->regmap[7].off = 168; l->regmap[7].ty = INTTY(32); */
    tb_args = alloca((l->nb_fastreg + 1) * sizeof(LLVMTypeRef));
    for (i = 0; i < l->nb_fastreg; i++) {
        tb_args[i] = l->regmap[i].ty;
    }
    tb_args[l->nb_fastreg] = PTRTY(l->env_ty);
    l->tb_type = LLVMFunctionType(VOIDTY, tb_args, l->nb_fastreg + 1, 0);



    l->abssym = g_array_new(FALSE, TRUE, sizeof(LLVMJITCSymbolMapPair));

    init_prologue(l);
    init_helpers(l);
    add_absolute_symbol(l, "ret_pc", &l->ret_pc);
    add_absolute_symbol(l, "map_pc", map_pc);
    commit_absolute_symbol(l);
    
    const char *cache_file = TCG_LLVM_CACHE_DIR "/cache.o";
    l->use_cache = (access(cache_file, F_OK) == 0);
    l->cache_meta = g_hash_table_new(NULL, NULL);
    if (l->use_cache) {
        char *msg = NULL;
        LLVMMemoryBufferRef membuf = NULL;
        if (LLVMCreateMemoryBufferWithContentsOfFile(cache_file, &membuf, &msg)) {
            qemu_log("%s\n", msg);
            LLVMDisposeMessage(msg);
            tcg_abort();
        }
        check_error(LLVMOrcLLJITAddObjectFile(l->jit, l->jd, membuf));
    }
    qemu_log("init ok! use_cache=%d\n", l->use_cache);
}

void tcg_llvm_init(void)
{
    const char *llvm_argv[] = {
        "qemu", /* argv[0] is program name */
        "-dse-memoryssa-walklimit=10000",
        //"--help-hidden",
    };
    LLVMParseCommandLineOptions(ARRAY_SIZE(llvm_argv), llvm_argv, "");

    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();
    LLVMInitializeNativeAsmParser();
}