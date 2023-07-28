/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *   This file implements the trace/region formation algorithm.
 */


#include "utils.h"
#include "tracer.h"
#include "llvm-state.h"

#define USE_RELAXED_NET


unsigned ProfileThreshold = NET_PROFILE_THRESHOLD;
unsigned PredictThreshold = NET_PREDICT_THRESHOLD;

static inline void start_trace_profiling(TranslationBlock *tb)
{
    /* Turn on trace profiling by jumping to the next instruction. */
    uintptr_t jmp_addr = tb_get_jmp_entry(tb);
#if defined(TCG_TARGET_I386)
    patch_jmp(jmp_addr, jmp_addr + 5);
#elif defined(TCG_TARGET_ARM) || defined(TCG_TARGET_AARCH64)
    patch_jmp(jmp_addr, jmp_addr + 4);
#elif defined(TCG_TARGET_PPC64)
    patch_jmp(jmp_addr, jmp_addr + 16);
#endif
}

static inline void copy_image(CPUArchState *env, TranslationBlock *tb)
{
#if defined(CONFIG_LLVM) && defined(CONFIG_SOFTMMU)
    char *p = new char[tb->size];
    for (int i = 0, e = tb->size; i != e; ++i)
        p[i] = cpu_ldub_code(env, tb->pc + i);
    tb->image = (void *)p;
#endif
}

static inline void tracer_handle_chaining(uintptr_t next_tb, TranslationBlock *tb)
{
#if defined(CONFIG_LLVM)
    llvm_handle_chaining(next_tb, tb);
#else
    /* see if we can patch the calling TB. When the TB spans two pages, we
     * cannot safely do a direct jump. */
    if (next_tb != 0 && tb->page_addr[1] == (tb_page_addr_t)-1
        && !qemu_loglevel_mask(CPU_LOG_TB_NOCHAIN)) {
        tb_add_jump((TranslationBlock *)(next_tb & ~TB_EXIT_MASK),
                    next_tb & TB_EXIT_MASK, tb);
    }
#endif
}


#if defined(CONFIG_LLVM)
#include "llvm.h"
#include "llvm-soft-perfmon.h"
#include "llvm-hard-perfmon.h"
static inline void OptimizeBlock(CPUArchState *env, TranslationBlock *TB)
{
    auto Request = OptimizationInfo::CreateRequest(TB);
    LLVMEnv::OptimizeBlock(env, std::move(Request));
}
static inline void OptimizeTrace(CPUArchState *env, NETTracer::TBVec &TBs,
                                 int LoopHeadIdx)
{
    auto Request = OptimizationInfo::CreateRequest(TBs, LoopHeadIdx);
    LLVMEnv::OptimizeTrace(env, std::move(Request));
}
static inline void RegisterThread(CPUArchState *env, BaseTracer *tracer)
{
    if (ENV_GET_CPU(env)->cpu_index < 0)
        return;
    HP->RegisterThread(tracer);
}
static inline void UnregisterThread(CPUArchState *env, BaseTracer *tracer)
{
    if (ENV_GET_CPU(env)->cpu_index < 0)
        return;
    HP->UnregisterThread(tracer);
    SP->NumTraceExits += env->num_trace_exits;
}
static inline void NotifyCacheEnter(CPUArchState *env)
{
    if (ENV_GET_CPU(env)->cpu_index < 0)
        return;
    HP->NotifyCacheEnter(cpu_get_tracer(env));
}
static inline void NotifyCacheLeave(CPUArchState *env)
{
    if (ENV_GET_CPU(env)->cpu_index < 0)
        return;
    HP->NotifyCacheLeave(cpu_get_tracer(env));
}
#else
static inline void OptimizeBlock(CPUArchState *, TranslationBlock *) {}
static inline void OptimizeTrace(CPUArchState *, NETTracer::TBVec &, int) {}
static inline void RegisterThread(CPUArchState *, BaseTracer *) {}
static inline void UnregisterThread(CPUArchState *, BaseTracer *) {}
static inline void NotifyCacheEnter(CPUArchState *) {}
static inline void NotifyCacheLeave(CPUArchState *) {}
#endif


/*
 * BaseTracer
 */
BaseTracer *BaseTracer::CreateTracer(CPUArchState *env)
{
#if defined(CONFIG_LLVM)
    switch (LLVMEnv::TransMode) {
        case TRANS_MODE_NONE:
            return new BaseTracer(env);
        case TRANS_MODE_BLOCK:
            return new SingleBlockTracer(env);
        case TRANS_MODE_HYBRIDS:
            return new NETTracer(env, TRANS_MODE_HYBRIDS);
        case TRANS_MODE_HYBRIDM:
            return new NETTracer(env, TRANS_MODE_HYBRIDM);
        default:
            break;
    }
#endif
    return new BaseTracer(env);
}

void BaseTracer::DeleteTracer(CPUArchState *env)
{
    auto Tracer = cpu_get_tracer(env);
    if (Tracer) {
        delete Tracer;
        Tracer = nullptr;
    }
}


/*
 * SingleBlockTracer
 */
SingleBlockTracer::SingleBlockTracer(CPUArchState *env) : BaseTracer(env)
{
    if (tracer_mode == TRANS_MODE_NONE)
        tracer_mode = TRANS_MODE_BLOCK;
}

void SingleBlockTracer::Record(uintptr_t next_tb, TranslationBlock *tb)
{
    /* Optimize the block if we see this block for the first time. */
    if (update_tb_mode(tb, BLOCK_NONE, BLOCK_ACTIVE))
        OptimizeBlock(Env, tb);
    TB = tb;
}


/*
 * NETTracer
 */
NETTracer::NETTracer(CPUArchState *env, int Mode) : BaseTracer(env)
{
    if (tracer_mode == TRANS_MODE_NONE)
        tracer_mode = Mode;
    RegisterThread(Env, this);
}

NETTracer::~NETTracer()
{
    UnregisterThread(Env, this);
}

void NETTracer::Reset()
{
    TBs.clear();
    Env->start_trace_prediction = 0;
}

void NETTracer::Record(uintptr_t next_tb, TranslationBlock *tb)
{
    bool NewTB = (tb->mode == BLOCK_NONE);

    /* Promote tb to the active state before any checks if it is a new tb. */
    if (update_tb_mode(tb, BLOCK_NONE, BLOCK_ACTIVE)) {
        tcg_save_state(Env, tb);
        copy_image(Env, tb);
    }

    if (isTraceHead(next_tb, tb, NewTB)) {
        if (update_tb_mode(tb, BLOCK_ACTIVE, BLOCK_TRACEHEAD))
            start_trace_profiling(tb);
    }

    Env->fallthrough = 0;
}

/* Determine whether tb is a potential trace head. tb is a trace head if it is
 * (1) a target of an existing trace exit,
 * (2) a target of an indirect branch,
 * (3) (relaxed  NET) a block in a cyclic path (i.e., seen more than once), or
 *     (original NET) a target of a backward branch. */
bool NETTracer::isTraceHead(uintptr_t next_tb, TranslationBlock *tb, bool NewTB)
{
    /* Rule 1: a target of an existing trace exit. */
    if ((next_tb & TB_EXIT_MASK) == TB_EXIT_LLVM)
        return true;

    /* Rule 2: a target of an indirect branch.
     * Here we check 'next_tb == 0', which can cover the cases other than the
     * indirect branches (e.g., system calls and exceptions). It is fine to
     * also start trace formation from the successors of these blocks. */
    if (next_tb == 0 && Env->fallthrough == 0)
        return true;

#ifdef USE_RELAXED_NET
    /* Rule 3: a block in a cyclic path (i.e., seen more than once). */
    if (!NewTB)
        return true;
#else
    /* Rule 3: a target of a backward branch. */
    if (next_tb != 0) {
        TranslationBlock *pred = (TranslationBlock *)(next_tb & ~TB_EXIT_MASK);
        if (tb->pc <= pred->pc)
            return true;
    }
#endif
    return false;
}

void NETTracer::Profile(TranslationBlock *tb)
{
    if (Atomic<uint32_t>::inc_return(&tb->exec_count) != ProfileThreshold)
        return;

#if 0
    /* If the execution is already in the prediction mode, process the
     * previously recorded trace. */
    if (Env->start_trace_prediction && !TBs.empty()) {
        OptimizeTrace(Env, TBs, -1);
        Reset();
    }
#endif

    /* We reach a profile threshold, stop trace profiling and start trace tail
     * prediction. The profiling is disabled by setting the jump directly to
     * trace prediction stub. */
    patch_jmp(tb_get_jmp_entry(tb), tb_get_jmp_next(tb));
    Env->start_trace_prediction = 1;
}

void NETTracer::Predict(TranslationBlock *tb)
{
    /* The trace prediction will terminate if a cyclic path is detected.
     * (i.e., current tb has existed in the tracing butter either in the
     * head or middle of the buffer.) */
    int LoopHeadIdx = -1;

#if defined(CONFIG_LLVM)
    /* Skip this trace if the next block is an annotated loop head and
     * is going to be included in the middle of a trace. */
    if (!TBs.empty() && TBs[0] != tb &&
        llvm_has_annotation(tb->pc, ANNOTATION_LOOP)) {
        goto trace_building;
    }
#endif

#if defined(USE_TRACETREE_ONLY)
    /* We would like to have a straight-line or O-shape trace.
     * (the 6-shape trace is excluded) */
    if (!TBs.empty() && tb == TBs[0]) {
        LoopHeadIdx = 0;
        goto trace_building;
    }
#elif defined(USE_RELAXED_NET)
    /* Find any cyclic path in recently recorded blocks. */
    for (int i = 0, e = TBs.size(); i != e; ++i) {
        if (tb == TBs[i]) {
            LoopHeadIdx = i;
            goto trace_building;
        }
    }
#else
    if (!TBs.empty()) {
        if (tb == TBs[0]) {
            /* Cyclic path. */
            LoopHeadIdx = 0;
            goto trace_building;
        }
        if (tb->pc <= TBs[TBs.size() - 1]->pc) {
            /* Backward branch. */
            goto trace_building;
        }
    }
#endif

    TBs.push_back(tb);

    /* Stop if the maximum prediction length is reached. */
    if (TBs.size() == PredictThreshold)
        goto trace_building;

    return;

trace_building:
    /* If the trace is a loop with a branch to the middle of the loop body,
     * we forms two sub-traces: (1) the loop starting from the loopback to
     * the end of the trace and (2) the original trace. */
    /* NOTE: We want to find more traces so the original trace is included. */

    if (LoopHeadIdx > 0) {
        /* Loopback at the middle. The sub-trace (1) is optimized first. */
        TBVec Loop(TBs.begin() + LoopHeadIdx, TBs.end());
        update_tb_mode(Loop[0], BLOCK_ACTIVE, BLOCK_TRACEHEAD);
        OptimizeTrace(Env, Loop, 0);
    }
    OptimizeTrace(Env, TBs, LoopHeadIdx);

    Reset();
}


/* The follows implement routines of the C interfaces for QEMU. */
extern "C" {

int tracer_mode = TRANS_MODE_NONE;

void tracer_reset(CPUArchState *env)
{
    auto Tracer = cpu_get_tracer(env);
    Tracer->Reset();
}

/* This routine is called when QEMU is going to leave the dispatcher and enter
 * the code cache to execute block code `tb'. Here, we determine whether tb is
 * a potential trace head and should perform trace formation. */
void tracer_exec_tb(CPUArchState *env, uintptr_t next_tb, TranslationBlock *tb)
{
    auto Tracer = cpu_get_tracer(env);
    Tracer->Record(next_tb, tb);

    tracer_handle_chaining(next_tb, tb);
}


/* Helper function to perform trace profiling. */
void helper_NET_profile(CPUArchState *env, int id)
{
    auto &Tracer = getNETTracer(env);
    Tracer.Profile(&tbs[id]);
}

/* Helper function to perform trace prediction. */
void helper_NET_predict(CPUArchState *env, int id)
{
    auto &Tracer = getNETTracer(env);
    Tracer.Predict(&tbs[id]);
}

} /* extern "C" */



/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
