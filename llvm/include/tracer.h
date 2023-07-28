/*
 *  (C) 2016 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __TRACE_H
#define __TRACE_H

#include <vector>
#include <iostream>
#include "qemu-types.h"
#include "optimization.h"
#include "utils.h"


/* 
 * Base processor tracer
 */
class BaseTracer {
public:
    CPUArchState *Env;
    void *Perf;

    BaseTracer(CPUArchState *env) : Env(env), Perf(nullptr) {}
    virtual ~BaseTracer() {}
    virtual void Reset() {}
    virtual void Record(uintptr_t next_tb, TranslationBlock *tb) {}

    /* Create and return the tracer object based on LLVM_MODE. */
    static BaseTracer *CreateTracer(CPUArchState *env);

    /* Release the trace resources. */
    static void DeleteTracer(CPUArchState *env);
};


/*
 * Trace of a single basic block
 */
class SingleBlockTracer : public BaseTracer {
    TranslationBlock *TB;

public:
    SingleBlockTracer(CPUArchState *env);

    void Record(uintptr_t next_tb, TranslationBlock *tb) override;
};


/*
 * Trace with NET trace formation algorithm
 */
#define NET_PROFILE_THRESHOLD 50
#if defined(CONFIG_SOFTMMU)
#  define NET_PREDICT_THRESHOLD 16
#else
#  define NET_PREDICT_THRESHOLD 64
#endif
class NETTracer : public BaseTracer {
    bool isTraceHead(uintptr_t next_tb, TranslationBlock *tb, bool NewTB);

public:
    typedef std::vector<TranslationBlock *> TBVec;
    TBVec TBs;

    NETTracer(CPUArchState *env, int Mode);
    ~NETTracer();

    void Reset() override;
    void Record(uintptr_t next_tb, TranslationBlock *tb) override;
    inline void Profile(TranslationBlock *tb);
    inline void Predict(TranslationBlock *tb);
};

/* Return the address of the patch point to the trace code. */
static inline uintptr_t tb_get_jmp_entry(TranslationBlock *tb) {
    return (uintptr_t)tb->tc_ptr + tb->patch_jmp;
}
/* Return the initial jump target address of the patch point. */
static inline uintptr_t tb_get_jmp_next(TranslationBlock *tb) {
    return (uintptr_t)tb->tc_ptr + tb->patch_next;
}
static inline SingleBlockTracer &getSingleBlockTracer(CPUArchState *env) {
    return *static_cast<SingleBlockTracer *>(cpu_get_tracer(env));
}
static inline NETTracer &getNETTracer(CPUArchState *env) {
    return *static_cast<NETTracer *>(cpu_get_tracer(env));
}

static inline void delete_image(TranslationBlock *tb)
{
#if defined(CONFIG_LLVM) && defined(CONFIG_SOFTMMU)
    delete (char *)tb->image;
    tb->image = nullptr;
#endif
}

static inline bool update_tb_mode(TranslationBlock *tb, int from, int to) {
    if (tb->mode != from)
        return false;
    return Atomic<int>::testandset(&tb->mode, from, to);
}

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

