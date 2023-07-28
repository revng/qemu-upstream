/*
 *  (C) 2015 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __HQEMU_H
#define __HQEMU_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "config-host.h"
#include "config-target.h"
#include "hqemu-config.h"

#define build_tcg(_env)        ((_env)->build_mode & BUILD_TCG)
#define build_llvm(_env)       ((_env)->build_mode & BUILD_LLVM)
#define build_llvm_only(_env)  ((_env)->build_mode == BUILD_LLVM)

void hqemu_help(void);

/* Optimizations */
int optimization_init(CPUArchState *env);
int optimization_finalize(CPUArchState *env);
int optimization_reset(CPUArchState *env, int force_flush);
int optimization_remove_entry(CPUArchState *env, TranslationBlock *tb);
int optimization_flush_page(CPUArchState *env, target_ulong pc);
int optimization_init_tb(TranslationBlock *tb, int id);

void itlb_update_entry(CPUArchState *env, TranslationBlock *tb);
void ibtc_update_entry(CPUArchState *env, TranslationBlock *tb);

int lpt_reset(CPUArchState *env);
int lpt_add_page(CPUArchState *env, target_ulong addr, target_ulong size);
int lpt_search_page(CPUArchState *env, target_ulong addr, target_ulong *addrp, target_ulong *sizep);
int lpt_flush_page(CPUArchState *env, target_ulong addr, target_ulong *addrp, target_ulong *sizep);


/* Tracer */
void tracer_exec_tb(CPUArchState *env, uintptr_t next_tb, TranslationBlock *tb);
void tracer_reset(CPUArchState *env);


/* LLVM */
int llvm_init(void);
int llvm_finalize(void);
int llvm_alloc_cache(void);
int llvm_check_cache(void);
int llvm_tb_flush(void);
int llvm_tb_remove(TranslationBlock *tb);
void llvm_handle_chaining(uintptr_t next_tb, TranslationBlock *tb);
int llvm_locate_trace(uintptr_t searched_pc);
TranslationBlock *llvm_find_pc(CPUState *cpu, uintptr_t searched_pc);
int llvm_restore_state(CPUState *cpu, TranslationBlock *tb, uintptr_t searched_pc);
void llvm_fork_start(void);
void llvm_fork_end(int child);


/* Annotation */
enum {
    ANNOTATION_NONE = 0,
    ANNOTATION_LOOP,
};
int llvm_has_annotation(target_ulong addr, int annotation);


/* External variables */
extern int tracer_mode;
extern target_ulong pcid;
extern unsigned long alignment_count[]; /* 0: misaligned, 1: aligned. */
extern unsigned long aligned_boundary;

#ifdef __cplusplus
}
#endif

#endif

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

