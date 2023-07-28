/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __X86_EVENTS_H
#define __X86_EVENTS_H

#include <vector>
#include "pmu/pmu.h"

namespace pmu {

class PMUEvent;

#if defined(__i386__)
/*
 * Some non-Intel clones support out of order store. wmb() ceases to be a
 * nop for these.
 */
#define pmu_mb()    asm volatile("lock; addl $0,0(%%esp)" ::: "memory")
#define pmu_rmb()   asm volatile("lock; addl $0,0(%%esp)" ::: "memory")
#define pmu_wmb()   asm volatile("lock; addl $0,0(%%esp)" ::: "memory")
#elif defined(__x86_64__)
#define pmu_mb()    asm volatile("mfence" ::: "memory")
#define pmu_rmb()   asm volatile("lfence" ::: "memory")
#define pmu_wmb()   asm volatile("sfence" ::: "memory")
#endif 

int X86Init(void);

} /* namespace pmu */

#endif /* __X86_EVENTS_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
