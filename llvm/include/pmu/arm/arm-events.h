/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __ARM_EVENTS_H
#define __ARM_EVENTS_H

#include <vector>
#include "pmu/pmu.h"

namespace pmu {

class PMUEvent;

#if defined(__arm__)
#define pmu_mb()    ((void(*)(void))0xffff0fa0)()
#define pmu_rmb()   ((void(*)(void))0xffff0fa0)()
#define pmu_wmb()   ((void(*)(void))0xffff0fa0)()
#elif defined(__aarch64__)
#define pmu_mb()    asm volatile("dmb ish" ::: "memory")
#define pmu_rmb()   asm volatile("dmb ishld" ::: "memory")
#define pmu_wmb()   asm volatile("dmb ishst" ::: "memory")
#endif 


int ARMInit(void);

} /* namespace pmu */

#endif /* __ARM_EVENTS_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
