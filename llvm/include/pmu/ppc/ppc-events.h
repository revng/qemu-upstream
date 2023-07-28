/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __PPC_EVENTS_H
#define __PPC_EVENTS_H

#include <vector>
#include "pmu/pmu.h"

namespace pmu {

class PMUEvent;

#if defined(_ARCH_PPC) || defined(_ARCH_PPC64)
#define pmu_mb()     __asm__ __volatile__ ("sync" : : : "memory")
#define pmu_rmb()    __asm__ __volatile__ ("sync" : : : "memory")
#define pmu_wmb()    __asm__ __volatile__ ("sync" : : : "memory")
#endif 

int PPCInit(void);

} /* namespace pmu */

#endif /* __PPC_EVENTS_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
