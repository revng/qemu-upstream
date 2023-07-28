/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __PMU_GLOBAL_H
#define __PMU_GLOBAL_H

#if defined(__i386__) || defined(__x86_64__)
#include "pmu/x86/x86-events.h"
#elif defined(__arm__) || defined(__aarch64__)
#include "pmu/arm/arm-events.h"
#elif defined(_ARCH_PPC) || defined(_ARCH_PPC64)
#include "pmu/ppc/ppc-events.h"
#endif

#include "pmu/pmu-utils.h"
#include "pmu/pmu.h"

namespace pmu {

#define PMU_SIGNAL_NUM     SIGIO
#define PMU_SAMPLE_PERIOD  1e6
#define PMU_SAMPLE_PAGES   4

class EventManager;

/* Pre-defined event identity. */
struct EventID {
    int Type;     /* Perf major type: hardware/software/etc */
    int Config;   /* Perf type specific configuration information */
};

/* System-wide configuration. */
struct GlobalConfig {
    int PageSize;       /* Host page size */
    int SignalReceiver; /* TID of the signal receiver */
    uint32_t Timeout;   /* Timer period in nanosecond */
    int PerfVersion;    /* Perf version used in this PMU tool */
    int OSPerfVersion;  /* Perf version used in the OS kernel */
};

extern EventManager *EventMgr;
extern GlobalConfig SysConfig;

} /* namespace pmu */

#endif /* __PMU_GLOBAL_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
