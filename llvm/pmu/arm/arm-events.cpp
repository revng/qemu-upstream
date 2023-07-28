/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "pmu/pmu-global.h"

namespace pmu {

/* ARMv8 recommended implementation defined event types. 
 * (copied from linux-4.x/arch/arm64/kernel/perf_event.c) */
#define ICACHE_MISS_CONFIG (0x01)
#define MEM_LOADS_CONFIG   (0x06)
#define MEM_STORES_CONFIG  (0x07)


extern EventID PreEvents[PMU_EVENT_MAX];  /* Pre-defined events.   */

static void ARMSetupEventCode()
{
#define SetupEvent(_Event,_Config)            \
    PreEvents[_Event].Type   = PERF_TYPE_RAW; \
    PreEvents[_Event].Config = _Config;

    SetupEvent(PMU_ICACHE_MISSES, ICACHE_MISS_CONFIG);
    SetupEvent(PMU_MEM_LOADS, MEM_LOADS_CONFIG);
    SetupEvent(PMU_MEM_STORES, MEM_STORES_CONFIG);

#undef SetEventCode
}

int ARMInit()
{
    ARMSetupEventCode();
    return PMU_OK;
}

} /* namespace pmu */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
