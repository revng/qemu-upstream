/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "pmu/pmu-global.h"

namespace pmu {

#define ICACHE_MISS_CONFIG (0x200fd)
#define MEM_LOADS_CONFIG   (0x100fc)

extern EventID PreEvents[PMU_EVENT_MAX];  /* Pre-defined events.   */

static void PPCSetupEventCode()
{
#define SetupEvent(_Event,_Config)            \
    PreEvents[_Event].Type   = PERF_TYPE_RAW; \
    PreEvents[_Event].Config = _Config;

    SetupEvent(PMU_ICACHE_MISSES, ICACHE_MISS_CONFIG);
    SetupEvent(PMU_MEM_LOADS, MEM_LOADS_CONFIG);

#undef SetEventCode
}

int PPCInit()
{
    PPCSetupEventCode();
    return PMU_OK;
}

} /* namespace pmu */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
