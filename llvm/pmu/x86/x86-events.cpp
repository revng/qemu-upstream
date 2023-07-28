/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include "pmu/pmu-global.h"

namespace pmu {

#define ICACHE_HIT_CONFIG  (0x83 | (0x1 << 8))   /* skylake/event=0x83,umask=0x1/ */
#define ICACHE_MISS_CONFIG (0x83 | (0x2 << 8))   /* skylake/event=0x83,umask=0x2/ */
#define MEM_LOADS_CONFIG   (0xd0 | (0x81 << 8 )) /* skylake/event=0xd0,umask=0x81/ */
#define MEM_STORES_CONFIG  (0xd0 | (0x82 << 8 )) /* skylake/event=0xd0,umask=0x82/ */

extern EventID PreEvents[PMU_EVENT_MAX];  /* Pre-defined events.   */

static void X86SetupEventCode()
{
#define SetupEvent(_Event,_Config)            \
    PreEvents[_Event].Type   = PERF_TYPE_RAW; \
    PreEvents[_Event].Config = _Config;

    SetupEvent(PMU_ICACHE_HITS, ICACHE_HIT_CONFIG);
    SetupEvent(PMU_ICACHE_MISSES, ICACHE_MISS_CONFIG);
    SetupEvent(PMU_MEM_LOADS, MEM_LOADS_CONFIG);
    SetupEvent(PMU_MEM_STORES, MEM_STORES_CONFIG);

#undef SetEventCode
}

int X86Init()
{
    X86SetupEventCode();
    return PMU_OK;
}

} /* namespace pmu */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
