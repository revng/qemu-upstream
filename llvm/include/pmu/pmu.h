/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 *
 *  Hardware Performance Monitoring Unit (PMU), C++ interfaces.
 */

#ifndef __PMU_H
#define __PMU_H

#include <vector>
#include <memory>
#include <stdint.h>

namespace pmu {

#define PMU_GROUP_EVENTS   (8)
#define PMU_TIMER_PERIOD   (400)     /* micro-second */
#define PMU_INVALID_HNDL   ((Handle)-1)

typedef unsigned Handle;
/* Sampling event overflow handling. */
typedef std::vector<uint64_t> SampleList;
typedef std::unique_ptr<SampleList> SampleDataPtr;
typedef void (*SampleHandlerTy)(Handle Hndl, SampleDataPtr Data, void *Opaque);

/* Error code. */
enum {
    PMU_OK       = 0,   /* No error */
    PMU_EINVAL   = -1,  /* Invalid argument */
    PMU_ENOMEM   = -2,  /* Insufficient memory */
    PMU_ENOEVENT = -3,  /* Pre-defined event not available */
    PMU_EEVENT   = -4,  /* Hardware event error */
    PMU_EPERM    = -5,  /* Permission denied */
    PMU_EINTER   = -6,  /* Internal error */
    PMU_EDECODER = -7,  /* Instruction trace decoder error */
};

/* Pre-defined event code. */
enum {
    /* Basic events */
    PMU_CPU_CYCLES = 0,
    PMU_REF_CPU_CYCLES,
    PMU_INSTRUCTIONS,
    PMU_LLC_REFERENCES,
    PMU_LLC_MISSES,
    PMU_BRANCH_INSTRUCTIONS,
    PMU_BRANCH_MISSES,
    /* Instruction cache events */
    PMU_ICACHE_HITS,
    PMU_ICACHE_MISSES,
    /* Memory instruction events */
    PMU_MEM_LOADS,
    PMU_MEM_STORES,

    PMU_EVENT_MAX,
};

/* PMU initial configuration. */
struct PMUConfig {
    /* Input */
    int SignalReceiver; /* TID of the signal receiver. 0 for auto-select. */
    uint32_t Timeout;   /* Timer period in micro-second. 0 for auto-select.  */

    /* Output */
    int PerfVersion;   /* Perf version used in this PMU tool */
    int OSPerfVersion; /* Perf version used in the OS kernel */
};

/* Config for sampling with one or multiple event(s).*/
struct SampleConfig {
    unsigned NumEvents; /* Number of events in the event group */
    unsigned EventCode[PMU_GROUP_EVENTS]; /* Event group. The 1st event is the leader. */
    unsigned NumPages;  /* Number of pages as the sample buffer size. (must be 2^n) */
    uint64_t Period;    /* Sampling period of the group leader. */
    uint64_t Watermark; /* Bytes before wakeup. 0 for every timer period. */
    SampleHandlerTy SampleHandler; /* User handler routine */
    void *Opaque;       /* An opaque pointer passed to the overflow handler. */
};

/* Config for sampling with only one event. */
struct Sample1Config {
    unsigned EventCode; /* Pre-defined event to trigger counter overflow */
    unsigned NumPages;  /* Number of pages as the sample buffer size. (must be 2^n) */
    uint64_t Period;    /* Sampling period */
    uint64_t Watermark; /* Bytes before wakeup. 0 for every timer period. */
    SampleHandlerTy SampleHandler; /* User handler routine */
    void *Opaque;       /* An opaque pointer passed to the overflow handler. */
};

/*
 * PMU main tools.
 */
class PMU {
    PMU()  = delete;
    ~PMU() = delete;

public:
    /* Initialize the PMU module. */
    static int Init(PMUConfig &Config);

    /* Finalize the PMU module. */
    static int Finalize(void);

    /* Stop the PMU module. When the PMU module is paused, the user can continue
     * to use counting events, but the overflow handler will not be invoked. */
    static int Pause(void);

    /* Restart the PMU module. After the PMU module is resumed, the overflow
     * handler will be invoked. */
    static int Resume(void);

    /* Start a counting/sampling/tracing event. */
    static int Start(Handle Hndl);

    /* Stop a counting/sampling/tracing event. */
    static int Stop(Handle Hndl);

    /* Reset the hardware counter. */
    static int Reset(Handle Hndl);

    /* Remove an event. */
    static int Cleanup(Handle Hndl);

    /* Start/stop a sampling/tracing event without acquiring a lock.
     * Note that these two function should only be used within the overflow
     * handler. Since the overflow handling is already in a locked section,
     * acquiring a lock is not required. */
    static int StartUnlocked(Handle Hndl);
    static int StopUnlocked(Handle Hndl);

    /* Open an event using the pre-defined event code. */
    static int CreateEvent(unsigned EventCode, Handle &Hndl);

    /* Open an event using the raw event number and umask value.
     * The raw event code is computed as (RawEvent | (Umask << 8)). */
    static int CreateRawEvent(unsigned RawEvent, unsigned Umask, Handle &Hndl);

    /* Open a sampling event, with the 1st EventCode as the interrupt event.
     * The sample data will be recorded in a vector of type 'uint64_t'.
     * The following vector shows the data format of sampling with N events:
     *     { pc, val1, val2, ..., valN,      # 1st sample
     *       ...
     *       pc, val1, val2, ..., valN };    # nth sample
     *
     * Note that ownwership of the output vector is transferred to the user.
     * It is the user's responsibility to free the resource of the vector. */
    static int CreateSampleEvent(SampleConfig &Config, Handle &Hndl);

    /* Generate an IP histogram, using EventCode as the interrupt event.
     * The IP histogram will be recorded in a vector of type 'uint64_t' with
     * the format: { pc1, pc2, pc3, ..., pcN }.
     * Note that ownwership of the output vector is transferred to the user.
     * It is the user's responsibility to free the resource of the vector. */
    static int CreateSampleIP(Sample1Config &Config, Handle &Hndl);

    /* Read value from the hardware counter. */
    static int ReadEvent(Handle Hndl, uint64_t &Value);

    /* Convert error code to string. */
    static const char *strerror(int ErrCode);
};

} /* namespace pmu */

#endif /* __PMU_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
