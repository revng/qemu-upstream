/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <errno.h>
#include <sys/mman.h>
#include "pmu/pmu-global.h"
#include "pmu/pmu-events.h"

/*
 * Performance Monitoring Unit (PMU) tools.
 */
namespace pmu {

static bool InitOnce;

EventManager *EventMgr;            /* Event manager. */
GlobalConfig SysConfig;            /* System-wide configuration. */
EventID PreEvents[PMU_EVENT_MAX];  /* Pre-defined events. */


/* Initialize system-wide configuration. */
static void SetupGlobalConfig(PMUConfig &Config)
{
    /* Get page size. */
    SysConfig.PageSize = getpagesize();

    /* Configure timeout and signal receiver for the timer. */
    SysConfig.SignalReceiver = Config.SignalReceiver;
    if (SysConfig.SignalReceiver <= 0)
        SysConfig.SignalReceiver = getpid();

    SysConfig.Timeout = Config.Timeout;
    if (SysConfig.Timeout == 0)
        SysConfig.Timeout = PMU_TIMER_PERIOD;

    SysConfig.Timeout *= 1000; /* nanosecond */

    /* Determine the Linux Perf version used by this tool and the kernel.
     * We set the last few bytes of the perf_event_attr structure and see the
     * size field returned from the kernel. */

    SysConfig.PerfVersion = 0;
    SysConfig.OSPerfVersion = 0;

    struct perf_event_attr attr;
    perf_attr_init(&attr, PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES);
    attr.aux_watermark = 1;
    int fd = sys_perf_event_open(&attr, 0, -1, -1, 0);
    close(fd);

#define CheckPerfVersion(_Ver)                     \
    do {                                           \
        SysConfig.PerfVersion = _Ver;              \
        if (attr.size == PERF_ATTR_SIZE_VER##_Ver) \
            SysConfig.OSPerfVersion = _Ver;        \
    } while(0)

    CheckPerfVersion(1);
    CheckPerfVersion(2);
    CheckPerfVersion(3);
    CheckPerfVersion(4);
    CheckPerfVersion(5);

#undef CheckPerfVersion
}

/* Initialize pre-defined events. */
static void SetupDefaultEvent()
{
    for (unsigned i = 0; i < PMU_EVENT_MAX; ++i) {
        PreEvents[i].Type = -1;
        PreEvents[i].Config = -1;
    }

#define SetupEvent(_Event,_Config)                 \
    PreEvents[_Event].Type   = PERF_TYPE_HARDWARE; \
    PreEvents[_Event].Config = _Config;

    /* Basic events. */
    SetupEvent(PMU_CPU_CYCLES, PERF_COUNT_HW_CPU_CYCLES);
    SetupEvent(PMU_REF_CPU_CYCLES, PERF_COUNT_HW_REF_CPU_CYCLES);
    SetupEvent(PMU_INSTRUCTIONS, PERF_COUNT_HW_INSTRUCTIONS);
    SetupEvent(PMU_LLC_REFERENCES, PERF_COUNT_HW_CACHE_REFERENCES);
    SetupEvent(PMU_LLC_MISSES, PERF_COUNT_HW_CACHE_MISSES);
    SetupEvent(PMU_BRANCH_INSTRUCTIONS, PERF_COUNT_HW_BRANCH_INSTRUCTIONS);
    SetupEvent(PMU_BRANCH_MISSES, PERF_COUNT_HW_BRANCH_MISSES);

#undef SetEventCode
}

/* Initialize the PMU module. */
int PMU::Init(PMUConfig &Config)
{
    if (InitOnce == true)
        return PMU_OK;

    /* Set the global configuration. */
    SetupGlobalConfig(Config);

    /* Initialize pre-defined event codes. */
    SetupDefaultEvent();

    /* Allocate event manager. */
    EventMgr = new EventManager;

    /* Initialize target-specific events. */
#if defined(__i386__) || defined(__x86_64__)
    X86Init();
#elif defined(__arm__) || defined (__aarch64__)
    ARMInit();
#elif defined(_ARCH_PPC) || defined(_ARCH_PPC64)
    PPCInit();
#endif

    Config.PerfVersion = SysConfig.PerfVersion;
    Config.OSPerfVersion = SysConfig.OSPerfVersion;

    InitOnce = true;
    return PMU_OK;
}

/* Finalize the PMU module. */
int PMU::Finalize(void)
{
    if (InitOnce == false)
        return PMU_OK;

    delete EventMgr;

    InitOnce = false;
    return PMU_OK;
}

/* Stop the PMU module. */
int PMU::Pause(void)
{
    EventMgr->Pause();
    return PMU_OK;
}

/* Restart the PMU module. */
int PMU::Resume(void)
{
    EventMgr->Resume();
    return PMU_OK;
}

/* Start a counting/sampling/tracing event. */
int PMU::Start(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;

    if (perf_event_start(Event->getFD()) != 0)
        return PMU_EEVENT;

    EventMgr->StartEvent(Event);

    return PMU_OK;
}

/* Stop a counting/sampling/tracing event. */
int PMU::Stop(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;

    if (perf_event_stop(Event->getFD()) != 0)
        return PMU_EEVENT;

    EventMgr->StopEvent(Event);

    return PMU_OK;
}

/* Reset the hardware counter. */
int PMU::Reset(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;

    if (perf_event_reset(Event->getFD()) != 0)
        return PMU_EEVENT;
    return PMU_OK;
}

/* Remove an event. */
int PMU::Cleanup(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;

    /* Do stop the event if the user hasn't called it. */
    if (Event->State != STATE_STOP) {
        int EC = Stop(Hndl);
        if (EC != PMU_OK)
            return EC;
    }

    /* At this point, this event has been removed from the sampling list and we
     * no longer get overflow handling (if this is a sampling event). We are
     * now able to release all resources. */

    /* Stop all events in a group. */
    for (auto fd : Event->FD)
        perf_event_stop(fd);

    /* Release allocated buffers. */
    if (Event->Data.Base)
        munmap(Event->Data.Base, Event->Data.Size);
    if (Event->Aux.Base)
        munmap(Event->Aux.Base, Event->Aux.Size);

    for (auto fd : Event->FD)
        close(fd);

    EventMgr->DeleteEvent(Event);
    return PMU_OK;
}

/* Start/stop a sampling/tracing event without acquiring a lock.
 * Note that these two function should only be used within the overflow
 * handler. Since the overflow handling is already in a locked section,
 * acquiring a lock is not required. */
int PMU::StartUnlocked(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;
    if (Event->Mode & MODE_COUNTER)
        return PMU_EINVAL;

    if (perf_event_start(Event->getFD()) != 0)
        return PMU_EEVENT;

    EventMgr->StartEvent(Event, false);

    return PMU_OK;
}

int PMU::StopUnlocked(Handle Hndl)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;
    if (Event->Mode & MODE_COUNTER)
        return PMU_EINVAL;

    if (perf_event_stop(Event->getFD()) != 0)
        return PMU_EEVENT;

    EventMgr->StopEvent(Event, false);

    return PMU_OK;
}

/* Open an event using the pre-defined event code. */
int PMU::CreateEvent(unsigned EventCode, Handle &Hndl)
{
    int fd;
    struct perf_event_attr Attr;

    Hndl = PMU_INVALID_HNDL;

    if (EventCode >= PMU_EVENT_MAX)
        return PMU_EINVAL;
    if (PreEvents[EventCode].Type == -1)
        return PMU_ENOEVENT;

    perf_attr_init(&Attr, PreEvents[EventCode].Type, PreEvents[EventCode].Config);
    fd = sys_perf_event_open(&Attr, 0, -1, -1, 0);
    if (fd < 0)
        return ErrorCode(errno);

    Hndl = EventMgr->AddEvent(fd);
    if (Hndl == PMU_INVALID_HNDL) {
        close(fd);
        return PMU_ENOMEM;
    }
    return PMU_OK;
}

/* Open an event using the raw event number and umask value.
 * The raw event code is computed as (RawEvent | (Umask << 8)). */
int PMU::CreateRawEvent(unsigned RawEvent, unsigned Umask, Handle &Hndl)
{
    int fd;
    struct perf_event_attr Attr;

    Hndl = PMU_INVALID_HNDL;

    perf_attr_init(&Attr, PERF_TYPE_RAW, RawEvent | (Umask << 8));
    fd = sys_perf_event_open(&Attr, 0, -1, -1, 0);
    if (fd < 0)
        return ErrorCode(errno);

    Hndl = EventMgr->AddEvent(fd);
    if (Hndl == PMU_INVALID_HNDL) {
        close(fd);
        return PMU_ENOMEM;
    }
    return PMU_OK;
}

/* Open a sampling event, with the 1st EventCode as the interrupt event.
 * The sample data will be recorded in a vector of type 'uint64_t'.
 * The following vector shows the data format of sampling with N events:
 *     { pc, val1, val2, ..., valN,      # 1st sample
 *       ...
 *       pc, val1, val2, ..., valN };    # nth sample
 *
 * Note that ownwership of the output vector is transferred to the user.
 * It is the user's responsibility to free the resource of the vector. */
int PMU::CreateSampleEvent(SampleConfig &Config, Handle &Hndl)
{
    unsigned i, NumEvents = Config.NumEvents;
    unsigned NumPages = Config.NumPages;
    uint64_t Period = Config.Period;
    int fds[PMU_GROUP_EVENTS], EC = PMU_ENOMEM;
    uint64_t DataSize;
    void *Data;

    Hndl = PMU_INVALID_HNDL;

    if (NumPages == 0)
        NumPages = PMU_SAMPLE_PAGES;
    if (Period < 1e3)
        Period = PMU_SAMPLE_PERIOD;

    if (NumEvents == 0 || NumEvents > PMU_GROUP_EVENTS || !isPowerOf2(NumPages))
        return PMU_EINVAL;

    /* Check event codes. */
    for (i = 0; i < NumEvents; ++i) {
        unsigned EventCode = Config.EventCode[i];
        if (EventCode >= PMU_EVENT_MAX)
            return PMU_EINVAL;
        if (PreEvents[EventCode].Type == -1)
            return PMU_ENOEVENT;
    }

    /* Open the events. If more than one event is requested, set read_format
     * to PERF_FORMAT_GROUP. */
    fds[0] = -1;
    for (i = 0; i < NumEvents; ++i) {
        struct perf_event_attr Attr;
        unsigned EventCode = Config.EventCode[i];
        perf_attr_init(&Attr, PreEvents[EventCode].Type, PreEvents[EventCode].Config);

        Attr.disabled = !i;
        if (i == 0) {
            Attr.sample_period = Period;
            Attr.sample_type = PERF_SAMPLE_IP | PERF_SAMPLE_READ;
            Attr.read_format = (NumEvents > 1) ? PERF_FORMAT_GROUP : 0;
        }

        fds[i] = sys_perf_event_open(&Attr, 0, -1, fds[0], 0);
        if (fds[i] < 0) {
            EC = ErrorCode(errno);
            goto failed;
        }
    }

    /* Allocate buffer for the sampling data. */
    DataSize = (1 + NumPages) * SysConfig.PageSize;
    Data = mmap(nullptr, DataSize, PROT_READ|PROT_WRITE, MAP_SHARED, fds[0], 0);
    if (Data == MAP_FAILED)
        goto failed;

    Hndl = EventMgr->AddSampleEvent(NumEvents, fds, DataSize, Data,
                                    MODE_SAMPLE_IP | MODE_SAMPLE_READ, Config);
    if (Hndl == PMU_INVALID_HNDL) {
        munmap(Data, DataSize);
        goto failed;
    }
    return PMU_OK;

failed:
    while (--i)
        close(fds[i]);
    return EC;
}

/* Generate an IP histogram using EventCode as the interrupt event.
 * The IP histogram will be recorded in a vector of type 'uint64_t' with
 * the format: { pc1, pc2, pc3, ..., pcN }.
 * Note that ownwership of the output vector is transferred to the user.
 * It is the user's responsibility to free the resource of the vector. */
int PMU::CreateSampleIP(Sample1Config &Config, Handle &Hndl)
{
    int fd;
    unsigned EventCode = Config.EventCode;
    unsigned NumPages = Config.NumPages;
    uint64_t Period = Config.Period;
    uint64_t DataSize;
    void *Data;

    Hndl = PMU_INVALID_HNDL;

    if (NumPages == 0)
        NumPages = PMU_SAMPLE_PAGES;
    if (Period < 1e3)
        Period = PMU_SAMPLE_PERIOD;

    if (!isPowerOf2(NumPages))
        return PMU_EINVAL;

    /* Check the events. */
    if (EventCode >= PMU_EVENT_MAX)
        return PMU_EINVAL;
    if (PreEvents[EventCode].Type == -1)
        return PMU_ENOEVENT;

    struct perf_event_attr Attr;
    perf_attr_init(&Attr, PreEvents[EventCode].Type, PreEvents[EventCode].Config);

    Attr.disabled = 1;
    Attr.sample_period = Period;
    Attr.sample_type = PERF_SAMPLE_IP;

    fd = sys_perf_event_open(&Attr, 0, -1, -1, 0);
    if (fd < 0)
        return ErrorCode(errno);

    /* Allocate buffer for the sampling data. */
    DataSize = (1 + NumPages) * SysConfig.PageSize;
    Data = mmap(nullptr, DataSize, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if (Data == MAP_FAILED)
        goto failed;

    /* Set the sampling config. */
    SampleConfig SConfig;
    SConfig.NumEvents = 1;
    SConfig.EventCode[0] = Config.EventCode;
    SConfig.NumPages = NumPages;
    SConfig.Period = Period;
    SConfig.Watermark = Config.Watermark;
    SConfig.SampleHandler = Config.SampleHandler;
    SConfig.Opaque = Config.Opaque;

    Hndl = EventMgr->AddSampleEvent(1, &fd, DataSize, Data, MODE_SAMPLE_IP, SConfig);
    if (Hndl == PMU_INVALID_HNDL) {
        munmap(Data, DataSize);
        goto failed;
    }
    return PMU_OK;

failed:
    close(fd);
    return PMU_ENOMEM;
}

/* Read value from the hardware counter. */
int PMU::ReadEvent(Handle Hndl, uint64_t &Value)
{
    auto Event = EventMgr->GetEvent(Hndl);
    if (!Event)
        return PMU_EINVAL;

    if (read(Event->getFD(), &Value, sizeof(uint64_t)) != sizeof(uint64_t))
        return PMU_EEVENT;
    return PMU_OK;
}

/* Convert error code to string. */
const char *PMU::strerror(int ErrCode)
{
    switch (ErrCode) {
        case PMU_OK:       return "Success";
        case PMU_EINVAL:   return "Invalid argument";
        case PMU_ENOMEM:   return "Insufficient memory";
        case PMU_ENOEVENT: return "Pre-defined event not available";
        case PMU_EEVENT:   return "Hardware event error";
        case PMU_EPERM:    return "Permission denied";
        case PMU_EINTER:   return "Internal error";
        case PMU_EDECODER: return "Decoder error";
        default:           return "Unknown error";
    }
}

} /* namespace pmu */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
