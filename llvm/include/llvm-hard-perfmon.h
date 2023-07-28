/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_HARD_PERFMON_H
#define __LLVM_HARD_PERFMON_H

#include <map>
#include <thread>
#include "pmu/pmu.h"
#include "utils.h"

class PerfmonData;
class BaseTracer;

enum HPMControl {
    HPM_INIT = 0,
    HPM_FINALIZE,
    HPM_START,
    HPM_STOP,
};

/*
 * Hardware Performance Monitor (HPM)
 */
class HardwarePerfmon {
    std::thread MonThread;  /* Monitor thread */
    int MonThreadID;        /* Monitor thread id */
    bool MonThreadStop;     /* Monitor thread is stopped or not */
    hqemu::Mutex Lock;

    /* Start monitor thread. */
    void StartMonThread();

    /* Monitor thread routine. */
    void MonitorFunc();

public:
    HardwarePerfmon();
    ~HardwarePerfmon();

    /* Set up HPM with the monitor thread id */
    void Init(int monitor_thread_tid);

    /* Register a thread to be monitored. */
    void RegisterThread(BaseTracer *Tracer);

    /* Unreigster a thread from being monitored. */
    void UnregisterThread(BaseTracer *Tracer);

    /* Notify that the execution enters/leaves the code cache. */
    void NotifyCacheEnter(BaseTracer *Tracer);
    void NotifyCacheLeave(BaseTracer *Tracer);

    /* Stop the monitor. */
    void Pause();

    /* Restart the monitor. */
    void Resume();
};


class PerfmonData {
public:
    PerfmonData(int tid);
    ~PerfmonData();

    int TID;
    pmu::Handle ICountHndl;
    pmu::Handle BranchHndl;
    pmu::Handle MemLoadHndl;
    pmu::Handle MemStoreHndl;
    pmu::Handle CoverSetHndl;
    uint64_t LastNumBranches, LastNumLoads, LastNumStores;

    void MonitorBasic(HPMControl Ctl);
    void MonitorCoverSet(HPMControl Ctl);
};

extern HardwarePerfmon *HP;

#endif /* __LLVM_HARD_PERFMON_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
