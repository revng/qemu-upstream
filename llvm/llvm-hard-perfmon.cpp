/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <string.h>
#include "config-target.h"
#include "tracer.h"
#include "llvm.h"
#include "llvm-soft-perfmon.h"
#include "llvm-hard-perfmon.h"

using namespace pmu;


HardwarePerfmon::HardwarePerfmon() : MonThreadID(-1), MonThreadStop(true)
{
}

HardwarePerfmon::~HardwarePerfmon()
{
    if (LLVMEnv::RunWithVTune)
        return;

    PMU::Finalize();

    if (!MonThreadStop)
        MonThreadStop = true;
}

/* Set up HPM with the monitor thread id */
void HardwarePerfmon::Init(int monitor_thread_tid)
{
    if (LLVMEnv::RunWithVTune)
        return;

    MonThreadID = monitor_thread_tid;

#if defined(ENABLE_HPM_THREAD)
    /* Start HPM thread. */
    StartMonThread();
#else
    /* If we attempt to profile hotspot but do not run the HPM translation mode,
     * we enable the HPM monitor thread for the hotspot profiling in order to
     * avoid deadlock. */
    if (SP->Mode & SPM_HOTSPOT)
        StartMonThread();
#endif

    /* Initialize the PMU tools. */
    PMUConfig Config;
    memset(&Config, 0, sizeof(PMUConfig));
    Config.SignalReceiver = MonThreadID;
    Config.Timeout = 400;
    int EC = PMU::Init(Config);
    if (EC != PMU_OK) {
        dbg() << DEBUG_HPM << "Failed to initialize PMU (" << PMU::strerror(EC)
              << ").\n";
        return;
    }
}

/* Stop the monitor. */
void HardwarePerfmon::Pause()
{
    if (LLVMEnv::RunWithVTune)
        return;

    PMU::Pause();
}

/* Restart the monitor. */
void HardwarePerfmon::Resume()
{
    if (LLVMEnv::RunWithVTune)
        return;

    PMU::Resume();
}

/* Start monitor thread. */
void HardwarePerfmon::StartMonThread()
{
    /* Start HPM thread. */
    MonThreadID = -1;
    MonThreadStop = false;
    MonThread = std::thread(
                    [=]() { MonitorFunc(); }
                );

    MonThread.detach();
    while (MonThreadID == -1)
        usleep(200);
}

/* Monitor thread routine. */
void HardwarePerfmon::MonitorFunc()
{
    MonThreadID = gettid();
    copy_tcg_context();

    while (!MonThreadStop)
        usleep(10000);
}

static void CoverSetHandler(Handle Hndl, std::unique_ptr<SampleList> DataPtr,
                            void *Opaque)
{
    /* Just attach the sampled IPs to the profile list. The soft-perfmon will
     * release the resource later. */
    SP->SampleListVec.push_back(DataPtr.release());
}

void HardwarePerfmon::RegisterThread(BaseTracer *Tracer)
{
    hqemu::MutexGuard Locked(Lock);

    dbg() << DEBUG_HPM << "Register thread " << gettid() << ".\n";

    if (LLVMEnv::RunWithVTune)
        return;

    PerfmonData *Perf = new PerfmonData(gettid());
    Perf->MonitorBasic(HPM_INIT);
    Perf->MonitorCoverSet(HPM_INIT);

    Tracer->Perf = static_cast<void *>(Perf);
}

void HardwarePerfmon::UnregisterThread(BaseTracer *Tracer)
{
    hqemu::MutexGuard Locked(Lock);

    dbg() << DEBUG_HPM << "Unregister thread " << gettid() << ".\n";

    if (LLVMEnv::RunWithVTune)
        return;
    if (!Tracer->Perf)
        return;

    auto Perf = static_cast<PerfmonData *>(Tracer->Perf);
    Perf->MonitorBasic(HPM_FINALIZE);
    Perf->MonitorCoverSet(HPM_FINALIZE);

    delete Perf;
    Tracer->Perf = nullptr;
}

void HardwarePerfmon::NotifyCacheEnter(BaseTracer *Tracer)
{
    hqemu::MutexGuard Locked(Lock);

    if (!Tracer->Perf)
        return;
    auto Perf = static_cast<PerfmonData *>(Tracer->Perf);
    Perf->MonitorBasic(HPM_START);
}

void HardwarePerfmon::NotifyCacheLeave(BaseTracer *Tracer)
{
    hqemu::MutexGuard Locked(Lock);

    if (!Tracer->Perf)
        return;
    auto Perf = static_cast<PerfmonData *>(Tracer->Perf);
    Perf->MonitorBasic(HPM_STOP);
}

/*
 * PerfmonData
 */
PerfmonData::PerfmonData(int tid) : TID(tid)
{
}

PerfmonData::~PerfmonData()
{
}

void PerfmonData::MonitorBasic(HPMControl Ctl)
{
    if (!(SP->Mode & SPM_HPM))
        return;

    switch (Ctl) {
    case HPM_INIT:
        if (PMU::CreateEvent(PMU_INSTRUCTIONS, ICountHndl) == PMU_OK) {
            dbg() << DEBUG_HPM << "Register event: # instructions.\n";
            PMU::Start(ICountHndl);
        }
        if (PMU::CreateEvent(PMU_BRANCH_INSTRUCTIONS, BranchHndl) == PMU_OK) {
            dbg() << DEBUG_HPM << "Register event: # branch instructions.\n";
            PMU::Start(BranchHndl);
        }
        if (PMU::CreateEvent(PMU_MEM_LOADS, MemLoadHndl) == PMU_OK) {
            dbg() << DEBUG_HPM << "Register event: # load instructions.\n";
            PMU::Start(MemLoadHndl);
        }
        if (PMU::CreateEvent(PMU_MEM_STORES, MemStoreHndl) == PMU_OK) {
            dbg() << DEBUG_HPM << "Register event: # store instructions.\n";
            PMU::Start(MemStoreHndl);
        }
        break;
    case HPM_FINALIZE:
    {
        uint64_t NumInsns = 0, NumBranches = 0, NumLoads = 0, NumStores = 0;
        if (ICountHndl != PMU_INVALID_HNDL) {
            PMU::ReadEvent(ICountHndl, NumInsns);
            PMU::Cleanup(ICountHndl);
        }
        if (BranchHndl != PMU_INVALID_HNDL) {
            PMU::ReadEvent(BranchHndl, NumBranches);
            PMU::Cleanup(BranchHndl);
        }
        if (MemLoadHndl != PMU_INVALID_HNDL) {
            PMU::ReadEvent(MemLoadHndl, NumLoads);
            PMU::Cleanup(MemLoadHndl);
        }
        if (MemStoreHndl != PMU_INVALID_HNDL) {
            PMU::ReadEvent(MemStoreHndl, NumStores);
            PMU::Cleanup(MemStoreHndl);
        }

        SP->NumInsns += NumInsns;
        SP->NumBranches += NumBranches;
        SP->NumLoads += NumLoads;
        SP->NumStores += NumStores;
        break;
    }
    case HPM_START:
        if (BranchHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(BranchHndl, LastNumBranches);
        if (MemLoadHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(MemLoadHndl, LastNumLoads);
        if (MemStoreHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(MemStoreHndl, LastNumStores);
        break;
    case HPM_STOP:
    {
        uint64_t NumBranches = 0, NumLoads = 0, NumStores = 0;
        if (BranchHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(BranchHndl, NumBranches);
        if (MemLoadHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(MemLoadHndl, NumLoads);
        if (MemStoreHndl != PMU_INVALID_HNDL)
            PMU::ReadEvent(MemStoreHndl, NumStores);
        break;
    }
    default:
        break;
    }
}

void PerfmonData::MonitorCoverSet(HPMControl Ctl)
{
    if (!(SP->Mode & SPM_HOTSPOT))
        return;

    switch (Ctl) {
    case HPM_INIT: {
        Sample1Config IPConfig;
        memset(&IPConfig, 0, sizeof(Sample1Config));
        IPConfig.EventCode = PMU_INSTRUCTIONS;
        IPConfig.NumPages = 4;
        IPConfig.Period = 1e5;
        IPConfig.Watermark = IPConfig.NumPages * getpagesize() / 2;
        IPConfig.SampleHandler = CoverSetHandler;
        IPConfig.Opaque = static_cast<void *>(this);

        if (PMU::CreateSampleIP(IPConfig, CoverSetHndl) == PMU_OK) {
            dbg() << DEBUG_HPM << "Register event: cover set sampling.\n";
            PMU::Start(CoverSetHndl);
        }
        break;
    }
    case HPM_FINALIZE:
        if (CoverSetHndl != PMU_INVALID_HNDL)
            PMU::Cleanup(CoverSetHndl);
        break;
    case HPM_START:
    case HPM_STOP:
    default:
        break;
    }
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
