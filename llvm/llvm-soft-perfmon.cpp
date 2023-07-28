/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <iostream>
#include <sstream>
#include "tracer.h"
#include "utils.h"
#include "llvm.h"
#include "llvm-target.h"
#include "llvm-soft-perfmon.h"


extern LLVMEnv *LLEnv;
extern unsigned ProfileThreshold;
extern unsigned PredictThreshold;

/*
 * Software Performance Monitor (SPM)
 */
void SoftwarePerfmon::ParseProfileMode(std::string &ProfileLevel)
{
    static std::string profile_str[SPM_NUM] = {
        "none", "basic", "trace", "cache", "pass", "hpm", "exit", "hotspot", "all"
    };
    static uint64_t profile_enum[SPM_NUM] = {
        SPM_NONE, SPM_BASIC, SPM_TRACE, SPM_CACHE, SPM_PASS, SPM_HPM,
        SPM_EXIT, SPM_HOTSPOT, SPM_ALL,
    };

    if (ProfileLevel.empty())
        return;

    std::istringstream ss(ProfileLevel);
    std::string token;
    while(getline(ss, token, ',')) {
        for (int i = 0; i != SPM_NUM; ++i) {
            if (token == profile_str[i]) {
                Mode |= profile_enum[i];
                break;
            }
        }
    }
}

void SoftwarePerfmon::printProfile()
{
    if (!isEnabled())
        return;

    if (LLVMEnv::TransMode == TRANS_MODE_NONE ||
        LLVMEnv::TransMode == TRANS_MODE_INVALID)
        return;

    if (LLVMEnv::TransMode == TRANS_MODE_BLOCK)
        printBlockProfile();
    else
        printTraceProfile();
}

void SoftwarePerfmon::printBlockProfile()
{
    LLVMEnv::TransCodeList &TransCode = LLEnv->getTransCode();
    uint32_t GuestSize = 0, GuestICount = 0, HostSize = 0;
    uint64_t TransTime = 0, MaxTime = 0;

    for (auto TC : TransCode) {
        TraceInfo *Trace = TC->Trace;
        TranslationBlock *TB = TC->EntryTB;
        GuestSize += TB->size;
        GuestICount += TB->icount;
        HostSize += TC->Size;
        TransTime += Trace->TransTime;
        if (Trace->TransTime > MaxTime)
            MaxTime = Trace->TransTime;
    }

    auto &OS = DM.debug();
    OS << "\nBlock statistic:\n"
       << "Num of Blocks    : " << TransCode.size() << "\n"
       << "G/H Code Size    : " << GuestSize << "/" << HostSize << "bytes\n"
       << "Guest ICount     : " << GuestICount << "\n"
       << "Translation Time : " << format("%.6f", (double)TransTime * 1e-6)
                                << " seconds (max=" << MaxTime /1000 << " ms)\n";
}

static void printBasic(LLVMEnv::TransCodeList &TransCode)
{
    uint32_t GuestSize = 0, GuestICount = 0, HostSize = 0;
    uint32_t NumBlock = 0, NumLoop = 0, NumExit = 0, NumIndirectBr = 0;
    uint32_t MaxBlock = 0, MaxLoop = 0, MaxExit = 0, MaxIndirectBr = 0;
    uint64_t TransTime = 0, MaxTime = 0;
    unsigned NumTraces = TransCode.size();
    std::map<unsigned, unsigned> LenDist;

    for (auto TC : TransCode) {
        TraceInfo *Trace = TC->Trace;
        TBVec &TBs = Trace->TBs;
        for (unsigned i = 0, e = TBs.size(); i != e; ++i) {
            GuestSize += TBs[i]->size;
            GuestICount += TBs[i]->icount;
        }
        HostSize += TC->Size;

        NumBlock += TBs.size();
        NumLoop += Trace->NumLoop;
        NumExit += Trace->NumExit;
        NumIndirectBr += Trace->NumIndirectBr;
        TransTime += Trace->TransTime;

        if (TBs.size() > MaxBlock)
            MaxBlock = TBs.size();
        if (Trace->NumLoop > MaxLoop)
            MaxLoop = Trace->NumLoop;
        if (Trace->NumExit > MaxExit)
            MaxExit = Trace->NumExit;
        if (Trace->NumIndirectBr > MaxIndirectBr)
            MaxIndirectBr = Trace->NumIndirectBr;
        if (Trace->TransTime > MaxTime)
            MaxTime = Trace->TransTime;
        LenDist[TBs.size()]++;
    }

    auto &OS = DM.debug();
    OS << "Trace statistic:\n"
       << "Num of Traces    : " << NumTraces << "\n"
       << "Profile Thres.   : " << ProfileThreshold << "\n"
       << "Predict Thres.   : " << PredictThreshold << "\n"
       << "G/H Code Size    : " << GuestSize << "/" << HostSize << " bytes\n"
       << "Translation Time : " << format("%.6f", (double)TransTime * 1e-6)
                                << " seconds (max=" << MaxTime /1000 << " ms)\n"
       << "Average # Blocks : " << format("%.1f", (double)NumBlock / NumTraces)
                                << " (max=" << MaxBlock << ")\n"
       << "Average # Loops  : " << format("%.1f", (double)NumLoop / NumTraces)
                                << " (max=" << MaxLoop << ")\n"
       << "Average # Exits  : " << format("%.1f", (double)NumExit / NumTraces)
                                << " (max=" << MaxExit << ")\n"
       << "Average # IBs    : " << format("%.1f", (double)NumIndirectBr / NumTraces)
                                << " (max=" << MaxIndirectBr << ")\n"
       << "Flush Count      : " << LLEnv->getNumFlush() << "\n";

    OS << "Trace length distribution: (1-" << MaxBlock << ")\n    ";
    for (unsigned i = 1; i <= MaxBlock; i++)
        OS << LenDist[i] << " ";
    OS << "\n";
}

static void printTraceExec(LLVMEnv::TransCodeList &TransCode)
{
    unsigned NumThread = 0;
    for (auto next_cpu = first_cpu; next_cpu != nullptr;
         next_cpu = CPU_NEXT(next_cpu))
        NumThread++;

    /* Detailed trace information and runtime counters. */
    auto &OS = DM.debug();
    OS << "----------------------------\n"
       << "Trace execution information:\n";

    unsigned NumTraces = TransCode.size();
    for (unsigned i = 0; i != NumThread; ++i) {
        unsigned TraceUsed = 0;

        OS << ">\n"
           << "Thread " << i << ":\n"
           << "                                   dynamic exec count\n"
           << "  id      pc      #loop:#exit      loop      ibtc      exit\n";
        for (unsigned j = 0; j != NumTraces; ++j) {
            TraceInfo *Trace = TransCode[j]->Trace;
            uint64_t *Counter = Trace->ExecCount[i];
            if (Counter[0] + Counter[1] + Counter[2] == 0)
                continue;
            TraceUsed++;
            OS << format("%4d", j) << ") "
               << format("0x%08" PRIx, Trace->getEntryPC()) << "    "
               << format("%2d", Trace->NumLoop)   << "    "
               << format("%2d", Trace->NumExit)   << "   "
               << format("%8" PRId64, Counter[0]) << "  "
               << format("%8" PRId64, Counter[1]) << "  "
               << format("%8" PRId64, Counter[2]) << "\n";
        }
        OS << "Trace used: " << TraceUsed << "/" << NumTraces <<"\n";
    }
}

static void printHPM()
{
    auto &OS = DM.debug();
    OS << "Num of Insns     : " << SP->NumInsns << "\n"
       << "Num of Loads     : " << SP->NumLoads << "\n"
       << "Num of Stores    : " << SP->NumStores << "\n"
       << "Num of Branches  : " << SP->NumBranches << "\n"
       << "Sample Time      : " << format("%.6f seconds", (double)SP->SampleTime * 1e-6)
       << "\n";
}

static void printHotspot(unsigned &CoverSet,
                         std::vector<std::vector<uint64_t> *> &SampleListVec)
{
    auto &OS = DM.debug();
    auto &TransCode = LLEnv->getTransCode();
    auto &SortedCode = LLEnv->getSortedCode();
    uint64_t BlockCacheStart = (uintptr_t)tcg_ctx_global.code_gen_buffer;
    uint64_t BlockCacheEnd = BlockCacheStart + tcg_ctx_global.code_gen_buffer_size;
    uint64_t TraceCacheStart = (uintptr_t)LLVMEnv::TraceCache;
    uint64_t TraceCacheEnd = TraceCacheStart + LLVMEnv::TraceCacheSize;
    uint64_t TotalSamples = 0;
    uint64_t NumBlockCache = 0, NumTraceCache = 0, NumOther = 0;

    for (auto *L : SampleListVec) {
        for (uint64_t IP : *L) {
            if (IP >= BlockCacheStart && IP < BlockCacheEnd)
                NumBlockCache++;
            else if (IP >= TraceCacheStart && IP < TraceCacheEnd)
                NumTraceCache++;
            else
                NumOther++;

            auto IT = SortedCode.upper_bound(IP);
            if (IT == SortedCode.begin())
                continue;
            auto TC = (--IT)->second;
            if (IP < (uint64_t)TC->Code + TC->Size)
                TC->SampleCount++;;
        }
        delete L;
    }

    TotalSamples = NumBlockCache + NumTraceCache + NumOther;
    if (TotalSamples == 0 || TransCode.empty()) {
        OS << CoverSet << "% CoverSet     : 0\n";
        return;
    }

    /* Print the time breakdown of block cache, trace cache and other. */
    char buf[128] = {'\0'};
    double RatioBlockCache = (double)NumBlockCache * 100 / TotalSamples;
    double RatioTraceCache = (double)NumTraceCache * 100 / TotalSamples;
    sprintf(buf, "block (%.1f%%) trace (%.1f%%) other (%.1f%%)", RatioBlockCache,
            RatioTraceCache, 100.0f - RatioBlockCache - RatioTraceCache);
    OS << "Breakdown        : " << buf << "\n";

    /* Print the amount of traces in the cover set. */
    std::map<TranslatedCode *, unsigned> IndexMap;
    for (unsigned i = 0, e = TransCode.size(); i != e; ++i)
        IndexMap[TransCode[i]] = i;

    LLVMEnv::TransCodeList Covered(TransCode.begin(), TransCode.end());
    std::sort(Covered.begin(), Covered.end(),
            [](const TranslatedCode *a, const TranslatedCode *b) {
                return a->SampleCount > b->SampleCount;
            });

    uint64_t CoverSamples = TotalSamples * CoverSet / 100;
    uint64_t AccuSamples = 0;
    unsigned NumTracesInCoverSet = 0;
    for (TranslatedCode *TC : Covered) {
        if (AccuSamples >= CoverSamples || TC->SampleCount == 0)
            break;
        NumTracesInCoverSet++;
        AccuSamples += TC->SampleCount;
    }

    OS << CoverSet << "% CoverSet     : " << NumTracesInCoverSet << "\n";

    if (NumTracesInCoverSet == 0)
        return;

    /* Print the percentage of time of the traces in the cover set. */
    if (DM.getDebugMode() & DEBUG_IR_OPT) {
        OS << "Traces of CoverSet:\n";
        for (unsigned i = 0; i < NumTracesInCoverSet; ++i) {
            TranslatedCode *TC = Covered[i];
            sprintf(buf, "%4d (%.1f%%): ", IndexMap[TC],
                    (double)TC->SampleCount * 100 / TotalSamples);
            OS << buf;
            int j = 0;
            for (auto *TB: TC->Trace->TBs) {
                std::stringstream ss;
                ss << std::hex << TB->pc;
                OS << (j++ == 0 ? "" : ",") << ss.str();
            }
            OS << "\n";
        }
    } else {
        unsigned top = 10;

        OS << "Percentage of CoverSet (top 10): ";
        if (NumTracesInCoverSet < top)
            top = NumTracesInCoverSet;
        for (unsigned i = 0; i < top; ++i) {
            TranslatedCode *TC = Covered[i];
            sprintf(buf, "%.1f%%", (double)TC->SampleCount * 100 / TotalSamples);
            OS << (i == 0 ? "" : " ") << buf;
        }
        OS << "\n";
    }
}

void SoftwarePerfmon::printTraceProfile()
{
    auto &OS = DM.debug();
    unsigned NumTraces = LLEnv->getTransCode().size();

    OS << "\n";
    if (NumTraces == 0) {
        OS << "Trace statistic:\n"
           << "Num of Traces  : " << NumTraces << "\n\n";
        return;
    }

    /* Static information */
    if (Mode & SPM_BASIC)
        printBasic(LLEnv->getTransCode());
    if (Mode & SPM_EXIT)
        OS << "Num of TraceExit : " << NumTraceExits << "\n";
    if (Mode & SPM_HPM)
        printHPM();
    if (Mode & SPM_HOTSPOT)
        printHotspot(CoverSet, SP->SampleListVec);

    /* Code cache infomation - start address and size */
    if (Mode & SPM_CACHE) {
        size_t BlockSize = (uintptr_t)tcg_ctx_global.code_gen_ptr -
                           (uintptr_t)tcg_ctx_global.code_gen_buffer;
        size_t TraceSize = LLEnv->getMemoryManager()->getCodeSize();

        OS << "-------------------------\n"
           << "Block/Trace Cache information:\n";
        OS << "Block: start=" << tcg_ctx_global.code_gen_buffer
           << " size=" << tcg_ctx_global.code_gen_buffer_size
           << " code=" << format("%8d", BlockSize) << " (ratio="
           << format("%.2f", (double)BlockSize * 100 / tcg_ctx_global.code_gen_buffer_size)
           << "%)\n";
        OS << "Trace: start=" << LLVMEnv::TraceCache
           << " size=" << LLVMEnv::TraceCacheSize
           << " code=" << format("%8d", TraceSize) << " (ratio="
           << format("%.2f", (double)TraceSize * 100 / LLVMEnv::TraceCacheSize)
           << "%)\n\n";
    }

    if (Mode & SPM_TRACE)
        printTraceExec(LLEnv->getTransCode());

    if ((Mode & SPM_PASS) && !ExitFunc.empty()) {
        OS << "\n-------------------------\n"
           << "Pass information:\n";
        for (unsigned i = 0, e = ExitFunc.size(); i != e; ++i)
            (*ExitFunc[i])();
    }
}

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */

