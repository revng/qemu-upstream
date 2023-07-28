/*
 *  (C) 2010 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __LLVM_SOFT_PERFMON_H
#define __LLVM_SOFT_PERFMON_H

#include "utils.h"

#define MAX_SPM_THREADS 256

#define SPM_NONE      (uint64_t)0
#define SPM_BASIC     ((uint64_t)1 << 0)
#define SPM_TRACE     ((uint64_t)1 << 1)
#define SPM_CACHE     ((uint64_t)1 << 2)
#define SPM_PASS      ((uint64_t)1 << 3)
#define SPM_HPM       ((uint64_t)1 << 4)
#define SPM_EXIT      ((uint64_t)1 << 5)
#define SPM_HOTSPOT   ((uint64_t)1 << 6)
#define SPM_ALL       SPM_BASIC | SPM_TRACE | SPM_CACHE | SPM_PASS | SPM_HPM | \
                      SPM_EXIT | SPM_HOTSPOT
#define SPM_NUM       9


/*
 * Software Performance Monitor (SPM)
 */
class SoftwarePerfmon {
public:
    typedef void (*ExitFuncPtr)(void);

    uint64_t Mode;         /* Profile level */
    uint64_t NumInsns;     /* Number of instructions */
    uint64_t NumBranches;  /* Number of branches */
    uint64_t NumLoads;     /* Number of memory loads */
    uint64_t NumStores;    /* Number of memory stores */
    uint64_t NumTraceExits;    /* Count of trace exits */
    uint64_t SampleTime;   /* Process time of the sampling handler. */
    unsigned CoverSet;
    std::vector<std::vector<uint64_t> *> SampleListVec;

    SoftwarePerfmon()
        : Mode(SPM_NONE), NumInsns(0), NumBranches(0), NumLoads(0), NumStores(0),
          NumTraceExits(0), SampleTime(0), CoverSet(90) {}
    SoftwarePerfmon(std::string &ProfileLevel) : SoftwarePerfmon() {
        ParseProfileMode(ProfileLevel);
    }

    bool isEnabled() {
        return Mode != SPM_NONE;
    }

    void registerExitFn(ExitFuncPtr F) {
        ExitFunc.push_back(F);
    }

    void printProfile();

private:
    std::vector<ExitFuncPtr> ExitFunc;

    void ParseProfileMode(std::string &ProfileLevel);
    void printBlockProfile();
    void printTraceProfile();
};

extern SoftwarePerfmon *SP;

#endif /* __LLVM_SOFT_PERFMON_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
