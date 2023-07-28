/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __PMU_UTILS_H
#define __PMU_UTILS_H

#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include "perf_event.h"

#ifndef ACCESS_ONCE
#define ACCESS_ONCE(x) (*(volatile decltype(x) *)&(x))
#endif

namespace pmu {

static inline int sys_perf_event_open(struct perf_event_attr *attr, pid_t pid,
                                      int cpu, int group_fd,
                                      unsigned long flags) {
    return syscall(__NR_perf_event_open, attr, pid, cpu, group_fd, flags);
}

static inline void perf_attr_init(struct perf_event_attr *attr, int type,
                                  int config) {
    memset(attr, 0, sizeof(struct perf_event_attr));
    attr->type = type;
    attr->config = config;
    attr->size = sizeof(struct perf_event_attr);
    attr->disabled = 1;
    attr->exclude_kernel = 1;
    attr->exclude_guest = 1;
    attr->exclude_hv = 1;
}

static inline int perf_event_start(int fd) {
    return ioctl(fd, PERF_EVENT_IOC_ENABLE, 0);
}

static inline int perf_event_stop(int fd) {
    return ioctl(fd, PERF_EVENT_IOC_DISABLE, 0);
}

static inline int perf_event_reset(int fd) {
    return ioctl(fd, PERF_EVENT_IOC_RESET, 0);
}

static inline int perf_event_set_filter(int fd, const char *arg) {
    return ioctl(fd, PERF_EVENT_IOC_SET_FILTER, (void *)arg);
}

static inline uint64_t perf_read_data_head(void *header) {
    struct perf_event_mmap_page *pc = (struct perf_event_mmap_page *)header;
    uint64_t head = ACCESS_ONCE(pc->data_head);
    pmu_rmb();
    return head;
}

static inline void perf_write_data_tail(void *header, uint64_t val) {
    struct perf_event_mmap_page *pc = (struct perf_event_mmap_page *)header;
    pmu_mb();
    pc->data_tail = val;
}

static inline uint64_t perf_read_aux_head(void *header) {
    struct perf_event_mmap_page *pc = (struct perf_event_mmap_page *)header;
    uint64_t head = ACCESS_ONCE(pc->aux_head);
    pmu_rmb();
    return head;
}

static inline void perf_write_aux_tail(void *header, uint64_t val) {
    struct perf_event_mmap_page *pc = (struct perf_event_mmap_page *)header;
    pmu_mb();
    pc->aux_tail = val;
}

static inline int isPowerOf2(uint64_t value) {
    if (!value)
        return 0;
    return !(value & (value - 1));
}

/* Convert system errno to PMU error code. */
static inline int ErrorCode(int err)
{
    switch (err) {
        case EPERM:
        case EACCES: return PMU_EPERM;
        case ENOMEM: return PMU_ENOMEM;
        default:     return PMU_EEVENT;
    }
}

} /* namespace pmu */

#endif /* __PMU_UTILS_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
