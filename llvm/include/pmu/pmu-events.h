/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef __PMU_EVENTS_H
#define __PMU_EVENTS_H

#include <list>
#include <vector>
#include <signal.h>
#include "pmu-global.h"
#include "pmu.h"

namespace pmu {

#define PMU_MAX_EVENTS  (1024)

class Timer;

/* Mode of the event. */
enum {
    MODE_NONE                = 0,
    MODE_COUNTER             = ((uint32_t)1U << 1),
    MODE_SAMPLE              = ((uint32_t)1U << 2),
    MODE_SAMPLE_IP           = ((uint32_t)1U << 3),
    MODE_SAMPLE_READ         = ((uint32_t)1U << 4),
};

/* State of the event. */
enum {
    STATE_STOP       = 0,
    STATE_START      = ((uint32_t)1U << 1),
    STATE_GOTO_STOP  = ((uint32_t)1U << 2),
    STATE_GOTO_START = ((uint32_t)1U << 3),
};

/* Sampling mmap buffer information. */
struct MMap {
    void *Base;
    uint64_t Size;
    uint64_t Prev;
};

/* Event. */
struct PMUEvent {
    PMUEvent() : Hndl(0), Mode(MODE_NONE), State(STATE_STOP) {}

    Handle Hndl;          /* Unique handle value */
    int Mode;             /* Event mode */
    int State;            /* Current event state */
    std::vector<int> FD;  /* Opened fd(s) of this event */
    MMap Data;            /* mmap data info */
    MMap Aux;             /* mmap aux info */
    uint64_t Watermark;   /* The bytes before wakeup */
    /* Overflow handling function pointer */
    union {
        void *OverflowHandler;
        SampleHandlerTy SampleHandler;
    };
    void *Opaque;         /* Opaque pointer passed to the overflow handler. */

    int getFD() { return FD[0]; }   /* Group leader fd */
};

/*
 * Event Manager.
 */
class EventManager {
    typedef std::list<PMUEvent *> EventList;

    PMUEvent Events[PMU_MAX_EVENTS]; /* Pre-allocated events */
    EventList FreeEvents;            /* Free events */
    EventList SampleEvents;          /* Sampling events */
    Timer *EventTimer;               /* Timer for sampling events. */
    std::vector<PMUEvent *> ChangedEvents;

public:
    EventManager();
    ~EventManager();

    /* Return the event of the input handle. */
    PMUEvent *GetEvent(Handle Hndl);

    /* Add a counting event and return its handle. */
    Handle AddEvent(int fd);

    /* Add a sampling event and return its handle. */
    Handle AddSampleEvent(unsigned NumFDs, int *FD, uint64_t DataSize, void *Data,
                          uint32_t Mode, SampleConfig &Config);

    /* Notify that an event is started. */
    void StartEvent(PMUEvent *Event, bool ShouldLock = true);

    /* Notify that an event is stopped. */
    void StopEvent(PMUEvent *Event, bool ShouldLock = true);

    /* Notify that an event is deleted. */
    void DeleteEvent(PMUEvent *Event);

    /* Stop the event manager. */
    void Pause();

    /* Restart the event manager. */
    void Resume();

    friend void DefaultHandler(int signum, siginfo_t *info, void *data);
};

/* Interval timer. */
class Timer {
    timer_t T;

public:
    Timer(int Signum, int TID);
    ~Timer();

    /* Start a timer that expires just once.  */
    void Start();

    /* Stop a timer.*/
    void Stop();
};

} /* namespace pmu */

#endif /* __PMU_EVENTS_H */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
