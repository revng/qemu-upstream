/*
 *  (C) 2018 by Computer System Laboratory, IIS, Academia Sinica, Taiwan.
 *      See COPYRIGHT in top-level directory.
 */

#include <algorithm>
#include <signal.h>
#include <sys/time.h>
#include "llvm-soft-perfmon.h"
#include "pmu/pmu-global.h"
#include "pmu/pmu-events.h"


namespace {

/* Mutex  */
class Mutex {
    pthread_mutex_t M;
public:
    Mutex() { pthread_mutex_init(&M, nullptr); }
    inline void acquire() { pthread_mutex_lock(&M);    }
    inline void release() { pthread_mutex_unlock(&M);  }
    inline bool trylock()  { return pthread_mutex_trylock(&M) == 0; }
};

class MutexGuard {
    Mutex &M;
public:
    MutexGuard(Mutex &M) : M(M) { M.acquire(); }
    ~MutexGuard() { M.release(); }
};

}

/*
 * Performance Monitoring Unit (PMU).
 */
namespace pmu {

static Mutex Lock;

SampleList *ReadSampleData(PMUEvent *Event);

/* The timer interrupt handler. */
void DefaultHandler(int signum, siginfo_t *info, void *data)
{
    /* If the thread is signaled while it is currently holding the lock, we
     * might enter deadlock if we attempt to acquire the lock. Use trylock to
     * detect such a condition and return from this handler if we cannot
     * successfully acquire the lock. */
    if (Lock.trylock() == false)
        return;

    /* We have hold the lock. Iterate over all sampling events and process
     * the sample buffer. */

    auto &SampleEvents = EventMgr->SampleEvents;
    if (SampleEvents.empty()) {
        Lock.release();
        return;
    }

    struct timeval Start, End, Elapse;
    if (SP->Mode & SPM_HPM)
        gettimeofday(&Start, nullptr);

    for (auto I = SampleEvents.begin(), E = SampleEvents.end(); I != E; ++I) {
        PMUEvent *Event = *I;
        if (Event->Mode & MODE_SAMPLE) {
            SampleList *Data = ReadSampleData(Event);
            if (Data)
                Event->SampleHandler(Event->Hndl, SampleDataPtr(Data),
                                     Event->Opaque);
        }
    }

    auto &ChangedEvents = EventMgr->ChangedEvents;
    if (!ChangedEvents.empty()) {
        for (auto *Event : ChangedEvents) {
            if (Event->State == STATE_GOTO_STOP) {
                Event->State = STATE_STOP;
                SampleEvents.remove(Event);
            } else if (Event->State == STATE_GOTO_START) {
                Event->State = STATE_START;
                SampleEvents.push_back(Event);
            }
        }
        ChangedEvents.clear();
    }

    if (SP->Mode & SPM_HPM) {
        gettimeofday(&End, nullptr);
        timersub(&End, &Start, &Elapse);
        SP->SampleTime += Elapse.tv_sec * 1e6 + Elapse.tv_usec;
    }

    if (!SampleEvents.empty())
        EventMgr->EventTimer->Start();
    Lock.release();
}

/*
 * Event Manager
 */
EventManager::EventManager()
{
    for (unsigned i = 0; i < PMU_MAX_EVENTS; ++i) {
        Events[i].Hndl = i;
        FreeEvents.push_back(&Events[i]);
    }

    /* Install the signal handler for the timer. */
    struct sigaction act;
    memset(&act, 0, sizeof(struct sigaction));
    act.sa_sigaction = DefaultHandler;
    act.sa_flags = SA_SIGINFO;
    sigaction(PMU_SIGNAL_NUM, &act, 0);

    EventTimer = new Timer(PMU_SIGNAL_NUM, SysConfig.SignalReceiver);
}

EventManager::~EventManager()
{
    EventTimer->Stop();
    delete EventTimer;
}

/* Return the event of the input handle. */
PMUEvent *EventManager::GetEvent(Handle Hndl)
{
    if (Hndl >= PMU_MAX_EVENTS)
        return nullptr;
    return &Events[Hndl];
}

/* Add a counting event and return its handle. */
Handle EventManager::AddEvent(int fd)
{
    MutexGuard Locked(Lock);

    if (FreeEvents.empty())
        return PMU_INVALID_HNDL;

    auto Event = FreeEvents.front();
    FreeEvents.pop_front();

    Event->FD.push_back(fd);
    Event->Data.Base = nullptr;
    Event->Aux.Base = nullptr;
    Event->OverflowHandler = nullptr;

    Event->Mode = MODE_COUNTER;
    Event->State = STATE_STOP;

    return Event->Hndl;
}

/* Add a sampling event and return its handle. */
Handle EventManager::AddSampleEvent(unsigned NumFDs, int *FD, uint64_t DataSize,
                                    void *Data, uint32_t Mode,
                                    SampleConfig &Config)
{
    MutexGuard Locked(Lock);

    if (FreeEvents.empty())
        return PMU_INVALID_HNDL;

    auto Event = FreeEvents.front();
    FreeEvents.pop_front();

    for (unsigned i = 0; i < NumFDs; ++i)
        Event->FD.push_back(FD[i]);

    Event->Data.Base = Data;
    Event->Data.Size = DataSize;
    Event->Data.Prev = 0;
    Event->Aux.Base = nullptr;
    Event->Aux.Size = 0;
    Event->Aux.Prev = 0;
    Event->Watermark = std::min(Config.Watermark, DataSize);
    Event->SampleHandler = Config.SampleHandler;
    Event->Opaque = Config.Opaque;

    Event->Mode = MODE_SAMPLE | Mode;
    Event->State = STATE_STOP;

    return Event->Hndl;
}

/* Notify that an event is started. */
void EventManager::StartEvent(PMUEvent *Event, bool ShouldLock)
{
    if (ShouldLock) {
        MutexGuard Locked(Lock);

        /* We don't add this event to the sampling event list if user doesn't
         * provide a valid overflow handler for a sampling event. */
        if (Event->State == STATE_STOP && Event->OverflowHandler) {
            SampleEvents.push_back(Event);
            EventTimer->Start();
        }
        Event->State = STATE_START;
    } else {
        /* We are within the overflow handling and it's not safe to change the
         * structure of the sampling event list. Here we only change the state
         * of the event and the event list will be fixed at the end of the
         * overflow handling. */
        if (Event->State == STATE_STOP && Event->OverflowHandler) {
            Event->State = STATE_GOTO_START;
            ChangedEvents.push_back(Event);
        }
    }
}

/* Notify that an event is stopped. */
void EventManager::StopEvent(PMUEvent *Event, bool ShouldLock)
{
    if (ShouldLock) {
        /* If this is a sampling event and is currently under sampling, remove
         * it from the sampling event list. */
        Lock.acquire();
        if (Event->State == STATE_START && Event->OverflowHandler) {
            SampleEvents.remove(Event);
            if (SampleEvents.empty())
                EventTimer->Stop();
        }
        Event->State = STATE_STOP;
        Lock.release();
    } else {
        /* We are within the overflow handling and it's not safe to change the
         * structure of the sampling event list. Here we only change the state
         * of the event and the event list will be fixed at the end of the
         * overflow handling. */
        if (Event->State == STATE_START && Event->OverflowHandler) {
            Event->State = STATE_GOTO_STOP;
            ChangedEvents.push_back(Event);
        }
    }
}

/* Notify that an event is deleted. */
void EventManager::DeleteEvent(PMUEvent *Event)
{
    MutexGuard Locked(Lock);

    Event->FD.clear();
    FreeEvents.push_back(Event);
}

/* Stop the event manager. */
void EventManager::Pause()
{
    MutexGuard Locked(Lock);
    if (!SampleEvents.empty())
        EventTimer->Stop();
}

/* Restart the event manager. */
void EventManager::Resume()
{
    MutexGuard Locked(Lock);
    if (!SampleEvents.empty())
        EventTimer->Start();
}

/*
 * Buffer processing
 */
static uint8_t *CopyData(uint8_t *Data, uint64_t DataSize, uint64_t Head, uint64_t Tail) {
    uint64_t Mask = DataSize - 1;
    uint64_t Size = Head - Tail;
    uint64_t HeadOff = Head & Mask;
    uint64_t TailOff = Tail & Mask;
    uint8_t *Buf = new uint8_t[Size];

    if (HeadOff > TailOff) {
        memcpy(Buf, Data + TailOff, Size);
    } else {
        uint64_t UpperSize = DataSize - TailOff;
        memcpy(Buf, Data + TailOff, UpperSize);
        memcpy(&Buf[UpperSize], Data, HeadOff);
    }
    return Buf;
}

/* Process and decode the sample buffer. */
SampleList *ReadSampleData(PMUEvent *Event)
{
    uint64_t Head = perf_read_data_head(Event->Data.Base);
    uint64_t Old = Event->Data.Prev;
    uint64_t Size = Head - Old;
    uint8_t *Data = (uint8_t *)Event->Data.Base + SysConfig.PageSize;
    uint64_t DataSize = Event->Data.Size - SysConfig.PageSize;
    SampleList *OutData = nullptr;

    if (Size < Event->Watermark)
        return OutData;

    OutData = new SampleList;
    if (Size == 0)
        return OutData;

    /* Overwrite head if we failed to keep up with the mmap data. */
    if (Size > DataSize) {
        Event->Data.Prev = Head;
        perf_write_data_tail(Event->Data.Base, Head);
        return OutData;
    }

    /* Process the buffer. */
    uint8_t *Buf = CopyData(Data, DataSize, Head, Old);
    uint8_t *Orig = Buf, *BufEnd = Buf + Size;
    bool SampleIP = Event->Mode & MODE_SAMPLE_IP;
    bool ReadFormat = Event->Mode & MODE_SAMPLE_READ;
    bool ReadGroup = Event->FD.size() > 1;

    while (1) {
        /* Check if we have enough size for the event header. */
        if (Buf + sizeof(struct perf_event_header) > BufEnd)
            break;

        auto *Header = (struct perf_event_header *)Buf;
        Buf += sizeof(struct perf_event_header);

        /* Check if we have enough size for the sample payload. */
        if (Buf + Header->size > BufEnd)
            break;

        if (Header->size == 0)
            continue;

        /* Skip this sample if it's not a PERF_RECORD_SAMPLE type. */
        if (Header->type != PERF_RECORD_SAMPLE) {
            Buf += Header->size;
            continue;
        }

        if (SampleIP) {     /* if PERF_SAMPLE_IP */
            uint64_t ip = *(uint64_t *)Buf;
            Buf += 8;
            OutData->push_back(ip);
        }
        if (ReadFormat) {   /* if PERF_SAMPLE_READ */
            if (ReadGroup) {
                uint64_t nr = *(uint64_t *)Buf;
                Buf += 8;
                while (nr--) {
                    uint64_t value = *(uint64_t *)Buf;
                    Buf += 8;
                    OutData->push_back(value);
                }
            } else {
                uint64_t value = *(uint64_t *)Buf;
                Buf += 8;
                OutData->push_back(value);
            }
        }
    }

    delete [] Orig;

    /* We have finished the buffer. Update data tail. */
    Event->Data.Prev = Head;
    perf_write_data_tail(Event->Data.Base, Head);

    return OutData;
}

/*
 * Timer
 */
Timer::Timer(int Signum, int TID)
{
    struct sigevent ev;
    memset(&ev, 0, sizeof(ev));
    ev.sigev_value.sival_int = 0;
    ev.sigev_notify = SIGEV_SIGNAL | SIGEV_THREAD_ID;
    ev.sigev_signo = Signum;
    ev._sigev_un._tid = TID;
    timer_create(CLOCK_REALTIME, &ev, &T);
}

Timer::~Timer()
{
    Stop();
    timer_delete(T);
}

/* Fire a timer which expires just once.  */
void Timer::Start()
{
    struct itimerspec Timeout;
    Timeout.it_interval.tv_sec = 0;
    Timeout.it_interval.tv_nsec = 0; /* 0 for one-shot timer */
    Timeout.it_value.tv_sec =  0;
    Timeout.it_value.tv_nsec = SysConfig.Timeout;
    timer_settime(T, 0 /* RELATIVE */, &Timeout, NULL);
}

void Timer::Stop()
{
    struct itimerspec Timeout;
    Timeout.it_interval.tv_sec = 0;
    Timeout.it_interval.tv_nsec = 0; /* 0 for one-shot timer */
    Timeout.it_value.tv_sec =  0;
    Timeout.it_value.tv_nsec = 0;
    timer_settime(T, 0 /* RELATIVE */, &Timeout, NULL);
}

} /* namespace pmu */

/*
 * vim: ts=8 sts=4 sw=4 expandtab
 */
