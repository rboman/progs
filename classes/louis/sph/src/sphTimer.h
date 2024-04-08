#ifndef SPH_TIMER_H
#define SPH_TIMER_H

#include "sph.h"
#include <chrono>
#include <iostream>

namespace sph {

/// A simple timer class.

class SPH_API Timer
{
    // Type aliases
    using Clock = std::chrono::high_resolution_clock;
    using TimePoint = Clock::time_point;
    using Duration = std::chrono::duration<double>;

    bool running;
    TimePoint tstart;
    Duration accumulated_time;

public:
    Timer()
    {
        running = false;
        reset();
    }

    void start()
    {
        running = true;
        tstart = Clock::now();
    }

    void stop()
    {
        if (running)
        {
            auto tnow = Clock::now();
            accumulated_time += tnow - tstart;
            running = false;
        }
    }

    void reset()
    {
        accumulated_time = Duration(0);
        tstart = Clock::now();
    }

    double elapsed() const
    {
        if (running)
        {
            auto tnow = std::chrono::high_resolution_clock::now();
            return std::chrono::duration_cast<std::chrono::duration<double>>(accumulated_time + (tnow - tstart)).count();
        }
        else
            return std::chrono::duration_cast<std::chrono::duration<double>>(accumulated_time).count();
    }

    friend SPH_API std::ostream &operator<<(std::ostream &os, const Timer &t);
};

}; // namespace sph

#endif // SPH_TIMER_H