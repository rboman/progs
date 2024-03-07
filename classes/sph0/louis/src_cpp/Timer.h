#ifndef SPH_TIMER_H
#define SPH_TIMER_H

#include <chrono>
#include <iostream>

/// @brief A simple timer class

class Timer
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
        tstart = Clock::now();
        accumulated_time = Duration(0);
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

    friend std::ostream &operator<<(std::ostream &os, const Timer &t);
};

#endif // SPH_TIMER_H