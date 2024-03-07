#include "sph.h"
#include <iostream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif

std::map<std::string, Timer> timers;

void
print_banner()
{
#ifdef _OPENMP
    std::cout << "OpenMP available: OMP_NUM_THREADS=" << omp_get_max_threads() << "\n";
#else
    std::cout << "OpenMP not available.\n";
#endif

#ifdef NDEBUG
    // code has been configured with "cmake -DCMAKE_BUILD_TYPE=Release .."
    std::cout << "code built in RELEASE mode.\n";
#else
    // code has been configured with "cmake .."
    std::cout << "code built in DEBUG mode.\n";
#endif
}

void
print_timers()
{
    auto f(std::cout.flags()); // better choice: "std::format" in C++20
    std::cout << "\nTimers:\n";
    for (auto &t : timers)
        std::cout << std::setw(20) << t.first << " = "
                  << std::setw(10) << std::fixed << std::setprecision(2) << t.second << "s"
                  << std::setw(10) << t.second.elapsed() / timers["TOTAL"].elapsed() * 100 << "%\n";
    std::cout.flags(f); // restore flags
}
