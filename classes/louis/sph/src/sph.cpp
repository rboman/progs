#include "sph.h"
#include <iostream>
#include <fstream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif

namespace sph
{
/// global variables
SPH_API std::map<std::string, Timer> g_timers;
}; // namespace sph

using namespace sph;

SPH_API void
sph::print_banner()
{
#ifdef _OPENMP
    std::cout << "OpenMP available: OMP_NUM_THREADS=" << omp_get_max_threads() << "\n";
#else
    std::cout << "OpenMP not available.\n";
#endif

#ifdef NDEBUG
    std::cout << "code built in RELEASE mode.\n";
#else
    std::cout << "code built in DEBUG mode.\n";
#endif

#ifdef SPH_USE_GUI
    std::cout << "code built with GUI.\n";
#else
    std::cout << "code built without GUI.\n";
#endif
}

SPH_API void
sph::print_timers()
{
    double cpu_total = g_timers["TOTAL"].elapsed();
    double cpu_gui = 0.0;
    if (g_timers.find("GUI") != g_timers.end())
        cpu_gui = g_timers["GUI"].elapsed();

    double cpu_real = cpu_total - cpu_gui;

    auto f(std::cout.flags()); // better choice: "std::format" in C++20
    std::cout << "\nTimers:\n";
    for (auto &t : g_timers)
    {
        std::cout << std::setw(20) << t.first << " = "
                  << std::setw(10) << std::fixed << std::setprecision(2) << t.second << "s"
                  << std::setw(10) << t.second.elapsed() / g_timers["TOTAL"].elapsed() * 100 << "%";
        if (t.first == "TOTAL")
            std::cout << std::setw(10) << 100.0 << "%";
        else if (t.first != "GUI")
            std::cout << std::setw(10) << t.second.elapsed() / cpu_real * 100 << "%";

        std::cout << std::endl;
    }
    std::cout.flags(f); // restore flags
}

SPH_API void
sph::save_timers()
{
    std::ofstream file("timers.txt");
    for (auto &t : g_timers)
        file << std::setw(20) << t.first << '\t' << t.second.elapsed() << '\n';
}
