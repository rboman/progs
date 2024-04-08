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
//SPH_API bool g_nogui = false;
SPH_API bool g_nosave = false;
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
    auto f(std::cout.flags()); // better choice: "std::format" in C++20
    std::cout << "\nTimers:\n";
    for (auto &t : g_timers)
        std::cout << std::setw(20) << t.first << " = "
                  << std::setw(10) << std::fixed << std::setprecision(2) << t.second << "s"
                  << std::setw(10) << t.second.elapsed() / g_timers["TOTAL"].elapsed() * 100 << "%"
                  << std::endl;
    std::cout.flags(f); // restore flags
}

SPH_API void
sph::save_timers()
{
    std::ofstream file("timers.txt");
    for (auto &t : g_timers)
        file << std::setw(20) << t.first << '\t' << t.second.elapsed() << '\n';
}

SPH_API void
sph::read_args(int argc, char *argv[])
{
    for (int i = 1; i < argc; i++)
    {
        // if (std::string(argv[i]) == "--nogui")
        //     g_nogui = true;
        if (std::string(argv[i]) == "--nosave")
            g_nosave = true;
    }
}
