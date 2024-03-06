// !> SPH simulation
// @n This program is used to solve the Navier-Stokes equations
//    using the SPH method. A number of files must be given:
//    paths.txt, *.prm, *.fp and *.mp.
// @warning The domain must be cubic!
// @brief   Main program to launch a SPH simulation
// @author  Louis Goffin
// @date    2013-05-26
// @version 1.0.0

#include "ParticleManager.h"
#include <iostream>
#include <chrono>
#ifdef _OPENMP
#include <omp.h>
#endif

int
main()
{
    std::cout << "============= SPH_simulation (L.Goffin)\n";

    // configure output stream to output double as in fortran
    std::cout.precision(15);
    std::cout.setf(std::ios::scientific, std::ios::floatfield);
    

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

    auto t1 = std::chrono::high_resolution_clock::now();

    try
    {
        ParticleManager manager;
        manager.initialisation();
        manager.solver();
    }
    catch (const std::exception &e)
    {
       std::cerr << "ERROR: " << e.what() << '\n';
    }

    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << "Elapsed real time = " << std::chrono::duration_cast<std::chrono::duration<double>>(t2 - t1).count() << '\n';

    return EXIT_SUCCESS;
}
