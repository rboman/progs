// !> SPH simulation
// @n This program is used to solve the Navier-Stokes equations
//    using the SPH method. A number of files must be given:
//    paths.txt, *.prm, *.fp and *.mp.
// @warning The domain must be cubic!
// @brief   Main program to launch a SPH simulation
// @author  Louis Goffin, Romain Boman
// @date    2013-05-26
// @version 1.0.0

#include "ParticleManager.h"
#include <iostream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif

int
main()
{
    int retcode = EXIT_SUCCESS;

    print_banner();

    timers["TOTAL"].start();
    try
    {
        ParticleManager manager;
        manager.initialise();
        manager.solve();
    }
    catch (const std::exception &e)
    {
       std::cerr << "\n** ERROR: " << e.what() << "\n\n";
       retcode = EXIT_FAILURE;
    }
    timers["TOTAL"].stop();

    print_timers();

    return retcode;
}
