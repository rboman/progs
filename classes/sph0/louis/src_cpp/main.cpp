// !> SPH simulation
// @n This program is used to solve the Navier-Stokes equations
//    using the SPH method. A number of files must be given:
//    paths.txt, *.prm, *.fp and *.mp.
// @warning The domain must be cubic!
// @brief   Main program to launch a SPH simulation
// @author  Louis Goffin, Romain Boman
// @date    2013-05-26
// @version 1.0.0

#include "Model.h"
#include "QtVTKHook.h"
#include <iostream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif

int
main(int argc, char *argv[])
{
    int retcode = EXIT_SUCCESS;

    print_banner();

    // QtVTKHook::standalone_VTK_demo(); // is VTK working?

    try
    {
        timers["TOTAL"].start();
        Model model;
        model.initialise();
        QtVTKHook gui(argc, argv, model);

        model.solve();

        timers["TOTAL"].stop();
        print_timers();

        gui.loop();
    }
    catch (const std::exception &e)
    {
        std::cerr << "\n** ERROR: " << e.what() << "\n\n";
        retcode = EXIT_FAILURE;
    }

    return retcode;
}
