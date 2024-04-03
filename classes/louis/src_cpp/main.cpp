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
#include <iostream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif
#ifdef SPH_USE_GUI
#include "QtVTKHook.h"
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

#ifdef SPH_USE_GUI
        QtVTKHook gui(argc, argv, model);
#endif

        model.solve();

        timers["TOTAL"].stop();
        print_timers();

#ifdef SPH_USE_GUI
        gui.loop();
#endif

    }
    catch (const std::exception &e)
    {
        std::cerr << "\n** ERROR: " << e.what() << "\n\n";
        retcode = EXIT_FAILURE;
    }

    return retcode;
}
