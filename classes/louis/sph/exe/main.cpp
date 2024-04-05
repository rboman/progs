// !> SPH simulation
// @n This program is used to solve the Navier-Stokes equations
//    using the SPH method. A number of files must be given:
//    paths.txt, *.prm, *.fp and *.mp.
// @warning The domain must be cubic!
// @brief   Main program to launch a SPH simulation
// @author  Louis Goffin, Romain Boman
// @date    2013-05-26
// @version 1.0.0

#include "sph.h"
#include "sphModel.h"
#include "sphDisplayHook.h"
#include <iostream>
#include <iomanip>
#ifdef _OPENMP
#include <omp.h>
#endif
#ifdef SPH_USE_GUI
#include "sphQtVTKHook.h"
#endif

using namespace sph;

int
main(int argc, char *argv[])
{
    int retcode = EXIT_SUCCESS;

    read_args(argc, argv);
    print_banner();

    // QtVTKHook::standalone_VTK_demo(); // is VTK working?

    try
    {
        g_timers["TOTAL"].start();
        Model model;
        model.initialise();

        DisplayHook *gui = nullptr;

#ifdef SPH_USE_GUI
        if(!g_nogui)
            gui = new QtVTKHook(argc, argv, model);
#endif

        model.solve();

        g_timers["TOTAL"].stop();
        print_timers();
        save_timers();

        if(gui)
        {
            std::cout << "\n  >>> Close window to Quit...\n" << std::endl;
            gui->loop();
            delete gui;
        }
    }
    catch (const std::exception &e)
    {
        std::cerr << "\n** ERROR: " << e.what() << "\n\n";
        retcode = EXIT_FAILURE;
    }

    return retcode;
}
