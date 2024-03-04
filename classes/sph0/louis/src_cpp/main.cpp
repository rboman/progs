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

int
main()
{
    std::cout << "============= SPH_simulation (L.Goffin)\n";

    auto t1 = std::chrono::high_resolution_clock::now();

    ParticleManager manager;
    manager.initialisation();
    manager.solver();

    auto t2 = std::chrono::high_resolution_clock::now();
    std::cout << "Elapsed real time = " << std::chrono::duration_cast<std::chrono::duration<double>>(t2 - t1).count() << '\n';

    return 0;
}
