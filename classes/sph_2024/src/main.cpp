#include "cube.h"
#include "export.h"
#include <iostream>
#include <fstream>
#include <cassert>
#ifdef _OPENMP
#include <omp.h>
#endif
#include "nlohmann/json.hpp"
using json = nlohmann::json;

/**
 * @brief Dummy SPH simulation testing paraview export formats
 */

int main(int argc, char *argv[])
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

    if (argc != 2)
    {
        std::cout << "\nusage: " << argv[0] << " <param.json>\n\n";
        return EXIT_FAILURE;
    }

    // read input data from json file given as argument

    std::ifstream inputf(argv[1]);
    json data = json::parse(inputf);

    // print input data to screen
    std::cout << argv[1] << ":\n" << data.dump(4) << std::endl;

    std::vector<double> pos;
    std::vector<double> o = data["o"];
    std::vector<double> L = data["L"];
    double s = data["s"];
    int nstepT = data["nstepT"];

    // creation of a cube of particles    
    meshcube(&o[0], &L[0], s, pos);

    // creation of dummy pressure/density/velocity fields &
    int nbp = (int)pos.size() / 3;
    std::cout << nbp << " particles created\n";
    std::vector<double> pressure(nbp);
    std::vector<double> density(nbp);
    std::vector<double> velocity(nbp * 3);

    std::map<std::string, std::vector<double> *> scalars;
    std::map<std::string, std::vector<double> *> vectors;
    scalars["pressure"] = &pressure;
    scalars["density"] = &density;
    vectors["velocity"] = &velocity;

    // time-marching loop

    std::cout << "starting time loop...\n";
    for (int nstep = 0; nstep < nstepT; ++nstep)
    {
        double a = nstep / double(nstepT) * 8 * atan(1.0);

        // generate dummy results
        for (int i = 0; i < nbp; ++i)
        {
            pos[3 * i + 2] -= 0.5 / nstepT;

            double x = pos[3 * i + 0];
            double y = pos[3 * i + 1];
            double z = pos[3 * i + 2];

            pressure[i] = z / 2 + sin(a);
            density[i] = y + cos(a) + 2;
            velocity[3 * i + 0] = x - (o[0] + L[0] / 2 + L[0] * sin(a));
            velocity[3 * i + 1] = y - (o[1] + L[1] / 2 + L[1] * cos(a));
            velocity[3 * i + 2] = z - (o[2] + L[2] / 2);
        }

        // save results to disk
        export_particles("sph", nstep, pos, scalars, vectors);
    }
    std::cout << "done.\n";
    return EXIT_SUCCESS;
}
