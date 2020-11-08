// example of sequential program

#include "vtl.h"
#include "vtlSPoints.h"
#include "vtl_spoints.h"

#include <string>
#include <vector>
#include <iostream>
#include <map>
#include <cmath>

int main(int argc, char *argv[])
{

#if defined(_MSV_VER) && (_MSC_VER < 1900)
    // Microsoft utilise 3 chiffres par defaut dans l'exposant
    // c'est corrige dans le VS2015 (MSVC 19)
    _set_output_format(_TWO_DIGIT_EXPONENT);
#endif
    
    // Global grid parameters

    SPoints grid;
    grid.o = Vec3d(10.0, 10.0, 10.0); // origin
    Vec3d L(5.0, 3.0, 4.0); // box dimensions

    grid.np1 = Vec3i(0, 0, 0);       // first index
    grid.np2 = Vec3i(5, 3, 4); // last index

    grid.dx = L / (grid.np() - 1);  // compute spacing

    int nstepT = 1; // nb of time steps

    // creation of dummy fields
    int nbp = grid.nbp();

    std::vector<double> scalarX(nbp);
    std::vector<double> scalarY(nbp);
    std::vector<double> scalarZ(nbp);
    std::vector<double> vectorSIN(nbp * 3); // vector field

    grid.scalars["scalar_X"] = &scalarX; // no space allowed in LEGACY format!
    grid.scalars["scalar_Y"] = &scalarY;
    grid.scalars["scalar_Z"] = &scalarZ;
    grid.vectors["vector_SIN"] = &vectorSIN;

    std::cout << grid;

    // time step loop
    for (int nstep = 0; nstep < nstepT; ++nstep)
    {
        double time = double(nstep) / nstepT;

        //int idx = 0;
        int npz1 = grid.np1[2];
        int npz2 = grid.np2[2];
        Vec3i np = grid.np();

        for (int k = npz1; k <= npz2; ++k)
        {
            double z = k * grid.dx[2] + grid.o[2];
            int npy1 = grid.np1[1];
            int npy2 = grid.np2[1];            
            for (int j = npy1; j <= npy2; ++j)
            {
                double y = j * grid.dx[1] + grid.o[1];
                int npx1 = grid.np1[0];
                int npx2 = grid.np2[0];                 
                for (int i = npx1; i <= npx2; ++i)
                {
                    double x = i * grid.dx[0] + grid.o[0];

                    int idx = (k-npz1) * (np[1] * np[0]) + (j-npy1) * np[0] + (i-npx1);

                    scalarX[idx] = x;
                    scalarY[idx] = y;
                    scalarZ[idx] = z;
                    vectorSIN[idx * 3 + 0] = sin(2 * M_PI * (((x - (grid.o[0] + L[0] / 2.)) / L[0]) + time));
                    vectorSIN[idx * 3 + 1] = cos(2 * M_PI * (((y - (grid.o[1] + L[1] / 2.)) / L[1]) + time));
                    vectorSIN[idx * 3 + 2] = sin(2 * M_PI * (((z - (grid.o[2] + L[2] / 2.)) / L[2]) + time));
                }
            }
        }

        // save results to disk
        export_spoints_LEGACY("test_spoint", nstep, grid, Mode::TEXT);
        //export_spoints_LEGACY("fdtd_b", nstep, grid, Mode::BINARY);
        //export_spoints_XML("fdtd", nstep, grid, grid, Zip::UNZIPPED);
        //export_spoints_XML("fdtdz", nstep, grid, grid, Zip::ZIPPED);
    }

    return 0;
}
