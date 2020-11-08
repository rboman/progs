// example of sequential program

#include "vtl_spoints.h"
#include "vtlSPoints.h"
#include "json.h"
#include <iostream>

bool same_files(std::string const &lFilePath, std::string const &rFilePath)
{
    const int BUFFER_SIZE = 1024 * 1024;

    std::ifstream lFile(lFilePath.c_str(), std::ifstream::in | std::ifstream::binary);
    std::ifstream rFile(rFilePath.c_str(), std::ifstream::in | std::ifstream::binary);

    if (!lFile.is_open() || !rFile.is_open())
    {
        std::cout << "unable to open files\n";
        return false;
    }

    char *lBuffer = new char[BUFFER_SIZE];
    char *rBuffer = new char[BUFFER_SIZE];

    do
    {
        lFile.read(lBuffer, BUFFER_SIZE);
        rFile.read(rBuffer, BUFFER_SIZE);
        auto lread = lFile.gcount();
        auto rread = rFile.gcount();

        if (lread != rread || std::memcmp(lBuffer, rBuffer, lread) != 0)
        {
            delete[] lBuffer;
            delete[] rBuffer;
            return false;
        }
    } while (lFile.good() || rFile.good());

    delete[] lBuffer;
    delete[] rBuffer;
    return true;
}

int main(int argc, char *argv[])
{
/*
    std::string file1="A.vtk";
    std::string file2="B.vtk";
    
    if(same_files(file1, file2))
        std::cout << "files are the same\n";
    else
        std::cout << "files are different\n";
    return 1;
*/
    rapidjson::Document d;
    // read parameters
    if (argc > 1)
        read_json(argv[1], d);

    // Global grid parameters

    SPoints grid;
    grid.o = read_Vec3d(d, "grid.o", Vec3d(10.0, 10.0, 10.0));  // origin
    Vec3d L = read_Vec3d(d, "grid.L", Vec3d(50.0, 60.0, 80.0)); // box dimensions
    grid.np1 = read_Vec3i(d, "grid.np1", Vec3i(0, 0, 0));       // first index
    grid.np2 = read_Vec3i(d, "grid.np2", Vec3i(25, 30, 40));    // last index

    grid.dx = L / (grid.np() - 1); // compute spacing

    int nstepT = read_int(d, "nstepT", 1); // nb of time steps

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

    // creation of dummy CELL fields
    int nbc = grid.nbc();
    std::vector<double> cscalarX(nbc); // scalar field at cells
    grid.cscalars["cscalar_X"] = &cscalarX;
    std::vector<double> cvectorSIN(nbc * 3); // vector field at cells
    grid.cvectors["cvector_SIN"] = &cvectorSIN;

    std::cout << grid << std::endl;

    // time step loop
    for (int nstep = 0; nstep < nstepT; ++nstep)
    {
        double time = double(nstep) / nstepT;

        //int idx = 0;
        int npz1 = grid.np1[2];
        int npz2 = grid.np2[2];
        Vec3i np = grid.np();
        Vec3i nc = grid.nc();

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

                    // fill point values
                    int idx = (k - npz1) * (np[1] * np[0]) + (j - npy1) * np[0] + (i - npx1);

                    scalarX[idx] = x;
                    scalarY[idx] = y;
                    scalarZ[idx] = z;
                    vectorSIN[idx * 3 + 0] = sin(2 * M_PI * (((x - (grid.o[0] + L[0] / 2.)) / L[0]) + time));
                    vectorSIN[idx * 3 + 1] = cos(2 * M_PI * (((y - (grid.o[1] + L[1] / 2.)) / L[1]) + time));
                    vectorSIN[idx * 3 + 2] = sin(2 * M_PI * (((z - (grid.o[2] + L[2] / 2.)) / L[2]) + time));

                    // fill cell values
                    if (i != npx2 && j != npy2 && k != npz2)
                    {
                        int idc = (k - npz1) * (nc[1] * nc[0]) + (j - npy1) * nc[0] + (i - npx1);

                        cscalarX[idc] = x;
                        cvectorSIN[idc * 3 + 0] = sin(2 * M_PI * (((x - (grid.o[0] + L[0] / 2.)) / L[0]) + time));
                        cvectorSIN[idc * 3 + 1] = cos(2 * M_PI * (((y - (grid.o[1] + L[1] / 2.)) / L[1]) + time));
                        cvectorSIN[idc * 3 + 2] = sin(2 * M_PI * (((z - (grid.o[2] + L[2] / 2.)) / L[2]) + time));
                    }
                }
            }
        }

        std::string prefix = read_string(d, "output.prefix", "fdtd");
        // save results to disk
        if (read_bool(d, "write.legacy.text", true))
            export_spoints_LEGACY(prefix + "_t", nstep, grid, Mode::TEXT);
        if (read_bool(d, "write.legacy.bin", true))
            export_spoints_LEGACY(prefix + "_b", nstep, grid, Mode::BINARY);
        if (read_bool(d, "write.xml.bin", true))
            export_spoints_XML(prefix, nstep, grid, grid, Zip::UNZIPPED);
        if (read_bool(d, "write.xml.binz", true))
            export_spoints_XML(prefix + "z", nstep, grid, grid, Zip::ZIPPED);
    }

    return 0;
}
