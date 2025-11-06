#include <gmsh.h>
#include <Eigen/Dense>
#include <iostream>
#include <cassert>
#ifdef _OPENMP
#include <omp.h>
#endif

int main(int argc, char **argv)
{
    std::cout << "using Gmsh " << GMSH_API_VERSION << ".\n";

    std::cout << "using Eigen "
              << EIGEN_WORLD_VERSION << '.'
              << EIGEN_MAJOR_VERSION << '.'
              << EIGEN_MINOR_VERSION << ".\n";

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
    
    // assert(2==3); // make program crash in Debug mode, but not in Release mode

    // read geo filename as first argument
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <model.geo>\n";
        return EXIT_FAILURE;
    }
    std::string geo_filename = argv[1];

    // load the geometry, mesh it and save it to disk.
    gmsh::initialize(argc, argv);
    gmsh::open(geo_filename);
    gmsh::model::mesh::generate(3);
    std::string msh_filename = geo_filename.substr(0, geo_filename.find_last_of('.')) + ".msh";
    gmsh::write(msh_filename);
    gmsh::finalize();

    return EXIT_SUCCESS;
}
