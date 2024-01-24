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

    return EXIT_SUCCESS;
}
