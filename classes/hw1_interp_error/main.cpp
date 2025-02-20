// linux:
//  cmake .. && make -j 10 && ./hw1 ../mixed.geo
//  cmake -DCMAKE_BUILD_TYPE=Release .. && make -j 10 && ./hw1 ../mixed.geo -order 2
// windows:
//  cmake .. && make -j 10 && hw1.exe ..\mixed.geo
//  cmake .. && make -j 10 && hw1.exe ..\mixed.geo -nopopup
//  cmake .. && make -j 10 && hw1.exe ..\mixed.geo -setnumber factor 10
//  cmake .. && make -j 10 && hw1.exe ..\mixed.geo -setnumber factor 1 -order 4 -v 0 -nopopup
//
// release build:
//  cmake -DCMAKE_BUILD_TYPE=Release .. && make -j 10

#include "hw1.h"
#include "groups.h"
#include "nodes.h"
#include "entities.h"
#include "view.h"
#include "eigenmaps.h"
#include "l2error.h"
#include "func.h"
#include "interpolate.h"
#include "elements.h"
#include <gmsh.h>
#include <iostream>
#include <map>
#include <set>
#include <cassert>
#include <tuple>
#include <Eigen/Dense>
#include <iomanip>
#include <chrono>
#ifdef _OPENMP
#include <omp.h>
#endif

/// level of verbosity (retrieved from Gmsh - use -v [0-99] from the command line)
int verbosity = 0;

// #ifdef __unix__
// #define _GNU_SOURCE // gives us feenableexcept on older gcc's
// #define __USE_GNU   // gives us feenableexcept on newer gcc's
// #include <fenv.h>
// #else
// #include <float.h>
// #ifndef _EM_OVERFLOW
// #define _EM_OVERFLOW EM_OVERFLOW
// #endif
// #endif

int doWork(int argc, char **argv)
{
    const std::vector<std::string_view> args(argv + 1, argv + argc);

    // #ifdef __unix__
    //     feenableexcept(FE_OVERFLOW);
    // #else
    // TODO: study how to catch fpe...
    //       this line seems to work with MinGW but fpe is not correctly caught
    //       while debugging in vscode
    //     _controlfp(0, _EM_INVALID|_EM_DENORMAL|_EM_UNDERFLOW); // underflow catch NaN!
    // #endif

    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <geo_file> [gmsh options]\n";
        return 0;
    }

    gmsh::initialize(argc, argv);

    // set global verbosity level to the one of gmsh
    double dverb;
    gmsh::option::getNumber("General.Verbosity", dverb);
    verbosity = static_cast<int>(dverb);
    // std::cout << "General.Verbosity = " << verbosity << '\n';

#ifdef _OPENMP
    if (verbosity > VERB::INFO)
        std::cout << "number of threads = " << omp_get_max_threads() << '\n';
#endif

    auto cputime0 = std::chrono::high_resolution_clock::now();

    // load geometry and generate mesh
    gmsh::open(argv[1]);
    gmsh::model::mesh::generate();

    // get function string from the onelab database
    std::vector<std::string> fcts;
    gmsh::onelab::getString("function", fcts);
    if (fcts.size() == 0)
    {
        std::cerr << "function missing in geo file!\n";
        exit(EXIT_FAILURE);
    }
    std::string fct = fcts[0];
    if (verbosity > VERB::INFO)
        std::cout << "function = \"" << fct << "\"\n";

    // retreive physical groups
    PhysicalGroups groups;
    fillGroups(groups);

    // get all the nodes
    Nodes nodes;
    fillNodes(nodes);

    // select the physical group to be processed by its name (domain)
    std::string groupname = "domain";
    std::vector<Entity> entities;
    std::map<int, ElemPrp> prps;
    fillEntities(groups, groupname, entities, prps);

    // allocate list of all elements
    size_t nbelems = countNbOfElements(entities);
    std::vector<std::size_t> elems2D(nbelems);                         ///< list of all elements of the phygroup to be processed
    std::vector<std::tuple<size_t, size_t, size_t>> elemLOCs(nbelems); ///< (entity_index, mesh_index, el_index) for element #i

    // allocate mesh data
    ElementData eldata;
    fillElementData(eldata, entities, prps, elems2D, elemLOCs);

    // values of f at the nodes (values per node, per element)
    std::vector<std::vector<double>> nodalFi(nbelems);
    interpolate(nodes, entities, prps, elems2D, nodalFi, fct);

    std::vector<std::vector<double>> nodalFa(nbelems);
    approximate(entities, prps, elems2D, elemLOCs,
                eldata.determinants, eldata.gpcoords,
                nodalFa, fct);

    double Etot_i = l2error(entities, prps, elems2D, elemLOCs,
                            eldata.determinants, eldata.gpcoords,
                            nodalFi, fct);

    double Etot_a = l2error(entities, prps, elems2D, elemLOCs,
                            eldata.determinants, eldata.gpcoords,
                            nodalFa, fct);
    // exit(1);
    // compute total area (used for the convergence study)
    double Atot = totalArea(entities, prps, elems2D, elemLOCs,
                            eldata.determinants);

    auto cputime1 = std::chrono::high_resolution_clock::now();

    // output of the code
    if (verbosity > VERB::FATAL_ERROR)
    {
        std::cout << "Nelems = " << elems2D.size() << '\n';
        std::cout << "Atot = " << Atot << '\n';
        std::cout << "Amean = " << Atot / elems2D.size() << '\n';
        std::cout << "Etot_i = " << std::setprecision(16) << Etot_i << std::setprecision(6) << '\n';
        std::cout << "Etot_a = " << std::setprecision(16) << Etot_a << std::setprecision(6) << '\n';
        std::cout << "CPU = " << std::chrono::duration_cast<std::chrono::duration<double>>(cputime1 - cputime0).count() << '\n';
    }

    if (std::find(args.begin(), args.end(), "-nopopup") == args.end())
    {
        view(elems2D, nodalFi, "interpolation");
        view(elems2D, nodalFa, "approximation");
        gmsh::fltk::run(); // open gmsh window
    }

    gmsh::finalize();
    return 0;
}

int main(int argc, char **argv)
{
    try
    {
        return doWork(argc, argv);
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << std::endl;
    }
    catch (...)
    {
        std::cerr << "unknown exception" << std::endl;
    }
    return EXIT_FAILURE;
}
