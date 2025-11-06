// This program takes a .geo file as argument, generates the mesh of the
// surfaces and displays the result in the Gmsh window.
//
// This example introduces:
// - the gmsh.h header
// - initialize()/finalize() functions
// - C++ namespaces / standard library (std::cout)
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code1_loadgeo.exe ..\rectangle.geo

#include <gmsh.h>   // gmsh main header (READ THIS FILE)
#include <iostream> // for std::cout

int main(int argc, char **argv)
{
    // the program requires 1 argument: a .geo file.
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <geo_file>\n";
        return 0;
    }

    gmsh::initialize(); // first thing to do before using any gmsh SDK function

    gmsh::open(argv[1]);            // similar to "File / Open" in the GUI
    gmsh::model::mesh::generate(2); // similar to "Mesh / 2D" in the GUI

    gmsh::fltk::run(); // open gmsh window

    gmsh::finalize(); // clean-up things before ending the program
    return 0;
}
