// This program loops over the physical groups defined in a .geo file
// and prints their names, dimensions and tags.
//
// This example introduces:
// - (dim,tags)
// - std::pair<T1,T2>
// - std::vector<T>
// - gmsh::vectorpair == std::vector<std::pair<int,int>>
// - references / const references
// - how to call a gmsh function (input vs output arguments)
// - std::string
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code2_getgroups.exe ..\rectangle.geo

#include <gmsh.h>
#include <iostream>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <geo_file>\n";
        return 0;
    }

    gmsh::initialize();

    gmsh::open(argv[1]);
    gmsh::model::mesh::generate(2);

    // -- code added to code1.cpp (begin) --------------------------------------

    gmsh::vectorpair dimTags; // output parameter of "getPhysicalGroups"
    gmsh::model::getPhysicalGroups(dimTags);

    for (size_t i = 0; i < dimTags.size(); ++i)
    {
        int dim = dimTags[i].first; // dimTags[i] is a std::pair<int,int>
        int tag = dimTags[i].second;

        std::string name; // output parameter of "getPhysicalName"
        gmsh::model::getPhysicalName(dim, tag, name);

        // prints the data to the console
        std::cout << "Physical group (" << dim << "D) "
                  << "named '" << name << "' tag=" << tag << '\n';
    }

    // -- code added to code1.cpp (end) ----------------------------------------

    gmsh::finalize();
    return 0;
}
