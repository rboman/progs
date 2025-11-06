// This program loops over the physical groups defined in a .geo file
// and prints their names, dimensions and tags.
//
// This example introduces:
// - std::map
// - Physical Group
//      => Entities
//          => Element types
//              => Elements/nodes hierarchy
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code3_buildphymap.exe ..\rectangle.geo

#include <gmsh.h>
#include <iostream>
#include <map>

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

    // -- code added to previous code (begin) ----------------------------------
    // definition of the map: "group_name" => (dim,tag)
    std::map<std::string, std::pair<int, int>> groups;
    // -- code added to previous code (end) ------------------------------------

    gmsh::vectorpair dimTags;
    gmsh::model::getPhysicalGroups(dimTags);

    for (size_t i = 0; i < dimTags.size(); ++i)
    {
        int dim = dimTags[i].first;
        int tag = dimTags[i].second;

        std::string name;
        gmsh::model::getPhysicalName(dim, tag, name);

        std::cout << "Physical group (" << dim << "D) "
                  << "named '" << name << "' tag=" << tag << '\n';

        // -- code added to previous code (begin) ------------------------------
        // set value (dim, tag) for key 'name'
        groups[name] = {dim, tag};
        // -- code added to previous code (end) --------------------------------
    }

    // -- code added to previous code (begin) ----------------------------------
    // print map
    std::cout << "Physical Groups:\n";
    for (auto &x : groups)
        std::cout << "\t '" << x.first << "' => (" << x.second.first << ','
                  << x.second.second << ")\n";
    // -- code added to previous code (end) ------------------------------------

    gmsh::finalize();
    return 0;
}
