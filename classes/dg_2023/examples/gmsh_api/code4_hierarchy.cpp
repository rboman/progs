// This program loops over the physical groups defined in a .geo file
// and prints their names, dimensions and tags.
//
// This example loops over the gmsh data structure hierarchy:
// - Physical Group
//      => contains 1 or more "Entities"
//          => contains 1 or more "Element types"
//              => access to Elements
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code4_hierarchy.exe ..\rectangle.geo

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

    std::map<std::string, std::pair<int, int>> groups;
    gmsh::vectorpair dimTags;
    gmsh::model::getPhysicalGroups(dimTags);
    for (size_t i = 0; i < dimTags.size(); ++i)
    {
        int dim = dimTags[i].first;
        int tag = dimTags[i].second;
        std::string name;
        gmsh::model::getPhysicalName(dim, tag, name);
        groups[name] = {dim, tag};
    }

    // -- code added to previous code (begin) ----------------------------------

    std::string groupname = "left_edge";

    int dim = groups[groupname].first;
    int tag = groups[groupname].second;
    if (tag == 0)
    {
        std::cerr << "Group '" << groupname << "' does not exist!\n";
        return 1;
    }

    // get entities of the chosen physical group
    std::vector<int> tags;
    gmsh::model::getEntitiesForPhysicalGroup(dim, tag, tags);

    std::cout << "Entities in group named '" << groupname << "': ";
    for (size_t i = 0; i < tags.size(); ++i)
        std::cout << tags[i] << " ";
    std::cout << '\n';

    // loop over entities
    for (size_t k = 0; k < tags.size(); ++k)
    {
        std::cout << "Entity (" << dim << "," << tags[k] << "):\n";
        std::vector<int> elementTypes;
        std::vector<std::vector<std::size_t>> elementTags;
        std::vector<std::vector<std::size_t>> nodeTags;
        gmsh::model::mesh::getElements(elementTypes, elementTags,
                                       nodeTags, dim, tags[k]);

        // loop over element types
        for (size_t i = 0; i < elementTypes.size(); ++i)
        {
            std::cout << "\telement type " << elementTypes[i] << ":\n";
            std::cout << "\t\telements: ";
            for (size_t j = 0; j < elementTags[i].size(); ++j)
                std::cout << elementTags[i][j] << " ";
            std::cout << '\n';
            std::cout << "\t\tnodes: ";
            for (size_t j = 0; j < nodeTags[i].size(); ++j)
                std::cout << nodeTags[i][j] << " ";
            std::cout << '\n';
        }
    }
    // -- code added to previous code (end) ------------------------------------

    gmsh::finalize();
    return 0;
}
