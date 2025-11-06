// This program displays info about "element type 1".
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code5_elprops.exe

#include <gmsh.h>
#include <iostream>
#include <map>

int main(int argc, char **argv)
{
    gmsh::initialize();

    // input parameter
    int elementType = 1;

    // outputs
    std::string elementName;
    int dim, order, numNodes, numPrimaryNodes;
    std::vector<double> localNodeCoord;

    // gmsh call
    gmsh::model::mesh::getElementProperties(elementType,
                                            elementName, dim, order,
                                            numNodes, localNodeCoord,
                                            numPrimaryNodes);

    // display data returned by gmsh
    std::cout << "Element type " << elementType << ":\n";
    std::cout << "\tname = '" << elementName << "'\n";
    std::cout << "\tdim = " << dim << '\n';
    std::cout << "\torder = " << order << '\n';
    std::cout << "\tnumNodes = " << numNodes << '\n';
    std::cout << "\tlocalNodeCoord = ";
    for (size_t i = 0; i < localNodeCoord.size(); ++i)
        std::cout << localNodeCoord[i] << " ";
    std::cout << '\n';
    std::cout << "\tnumPrimaryNodes = " << numPrimaryNodes << '\n';

    gmsh::finalize();
    return 0;
}
