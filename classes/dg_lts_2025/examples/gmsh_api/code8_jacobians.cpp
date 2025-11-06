// This program demonstrates how to convert gmsh vectors to Eigen matrices.
// It also shows:
// - how to select a quadrature type and get the positions and weights of
//   the Gauss points.
// - how to compute the inverse of the Jacobians.
// - how to get the values of the shape functions and their derivatives at
//   the Gauss points.
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code8_jacobians.exe ..\mono.geo

#include <gmsh.h>
#include <iostream>
#include <map>
#include <Eigen/Dense> // <= new header

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

    std::string groupname = "domain"; // <= we choose a physical group here!

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
            // -- CODE ADDED TO PREVIOUS EXAMPLE (begin) -----------------------

            // get element name as a string
            std::string elementName;
            int edim, eorder, enumNodes, enumPrimaryNodes;
            std::vector<double> localNodeCoord;
            gmsh::model::mesh::getElementProperties(elementTypes[i],
                                                    elementName, edim, eorder,
                                                    enumNodes, localNodeCoord,
                                                    enumPrimaryNodes);
            std::cout << '\t' << elementName << " order=" << eorder
                      << " nodes=" << enumNodes << '\n';

            // get Gauss points coordinates and weights
            std::vector<double> localCoords, weights;
            gmsh::model::mesh::getIntegrationPoints(elementTypes[i],
                                                    "CompositeGauss2", // "Gauss2"
                                                    localCoords, weights);
            std::cout << "\t" << weights.size() << " integration points\n";

            std::cout << "\tweights: ";
            for (size_t j = 0; j < weights.size(); ++j)
                std::cout << weights[j] << " ";
            std::cout << '\n';

            std::cout << "\tlocalCoords (gmsh): ";
            for (size_t j = 0; j < localCoords.size(); ++j)
                std::cout << localCoords[j] << " ";
            std::cout << '\n';

            // convert to Eigen format
            Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
                localCoords_E(&localCoords[0], localCoords.size() / 3, 3);
            std::cout << "\tlocalCoords (Eigen):\n";
            Eigen::IOFormat fmt(4, 0, ", ", "\n", "\t\t[", "]");
            std::cout << localCoords_E.format(fmt) << '\n';

            // get Jacobians
            std::vector<double> jacobians, determinants, coords;
            gmsh::model::mesh::getJacobians(elementTypes[i],
                                            localCoords, jacobians,
                                            determinants, coords, tags[k]);
            std::cout << "\tGot " << determinants.size()
                      << " Jacobians for type " << elementTypes[i] << "\n";

            std::cout << "\tjacobians (gmsh): ";
            for (size_t j = 0; j < jacobians.size(); ++j)
                std::cout << jacobians[j] << " ";
            std::cout << '\n';

            // convert first jacobian to Eigen format
            Eigen::Map<Eigen::Matrix3d> jacob(&jacobians[0], 3, 3);
            std::cout << "\tfirst jacobian (Eigen):\n"
                      << jacob.format(fmt) << '\n';

            // compute the inverse with Eigen
            Eigen::Matrix3d jacobinv = jacob.inverse();
            std::cout << "\tinverse of the first jacobian (Eigen):\n"
                      << jacobinv.format(fmt) << "\n";

            // Compute J*Jinv with Eigen "*" operator
            std::cout << "\tVerification (J*Jinv):\n"
                      << (jacob * jacobinv).format(fmt) << '\n';

            // shape functions and derivatives at the Gauss points
            int numComponents, numOrientations;
            std::vector<double> basisFunctions;
            gmsh::model::mesh::getBasisFunctions(elementTypes[i], localCoords,
                                                 "Lagrange", numComponents,
                                                 basisFunctions,
                                                 numOrientations);
            std::cout << "\tGot " << basisFunctions.size()
                      << " basis functions for type "
                      << elementTypes[i] << '\n';

            gmsh::model::mesh::getBasisFunctions(elementTypes[i], localCoords,
                                                 "GradLagrange", numComponents,
                                                 basisFunctions,
                                                 numOrientations);
            std::cout << "\tGot " << basisFunctions.size()
                      << " basis function gradients for type "
                      << elementTypes[i] << '\n';

            // -- CODE ADDED TO PREVIOUS EXAMPLE (end) -------------------------
        }
    }

    gmsh::finalize();
    return 0;
}
