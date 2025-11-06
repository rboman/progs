// This example generates some results over a mesh.
//
// How to build/run? (windows)
//   [in examples/gmsh_api]
//   mkdir build
//   cd build
//   cmake .. && make && code9_views.exe ..\rectangle.geo

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
    gmsh::model::mesh::generate(2); // generate 2D elements
    gmsh::model::mesh::setOrder(3); // generate high-order mesh

    std::vector<std::string> names;
    gmsh::model::list(names);
    std::cout << "the file contains " << names.size() << " model names\n";

    // get all the nodes
    std::vector<std::size_t> nodeTags;
    std::vector<double> coords;
    std::vector<double> parametricCoord;
    gmsh::model::mesh::getNodes(nodeTags, coords, parametricCoord);

    std::cout << "the mesh has " << nodeTags.size() << " nodes\n";

    // get all the elements for dim=2
    std::vector<int> elementTypes;
    std::vector<std::vector<std::size_t>> elementTags;
    std::vector<std::vector<std::size_t>> elnodeTags;
    gmsh::model::mesh::getElements(elementTypes, elementTags, elnodeTags, 2);

    // compute total number of 2D elements
    int nbelems = 0;
    for (std::size_t i = 0; i < elementTags.size(); ++i)
        nbelems += elementTags[i].size();
    std::cout << "the mesh has " << nbelems << " elements\n";

    // Create a new post-processing view
    int viewtag1 = gmsh::view::add("NodeData");
    int viewtag2 = gmsh::view::add("ElementNodeData");

    std::size_t nstep = 20;
    double Lx = 20.;
    double v = 10.0;
    double tend = Lx / v;
    for (std::size_t step = 0; step < nstep; step++)
    {
        double time = tend * ((double)step / nstep);

        // ** NodeData
        // loop over the nodes and compute some values from their coordinates
        std::vector<double> data1(nodeTags.size());
        for (std::size_t i = 0; i < nodeTags.size(); ++i)
        {
            double x = coords[i * 3 + 0];
            double y = coords[i * 3 + 1];

            double val = sin(2 * M_PI * (x - v * time) / Lx * 2) + y;
            data1[i] = val;
        }
        gmsh::view::addHomogeneousModelData(viewtag1, step, names[0], "NodeData",
                                            nodeTags, data1, time);

        // ** ElementNodeData
        // loop over the elements and compute some values at the coords of its nodes

        std::vector<std::vector<double>> data2(nbelems);
        std::vector<std::size_t> elems2D(nbelems);

        // for each element type
        // std::cout << "there are " << elementTypes.size() << " element type(s)\n";
        int k = 0;
        for (std::size_t ityp = 0; ityp < elementTypes.size(); ++ityp)
        {
            // get info about this type of element
            std::string name;
            int dim, order, numNodes, numPrimaryNodes;
            std::vector<double> paramCoord;
            gmsh::model::mesh::getElementProperties(elementTypes[ityp], name, dim, order,
                                                    numNodes, paramCoord, numPrimaryNodes);
            // std::cout << "element type has " << numNodes << " nodes\n";

            // select element/node tags for this type of element
            auto &etags = elementTags[ityp];
            auto &enods = elnodeTags[ityp];

            // loop over elements of type "ityp"
            // std::cout << "loop over the " << etags.size() << " elements\n";
            for (std::size_t i = 0; i < etags.size(); ++i)
            {
                elems2D[k] = etags[i];

                double offset = (elems2D[k] % 2) * 0.5; // so that fields are discontinious across elements

                data2[k].resize(numNodes);
                // loop over nodes of the element
                for (int j = 0; j < numNodes; ++j)
                {
                    int nodeTag = enods[numNodes * i + j];

                    std::vector<double> nodecoord;
                    std::vector<double> nodepcoord;
                    int dim, tag;
                    gmsh::model::mesh::getNode(nodeTag, nodecoord, nodepcoord, dim, tag);
                    double x = nodecoord[0];
                    double y = nodecoord[1];

                    double val = sin(2 * M_PI * (x - v * time) / Lx * 2) + y;
                    data2[k][j] = val + offset;
                }
                k++;
            }
        }
        gmsh::view::addModelData(viewtag2, step, names[0], "ElementNodeData",
                                 elems2D, data2, time, 1); // the last ,1 is important!
    }

    // save the results to disk
    // ...write view 1 to results.msh
    gmsh::view::write(viewtag1, "results.msh");
    // ...write view 2 to results.msh
    gmsh::option::setNumber("PostProcessing.SaveMesh", 0); // we don't want to write the mesh twice
    gmsh::option::setNumber("PostProcessing.SaveInterpolationMatrices", 0);
    gmsh::view::write(viewtag2, "results.msh", true);  // ,true means "append"

    // set display options & view results
    gmsh::option::setNumber("View[0].IntervalsType", 3); 
    // improve display of high-order elements...
    gmsh::option::setNumber("View[0].AdaptVisualizationGrid", 1);
    gmsh::option::setNumber("View[0].MaxRecursionLevel", 3);
    gmsh::option::setNumber("View[0].TargetError", -0.001);

    gmsh::option::setNumber("View[1].IntervalsType", 3);
    // improve display of high-order elements...
    gmsh::option::setNumber("View[1].AdaptVisualizationGrid", 1);
    gmsh::option::setNumber("View[1].MaxRecursionLevel", 3);
    gmsh::option::setNumber("View[1].TargetError", -0.001);

    gmsh::fltk::run(); // open gmsh window

    gmsh::finalize();
    return 0;
}
