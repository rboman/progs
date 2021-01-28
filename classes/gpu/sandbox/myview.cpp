// This example generate some results over a mesh
//
// How to use this example?
//
// 1.build the code:
//     cd build
//     cmake ..
//     make
// 2.generate a mesh from a geo file:
//     gmsh -2 -order 3 ..\sandbox\sea.geo
// 3.run the program with the msh as argument
//     bin\myview.exe ..\sandbox\sea.msh
// 4.display the generated msh data
//     gmsh data.msh

#ifdef _MSC_VER
#include <gmsh.h_cwrap>
#else
#include <gmsh.h>
#endif
#include <iostream>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " file.msh [options]" << std::endl;
        return 0;
    }
    gmsh::initialize(argc, argv);
    gmsh::option::setNumber("General.Terminal", 1); // enables "gmsh::logger::write(...)"
    gmsh::open(argv[1]);                            // reads the msh file

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
    int nbelems=0;
    for(std::size_t i=0; i< elementTags.size(); ++i)
        nbelems+=elementTags[i].size();
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
        std::vector<std::vector<double>> data1(nodeTags.size()); // check this size!!
        for (std::size_t i = 0; i < nodeTags.size(); ++i)
        {
            std::size_t tag = nodeTags[i];
            double x = coords[i * 3 + 0];
            double y = coords[i * 3 + 1];
            double z = coords[i * 3 + 2];

            double val = sin(2 * M_PI * (x - v * time) / Lx * 2) + y;
            data1[i].resize(1);
            data1[i][0] = val;
        }
        gmsh::view::addModelData(viewtag1, step, names[0], "NodeData",
                                 nodeTags, data1, time);

        // ** ElementNodeData
        // loop over the elements and compute some values at the coords of its nodes

        std::vector<std::vector<double>> data2(nbelems);
        std::vector<std::size_t> elems2D(nbelems);

        // for each element type
        //std::cout << "there are " << elementTypes.size() << " element type(s)\n";
        int k=0;
        for (std::size_t ityp = 0; ityp < elementTypes.size(); ++ityp)
        {
            // get info about this type of element
            std::string name;
            int dim, order, numNodes, numPrimaryNodes;
            std::vector<double> paramCoord;
            gmsh::model::mesh::getElementProperties(elementTypes[ityp], name, dim, order,
                                                    numNodes, paramCoord, numPrimaryNodes);
            //std::cout << "element type has " << numNodes << " nodes\n";

            // select element/node tags for this type of element
            auto &etags = elementTags[ityp];
            auto &enods = elnodeTags[ityp];

            // loop over elements of type "ityp"
            //std::cout << "loop over the " << etags.size() << " elements\n";
            for (std::size_t i = 0; i < etags.size(); ++i)
            {
                elems2D[k] = etags[i];

                double offset = (elems2D[k]%2) *0.5; // so that fields are discontinious across elements

                data2[k].resize(numNodes);
                // loop over nodes of the element
                for (std::size_t j = 0; j < numNodes; ++j)
                {
                    int tag = enods[numNodes * i + j];

                    std::vector<double> nodecoord;
                    std::vector<double> nodepcoord;
                    gmsh::model::mesh::getNode(tag, nodecoord, nodepcoord);
                    double x = nodecoord[0];
                    double y = nodecoord[1];
                    double z = nodecoord[2];
/*
                    double x = coords[nod * 3 + 0];
                    double y = coords[nod * 3 + 1];
                    double z = coords[nod * 3 + 2];
*/
                    double val = sin(2 * M_PI * (x - v * time) / Lx * 2) + y;
                    data2[k][j] = val + offset;
                }
                k++;
            }
        }
          gmsh::view::addModelData(viewtag2, step, names[0], "ElementNodeData",
                                 elems2D, data2, time, 1); // the last ,1 is important!
      
    }

    gmsh::view::write(viewtag1, "data.msh");
    gmsh::view::write(viewtag2, "data.msh", true);

    gmsh::option::setNumber("View[0].IntervalsType", 3);
    gmsh::option::setNumber("View[0].AdaptVisualizationGrid", 1);
    gmsh::option::setNumber("View[0].MaxRecursionLevel", 3);
    gmsh::option::setNumber("View[0].TargetError", -0.0001);
    
    gmsh::option::setNumber("View[1].IntervalsType", 3);
    gmsh::option::setNumber("View[1].AdaptVisualizationGrid", 1);
    gmsh::option::setNumber("View[1].MaxRecursionLevel", 3);
    gmsh::option::setNumber("View[1].TargetError", -0.0001);

    gmsh::fltk::run();

    gmsh::finalize();
    return 0;
}
