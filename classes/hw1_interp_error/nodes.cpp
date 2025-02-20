#include "nodes.h"
#include <gmsh.h>
#include <iostream>

void fillNodes(Nodes &nodes)
{
    gmsh::model::mesh::getNodes(nodes.tags, nodes.coords, nodes.pCoords);
    if (verbosity > VERB::INFO)
        std::cout << "the mesh has " << nodes.tags.size() << " nodes\n";

    // build a map (node_tag => node_index)
    for (size_t i = 0; i < nodes.tags.size(); ++i)
        nodes.nodeIdx[nodes.tags[i]] = i;
}
