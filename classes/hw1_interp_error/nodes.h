#ifndef HW1_NODES_H
#define HW1_NODES_H

#include "hw1.h"
#include <vector>
#include <map>
#include <cstddef> 

struct Nodes
{
    std::vector<std::size_t> tags;
    std::vector<double> coords;
    std::vector<double> pCoords;

    // additional map
    std::map<size_t, size_t> nodeIdx;
};


void fillNodes(Nodes &nodes);

#endif // HW1_NODES_H