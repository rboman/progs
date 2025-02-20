#ifndef HW1_ENTITIES_H
#define HW1_ENTITIES_H

#include "hw1.h"
#include <vector>
#include <string>
#include <map>

struct GroupEntities
{
    std::vector<int> tags;
};

struct Entity
{
    int tag;
    std::vector<int> elementTypes;
    std::vector<std::vector<std::size_t>> elementTags;
    std::vector<std::vector<std::size_t>> nodeTags;
};

struct ElemPrp
{
    int type;
    // properties
    std::string name;
    int dim, order, numNodes, numPrimaryNodes;
    std::vector<double> paramCoord;

    // integ rule
    std::vector<double> localCoords, weights;
    // basis functions and derivatives at GPs
    std::vector<double> basisF;
    // std::vector<double> basisDF;
};


void fillEntities(PhysicalGroups const &groups,
                 std::string const &groupname,
                 std::vector<Entity> &entities,
                 std::map<int, ElemPrp> &prps);


#endif // HW1_ENTITIES_H
