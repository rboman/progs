
#ifndef MESH_H
#define MESH_H

#include <iostream>
#include <vector>
#include "Node.h"
#include "Element.h"

class Mesh
{
public:
    std::vector<Node*>    nodes;
    std::vector<Element*> elems;
public:
    Mesh() {}
    friend std::ostream &operator<<(std::ostream &out, Mesh const &obj);
    void generate(double xmin, double xmax, int nelm);
};

#endif

