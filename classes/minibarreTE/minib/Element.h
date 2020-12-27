#ifndef ELEMENT_H
#define ELEMENT_H

#include <iostream>
#include "Node.h"

class Element
{
public:
    int no;
    Node *node1;
    Node *node2;

public:
    Element(int _no, Node *_node1, Node *_node2)
        : no(_no), node1(_node1), node2(_node2)
    {
    }
    friend std::ostream &operator<<(std::ostream &out, Element const &obj);
};

#endif
