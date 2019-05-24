#ifndef NODE_H
#define NODE_H

#include <iostream>

class Unknowns
{
  public:
    double T;
    double u;

  public:
    Unknowns() : T(0.0), u(0.0) {}
};

class Node
{
  public:
    int no;
    double x;
    Unknowns unks;

  public:
    Node(int _no, double _x) : no(_no), x(_x) {}
    friend std::ostream &operator<<(std::ostream &out, Node const &obj);
};

#endif
