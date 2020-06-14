
#ifndef OBJECTA_H
#define OBJECTA_H

#include "mydll.h"

template <typename T> class ObjectA
{
    T val;

public:
    ObjectA(T v);
    void print();
};

#include "ObjectA.inl"

#endif
