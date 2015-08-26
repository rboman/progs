
#include "ObjectB.h"
#include <iostream>

//extern template class ObjectA<int>;

ObjectB::ObjectB(int v) : ObjectA<int>(v)
{
    std::cout << "ObjectB::ObjectB()\n";
}

