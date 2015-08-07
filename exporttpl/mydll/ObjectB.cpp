
#include "ObjectB.h"
#include <iostream>

ObjectB::ObjectB(int v) : ObjectA<int>(v)
{
    std::cout << "ObjectB::ObjectB()\n";
}

