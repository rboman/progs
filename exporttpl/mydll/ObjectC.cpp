
#include "Singleton.h"
#include "ObjectC.h"

template class MYDLL_API Singleton<ObjectC>;

#include "Singleton.inl"

template<> ObjectC *Singleton<ObjectC>::instance = NULL;


ObjectC::ObjectC()
{
}

ObjectC::~ObjectC()
{
}

void
ObjectC::print()
{
    std::cout << "ObjectC::print()\n";
}
