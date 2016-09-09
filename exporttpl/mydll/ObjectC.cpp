
#include "Singleton.h"
#include "ObjectC.h"
#include "Singleton.inl"
template<> ObjectC *Singleton<ObjectC>::instance = NULL;

template class MYDLL_API Singleton<ObjectC>;




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
