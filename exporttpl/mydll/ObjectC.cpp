
#include "mydll.h"
#include "Singleton.h"
#include "Singleton.inl"

#if defined(__MINGW32__)
class ObjectC;
template class MYDLL_API Singleton<ObjectC>;
#include "ObjectC.h"
#else
#include "ObjectC.h"
template class MYDLL_API Singleton<ObjectC>;
#endif

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
