
#include "mydll.h"

#include "Singleton.h"
#include "Singleton.inl"

SINGLETON_CLSATTR(ObjectC)
#include "ObjectC.h"
template class SINGLETON_EXPORT Singleton<ObjectC>;

/*  
// works OK but.... 
#if defined(__MINGW32__)
class ObjectC;
template class MYDLL_API Singleton<ObjectC>;
#include "ObjectC.h"
#else
#include "ObjectC.h"
template class MYDLL_API Singleton<ObjectC>;
#endif
*/

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
