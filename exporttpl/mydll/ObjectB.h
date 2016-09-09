
#ifndef OBJECTB_H
#define OBJECTB_H

#include "mydll.h"
#include "ObjectA.h"

//extern template class MYDLL_API ObjectA<int>;  // VC: C4910 '__declspec(dllexport)' and 'extern' are incompatible on an explicit instantiation / MinGW: inline mult defined 
//extern template class ObjectA<int>;              // VC: C4661 no suitable definition provided for explicit template instantiation request  / MinGW OK - Intel OK

class MYDLL_API ObjectB : public ObjectA<int>
{
public:
    ObjectB(int v);
};

#endif
