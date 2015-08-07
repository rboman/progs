
#ifndef OBJECTB_H
#define OBJECTB_H

#include "mydll.h"
#include "ObjectA.h"

class MYDLL_API ObjectB : public ObjectA<int>
{
public:
    ObjectB(int v);
};

#endif
