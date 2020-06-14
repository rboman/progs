#ifndef OBJECTC_H
#define OBJECTC_H

#include "mydll.h"
#include "Singleton.h"

class MYDLL_API ObjectC : public Singleton<ObjectC>
{
    friend class Singleton<ObjectC>;

public:
    void print();

protected:
    ObjectC();
    ~ObjectC();
};

#endif // OBJECTC_H
