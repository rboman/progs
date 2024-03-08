#ifndef SPH_DISPLAYHOOK_H
#define SPH_DISPLAYHOOK_H

#include "sph.h"

class DisplayHook
{
public:
    DisplayHook();
    virtual ~DisplayHook() = default;
    virtual void display() = 0;
};

#endif //SPH_DISPLAYHOOK_H

