#ifndef SPH_DISPLAYHOOK_H
#define SPH_DISPLAYHOOK_H

#include "sph.h"

class DisplayHook
{
public:
    DisplayHook() = default;
    virtual ~DisplayHook() = default;
    virtual void interact() = 0;
    virtual void update_data() = 0;
    virtual void loop() = 0;
};

#endif //SPH_DISPLAYHOOK_H

