#ifndef LINK_H
#define LINK_H

#include "sph.h"

/// A particle and a distance to it.

class Link
{
public:
    FixedParticle *ptr = nullptr; ///< pointer towards a particle
    double r = 0.0;               ///< distance between neighbours

    Link(FixedParticle *_ptr, double _r) : ptr(_ptr), r(_r)
    {
    }
};

#endif // LINK_H
