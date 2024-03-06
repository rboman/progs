#ifndef LINK_H
#define LINK_H

#include "sph.h"

/// This class contains a pointer that points toward an object
/// and the distance between 2 particles.
/// This class is used to build vectors of pointers toward objects.

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
