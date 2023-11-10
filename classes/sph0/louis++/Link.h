#ifndef LINK_H
#define LINK_H

#include "sph.h"

/// This class contains a pointer that points toward an object
/// and the distance between 2 particles. This class is used to
/// build vectors of pointers toward objects.

class Link
{
public:
    FixedParticle *ptr = nullptr; ///< pointer toward a particle
    double r = 0.0;           ///< distance between neighbours

    // Link() : ptr(nullptr), r(0.0)
    // {
    // }
};

#endif // LINK_H
