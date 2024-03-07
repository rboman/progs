#ifndef SPH_NEIGHBOUR_H
#define SPH_NEIGHBOUR_H

#include "sph.h"

/// A particle and a distance to it.

class Neighbour
{
public:
    Particle *ptr;
    double r;      ///< distance between neighbours

    Neighbour(Particle *_ptr, double _r) : ptr(_ptr), r(_r)
    {
    }
};

#endif // SPH_NEIGHBOUR_H
