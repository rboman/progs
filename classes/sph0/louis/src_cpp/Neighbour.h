#ifndef SPH_NEIGHBOUR_H
#define SPH_NEIGHBOUR_H

#include "sph.h"

/// A particle and a distance to it.

class Neighbour
{
public:
    Particle *p;
    double r;      ///< distance

    Neighbour(Particle *_p, double _r) : p(_p), r(_r)
    {
    }
};

#endif // SPH_NEIGHBOUR_H
