#ifndef SPH_NEIGHBOUR_H
#define SPH_NEIGHBOUR_H

#include "sph.h"

namespace sph {

/// A particle and a distance to it.

class SPH_API Neighbour
{
public:
    Particle *p;
    double r;      ///< distance

    Neighbour(Particle *_p, double _r) : p(_p), r(_r)
    {
    }
};

}; // namespace sph

#endif // SPH_NEIGHBOUR_H
