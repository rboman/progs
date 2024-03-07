#ifndef SPH_FIXEDPARTICLE_H
#define SPH_FIXEDPARTICLE_H

#include "sph.h"
#include "Particle.h"

class FixedParticle : public Particle

{
public:
    FixedParticle(Model &m);

    virtual void update_vars();
};

#endif // SPH_FIXEDPARTICLE_H
