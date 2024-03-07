#ifndef FIXEDPARTICLE_H
#define FIXEDPARTICLE_H

#include "sph.h"
#include "Particle.h"

class FixedParticle : public Particle

{
public:
    FixedParticle(ParticleManager &m);

    virtual void update_vars();
};

#endif // FIXEDPARTICLE_H
