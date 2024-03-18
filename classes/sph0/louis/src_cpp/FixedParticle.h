#ifndef SPH_FIXEDPARTICLE_H
#define SPH_FIXEDPARTICLE_H

#include "sph.h"
#include "Particle.h"

class FixedParticle : public Particle

{
public:
    explicit FixedParticle(Model &m);

    virtual void update_vars() override;
};

#endif // SPH_FIXEDPARTICLE_H
