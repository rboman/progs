#ifndef SPH_FIXEDPARTICLE_H
#define SPH_FIXEDPARTICLE_H

#include "sph.h"
#include "sphParticle.h"

namespace sph {

class FixedParticle : public Particle
{
public:
    explicit FixedParticle(Model &m);

    virtual void update_vars() override;
};

}; // namespace sph

#endif // SPH_FIXEDPARTICLE_H
