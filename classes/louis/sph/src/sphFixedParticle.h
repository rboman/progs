#ifndef SPH_FIXEDPARTICLE_H
#define SPH_FIXEDPARTICLE_H

#include "sph.h"
#include "sphParticle.h"

namespace sph {

class FixedParticle : public Particle
{
public:
    explicit FixedParticle(Model &model, double x=0.0, double y=0.0, double z=0.0,
             double vx=0.0, double vy=0.0, double vz=0.0,
             double rho0=0.0, double m0=0.0);

    virtual void update_vars() override;
};

}; // namespace sph

#endif // SPH_FIXEDPARTICLE_H
