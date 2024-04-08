#ifndef SPH_MOBILEPARTICLE_H
#define SPH_MOBILEPARTICLE_H

#include "sph.h"
#include "sphFixedParticle.h"

namespace sph {

/// This is an extension of the FixedParticle class.
/// The procedure update_vars is overwritten to include the update
/// of u and x.

class SPH_API MobileParticle : public Particle
{
public:
    explicit MobileParticle(double x=0.0, double y=0.0, double z=0.0,
             double vx=0.0, double vy=0.0, double vz=0.0,
             double rho0=0.0, double m0=0.0);

    virtual void update_vars() override;

private:
    double compute_viscosity(Particle *neigh,
                             double alpha, double beta);
};

}; // namespace sph

#endif // SPH_MOBILEPARTICLE_H
