#ifndef SPH_MOBILEPARTICLE_H
#define SPH_MOBILEPARTICLE_H

#include "sph.h"
#include "FixedParticle.h"

/// This is an extension of the FixedParticle class.
/// The procedure update_vars is overwritten to include the update
/// of u and x.

class MobileParticle : public FixedParticle
{
public:
    explicit MobileParticle(Model &m);

    virtual void update_vars() override;

private:
    double compute_viscosity(Particle *neigh,
                             double alpha, double beta);
};

#endif // SPH_MOBILEPARTICLE_H
