#ifndef MOBILEPARTICLE_H
#define MOBILEPARTICLE_H

#include "sph.h"
#include "FixedParticle.h"

/// This is an extension of the fixed_particle class.
/// The procedure varUpdate is overwritten to include the update of u and x.

class MobileParticle : public FixedParticle
{
public:
    MobileParticle() {}

    virtual void varUpdate() {}
    void ArtificialViscosity() {}
};

#endif // MOBILEPARTICLE_H
