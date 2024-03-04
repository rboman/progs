#ifndef MOBILEPARTICLE_H
#define MOBILEPARTICLE_H

#include "sph.h"
#include "FixedParticle.h"

/// This is an extension of the FixedParticle class.
/// The procedure varUpdate is overwritten to include the update 
//  of u and x.

class MobileParticle : public FixedParticle
{
public:
    MobileParticle() : FixedParticle() {}

    virtual void varUpdate();
    double ArtificialViscosity(FixedParticle *neighObj,
                               double alpha, double beta);
};

#endif // MOBILEPARTICLE_H
