#ifndef FIXEDPARTICLE_H
#define FIXEDPARTICLE_H

#include "sph.h"
#include "List.h"

/// This class contains a certain number of parameters describing
/// the state of a fixed particle (boundary particle). It also
/// includes the needed procedures to calculate the continuity
/// and some other equations.

class FixedParticle
{
public:
    double coord[3][3];           ///< 3x3 array containing the coordinates of a particle.
                                  ///      column 1 = currentTime
                                  ///      column 2 = RKstep
                                  ///      column 3 = nextTime
    double speed[3][3];           ///< 3x3 array containing the velocity of a particle.
                                  ///      column 1 = currentTime
                                  ///      column 2 = RKstep
                                  ///      column 3 = nextTime
    double rho[3];                ///< 3x1 array containing the density of a particle.
                                  ///      element 1 = currentTime
                                  ///      element 2 = RKstep
                                  ///      element 3 = nextTime
    double m;                     ///< mass of the particle
    double p[3];                  ///< pressure of the particle
                                  ///      element 1 = currentTime
                                  ///      element 2 = RKstep
                                  ///      element 3 = nextTime
    double c[3];                  ///< speed of sound of a particle
                                  ///      element 1 = currentTime
                                  ///      element 2 = RKstep
                                  ///      element 3 = nextTime
    double h;                     ///< smoothing length
    List neighbours;              ///< list of neighbours
    int numOfNeighbours;          ///< number of neighbours
    double vec_gradW[150][3];     ///< array that contains the gradient for every
                                  /// neighbours; initially set to 150 elements to
                                  /// increase the computational efficiency
    double vec_gradW_mod[150][3]; ///< corrected vec_gradW if asked
    ParticleManager *manager;     ///< pointer toward the object particle_manager
    double max_mu_ab;             ///< maximum mu_ab of a particle (used for the timestep calculation)

public:
    FixedParticle() {}

    void save2disk() {}
    void loadfromdisk() {}
    void getNeighbours() {}
    void calcPressure() {}
    void calcCelerity() {}
    void gradW() {}
    void kernel_corr() {}

    virtual void varUpdate() {}
};

#endif // FIXEDPARTICLE_H
