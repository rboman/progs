#ifndef FIXEDPARTICLE_H
#define FIXEDPARTICLE_H

#include "sph.h"
#include "Link.h"

/// This class contains a certain number of parameters describing
/// the state of a fixed particle (boundary particle). It also
/// includes the needed procedures to calculate the continuity
/// and some other equations.

class FixedParticle
{
public:
    Eigen::Vector3d coord[3];           ///< 3x1 array containing the coordinates of a particle.
                                        ///      element 0 = currentTime
                                        ///      element 1 = RKstep
                                        ///      element 2 = nextTime
    Eigen::Vector3d speed[3];           ///< 3x1 array containing the velocity of a particle.
    double rho[3];                      ///< 3x1 array containing the density of a particle.
    double m;                           ///< mass of the particle
    double p[3];                        ///< 3x1 array containing the pressure of a particle.
    double c[3];                        ///< 3x1 array containing the speed of sound of a particle.
    double h;                           ///< smoothing length
    std::vector<Link> neighbours;       ///< list of neighbours
    Eigen::Vector3d vec_gradW[150];     ///< array that contains the gradient for every
                                        ///  neighbours; initially set to 150 elements to
                                        ///  increase the computational efficiency
    Eigen::Vector3d vec_gradW_mod[150]; ///< corrected vec_gradW if asked
    ParticleManager *manager;           ///< pointer toward the object particle_manager
    double max_mu_ab;                   ///< maximum mu_ab of a particle (used for the timestep calculation)

public:
    FixedParticle() {}

    void save2disk(std::ofstream &file) const;
    void loadfromdisk(std::ifstream &ufile, double h_0);
    void getNeighbours();
    double calcPressure(double rho) const;
    double calcCelerity(double rho) const;
    void gradW();
    void kernel_corr();

    virtual void update_vars();
};

#endif // FIXEDPARTICLE_H
