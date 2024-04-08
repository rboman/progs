#ifndef SPH_PARTICLE_H
#define SPH_PARTICLE_H

#include "sph.h"
#include "sphNeighbour.h"

namespace sph
{

/// Base class for particles.

class SPH_API Particle
{
public:
#ifndef SWIG
    Model *model;

    Eigen::Vector3d coord[3]; ///< 3x1 array containing the coordinates of a particle.
                              ///      element 0 = currentTime
                              ///      element 1 = RKstep
                              ///      element 2 = nextTime
    Eigen::Vector3d speed[3]; ///< 3x1 array containing the velocity of a particle.
    double rho[3];            ///< 3x1 array containing the density of a particle.
    double m;                 ///< mass of the particle
    double p[3];              ///< 3x1 array containing the pressure of a particle.
    double c[3];              ///< 3x1 array containing the speed of sound of a particle.
    double h;                 ///< smoothing length
    double max_mu_ab;         ///< maximum mu_ab of a particle (used for the timestep calculation)

    std::vector<Neighbour> neighbours; ///< list of neighbours

    // std::vector<Eigen::Vector3d> vec_gradW;      // slow
    // std::vector<Eigen::Vector3d> vec_gradW_mod;  // slow

    Eigen::Vector3d vec_gradW[150];     ///< array that contains the gradient for every
                                        ///  neighbours; initially set to 150 elements to
                                        ///  increase the computational efficiency
    Eigen::Vector3d vec_gradW_mod[150]; ///< corrected vec_gradW if asked
#endif

public:
#ifndef SWIG
    explicit Particle(double x=0.0, double y=0.0, double z=0.0,
             double vx=0.0, double vy=0.0, double vz=0.0,
             double rho0=0.0, double m0=0.0);

    void save(std::ofstream &file) const;
    void load(std::ifstream &ufile, double h_0);
    void initialise(double h_0);

    virtual void update_vars() = 0;

    void to_fortran(std::ofstream &file) const;
#endif

    virtual ~Particle() = default;

protected:
    void gradW();
    void kernel_corr();
    void getNeighbours();
};

}; // namespace sph

#endif // SPH_PARTICLE_H
