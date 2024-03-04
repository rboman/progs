#ifndef PARTICLEMANAGER_H
#define PARTICLEMANAGER_H

#include "sph.h"
#include "ParticleSort.h"
#include <vector>

/// This class is used to manage all the particles,
/// i.e. it contains a reference to every particles,
/// it contains a number of parameters useful for the problem
/// (variable smoothing length or not, ...), it has a solver, etc.

class ParticleManager
{
public:
    ParticleSort sorting; ///< sorting machine
    //FixedParticle *part;  ///< array of pointers toward particles
    std::vector<FixedParticle *> part; ///< array of pointers toward particles

    int numFP;            ///< number of fixed particles
    int numMP;            ///< number of mobile particles
    int numPart;          ///< number of particles (FP+MP)
    int kernelKind;       ///< kind of kernel
    int kappa;            ///< kappa linked to the eqn state
    double alpha;         ///< weighting factor in the artificial viscosity formulation
    double beta;          ///< weighting factor in the artificial viscosity formulation
    int eqnState;         ///< equation of state
                          ///<   1 = ideal gas law
                          ///<   2 = quasi-incompressible fluid
    int state_gamma;      ///< power in eqn State 2.
                          ///< often taken around 7
    double molMass;       ///< Molar mass of the fluid for the prefect gas law
    int kernelCorrection; ///< correction of the kernel
                          ///<   0 = no correction
                          ///<   1 = correction enabled
    double maxTime;       ///< simulation time in seconds
    double saveInt;       ///< saving interval
    double h_0;           ///< initial smoothing length
    double rho_0;         ///< density of the fluid at free surface
    double c_0;           ///< speed of sound in normal conditions
    double timeStep;      ///< timestep (not constant)
    double currentTime;   ///< current time
    int RKstep;           ///< used to know in which RK iteration we are      [RB] (1 or 2)
    double dom_dim;       ///< length of a side of the domain
                          ///< (exterior particle to exterior particle).
                          ///< the domain is assumed to be cubic

public:
    ParticleManager();

    void initialisation();
    void solver();
    void readPRM(std::string const &param_path);
    void timeStepUpdate();
    void slUpdate();
    void savePartSet();
};

#endif // PARTICLEMANAGER_H
