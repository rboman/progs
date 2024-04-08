#ifndef SPH_MODEL_H
#define SPH_MODEL_H

#include "sph.h"
#include "sphSorter.h"
#include <vector>
#include <memory>

namespace sph {

/// This class is used to manage all the particles,
/// i.e. it contains a reference to every particles,
/// it contains a number of parameters useful for the problem
/// (variable smoothing length or not, ...), it has a solver, etc.

class SPH_API Model
{
public:
#ifndef SWIG
    Sorter sorter;

    std::shared_ptr<DisplayHook> displayHook;

    /// array of pointers toward particles.
    /// the particles are generated in python, so we need to use shared_ptrs
    std::vector<std::shared_ptr<Particle>> particles;
#endif
    
    std::shared_ptr<Kernel> kernel;
    std::shared_ptr<EqState> eqState;

    int numFP;            ///< number of fixed particles
    int numMP;            ///< number of mobile particles
    int numPart;          ///< number of particles (FP+MP)

    double alpha;         ///< weighting factor in the artificial viscosity formulation
    double beta;          ///< weighting factor in the artificial viscosity formulation

    int kernelCorrection; ///< correction of the kernel
                          ///<   0 = no correction
                          ///<   1 = correction enabled
    double maxTime;       ///< simulation time in seconds
    double saveInt;       ///< saving interval
    double h_0;           ///< initial smoothing length

    double timeStep;      ///< timestep (not constant)
    double currentTime;   ///< current time
    int RKstep;           ///< used to know in which RK iteration we are      [RB] (1 or 2)
    double dom_dim;       ///< length of a side of the domain
                          ///< (exterior particle to exterior particle).
                          ///< the domain is assumed to be cubic

public:
    Model();
    ~Model();

    void run();
    void set_hook(std::shared_ptr<DisplayHook> hook) { displayHook = hook; }

#ifndef SWIG
    //void initialise();
    void solve();
    friend SPH_API std::ostream &operator<<(std::ostream &os, const Model &m);
#endif
    void to_fortran();
    std::shared_ptr<Particle> add(std::shared_ptr<Particle> p);

private:
    //void load_parameters(std::string const &param_path);
    void save_particles(std::string const &name, int ite, int start, int end) const;
    void update_dt();
    void update_h();
};

}; // namespace sph


#endif // SPH_MODEL_H
