#include "sphModel.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include "sphFixedParticle.h"
#include "sphMobileParticle.h"
#include "sphSorter.h"
#include "sphKernels.h"
#include "sphEqState.h"
#include "sphDisplayHook.h"

using namespace sph;

Model::Model()
    : sorter(*this)
{
    this->timeStep = 1.0e-15;
    this->currentTime = 0.0;
    this->RKstep = 0;

    // init other variables
    this->numFP = 0;
    this->numMP = 0;
    this->h_0 = 0.0;
    this->dom_dim = 0.0;
    this->alpha = 0.0;
    this->beta = 0.0;
    this->kernelCorrection = 0.0;
    this->maxTime = 0.0;
    this->saveInt = 0.0;
    this->numPart = 0;
    this->nosave = false;

    this->kernel = std::make_shared<CubicSplineKernel>();
    this->eqState = std::make_shared<QincFluid>();
}

Model::~Model()
{
    // nothing needed: memory managed by shared_ptr's
}

/*
void
Model::initialise()
{
    g_timers["initialisation"].start();

    // Reading of the paths of the input files
    std::ifstream file("paths.txt");
    if (!file.is_open())
        throw std::runtime_error("paths.txt not found");

    std::string param_path; ///< path of the parameters file
    file >> param_path;
    std::string fp_path; ///< path of the fixed particle(fp) file
    file >> fp_path;
    std::string mp_path; ///< path of the mobile particle(mp) file
    file >> mp_path;
    file.close();

    this->load_parameters(param_path);

    // allocation of the particles array
    this->numPart = this->numFP + this->numMP;
    this->particles.reserve(this->numPart);

    // Reading and storing of the data for the fixed particles

    std::ifstream file1(fp_path);
    if (!file1.is_open())
        throw std::runtime_error(fp_path + " not found");

    int fsizFP = this->numFP;
    int fsizMP = this->numMP;
    this->numFP = 0;
    this->numMP = 0;

    for (int i = 0; i < fsizFP; i++)
    {
        // std::cout << "loading fixed particle " << i << " " << this->numFP << std::endl;
        auto p = this->add(std::make_shared<FixedParticle>());
        // std::cout << "p->load(file1, this->h_0) " << std::endl;
        p->load(file1, this->h_0);

    }
    file1.close();

    // Reading and storing of the data for the mobile particles

    std::ifstream file2(mp_path);
    if (!file2.is_open())
        throw std::runtime_error(mp_path + " not found");

    for (int i = 0; i < fsizMP; i++)
    {
        auto p = this->add(std::make_shared<MobileParticle>());
        p->load(file2, this->h_0);
    }
    file2.close();

    assert(this->numPart == this->numFP + this->numMP);

    g_timers["initialisation"].stop();
}
*/

/// Solves the problem using a RK22 time integration scheme.

void
Model::solve()
{
    Timer hooktimer;
    hooktimer.start();
    Timer couttimer;
    couttimer.start();

    int ite = 0;

    while (this->currentTime <= this->maxTime)
    {
        // Time increment and saving status
        bool to_save = false;
        if ((floor(this->currentTime / this->saveInt) !=
             floor((this->currentTime + this->timeStep) / this->saveInt)) ||
            ite == 0)
        {
            to_save = true;
        }
        this->currentTime += this->timeStep;

        // Runge-Kutta loop
        for (int j = 0; j < 2; j++)
        {
            this->RKstep = j;

            g_timers["sort"].start();
            // if(j==0)
            this->sorter.execute();
            g_timers["sort"].stop();

            g_timers["update_vars"].start();
#pragma omp parallel for schedule(dynamic)
            for (int i = 0; i < this->numPart; i++)
                this->particles[i]->update_vars();
            g_timers["update_vars"].stop();
        }

        // Update of the current time variables (currentTime = nextTime)
        g_timers["copy_vars"].start();
#pragma omp parallel for schedule(dynamic)
        for (int i = 0; i < this->numPart; i++)
        {
            Particle *p = this->particles[i].get();
            p->rho[0] = p->rho[2];
            p->p[0] = p->p[2];
            p->c[0] = p->c[2];
            p->speed[0] = p->speed[2];
            p->coord[0] = p->coord[2];
        }
        g_timers["copy_vars"].stop();

        // Test for the data saving
        if (to_save)
        {
            if (this->displayHook && hooktimer.elapsed() > 0.2)
            {
                hooktimer.reset();
                this->displayHook->update_data();
            }

            if (!this->nosave)
            {
                this->save_particles("resMP", ite, this->numFP, this->numFP + this->numMP - 1);
                this->save_particles("resFP", ite, 0, this->numFP - 1);
            }
            to_save = false;
        }

        // Display time-step information
        if (couttimer.elapsed() > 1.0 || ite == 0 ||
            this->currentTime >= this->maxTime)
        {
            double cpu = g_timers["TOTAL"].elapsed();
            double eta = (this->maxTime - this->currentTime) * cpu / this->currentTime;

            auto f(std::cout.flags()); // better choice: "std::format" in C++20
            std::cout << "It #" << std::setw(8) << ite
                      << " t = " << std::scientific << std::setprecision(3)
                      << std::setw(10) << this->currentTime
                      << " dt = " << std::setw(9) << this->timeStep
                      << " - CPU = " << std::fixed << std::setprecision(2)
                      << std::setw(10) << cpu << "s"
                      << " ETA = " << std::setw(10) << eta << "s"
                      << std::endl;
            std::cout.flags(f); // restore flags
            couttimer.reset();
        }

        if (this->displayHook)
        {
            this->displayHook->interact();
        }

        this->update_dt();
        this->update_h();

        ite++;
    }
}

/// Save a particle set onto disk.
/// @param name  : name of the file
/// @param ite   : iteration number
/// @param start : first particle to save
/// @param end   : last particle to save

void
Model::save_particles(std::string const &name, int ite,
                      int start, int end) const
{
    g_timers["save"].start();
    // build filename from name and ite
    std::ostringstream oss;
    oss << std::setw(8) << std::setfill('0') << ite;
    std::string filename = name + "_" + oss.str() + ".res";

    std::ofstream file;
    file.open(filename);
    for (int i = start; i <= end; ++i)
        this->particles[i]->save(file);
    file.close();
    g_timers["save"].stop();
}

/// Computes the next timestep using the properties of the particles.

void
Model::update_dt()
{
    g_timers["update_dt"].start();

    // timestep relative to the body forces
    // - mistake in Louis' thesis: the square root is missing
    // - mistake in Monagan-1989 paper: sq root is applied to g instead of h/g
    double g = 9.81;

    double dTf = std::numeric_limits<double>::max();
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        Particle const *p = this->particles[i].get();
        double dt = sqrt(p->h / g);
        if (dt < dTf)
            dTf = dt;
    }

    // timestep relative to the Courant number and the viscous forces

    double dTcv = std::numeric_limits<double>::max();
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        Particle *p = this->particles[i].get();
        double dt = p->h / (p->c[0] + 0.6 * (this->alpha * p->c[0] + this->beta * p->max_mu_ab));
        if (dt < dTcv)
            dTcv = dt;
    }

    // final timestep
    this->timeStep = std::min(0.4 * dTf, 0.25 * dTcv);

    // possibility to change the timestep if we use the ideal gas law
    this->timeStep *= this->eqState->dt_factor();

    g_timers["update_dt"].stop();
}

/// Updates the smoothing length at each timestep.
/// It is written to provide the same smoothing length for every particle.

void
Model::update_h()
{
    g_timers["update_h"].start();

    // calculation of the average density
    double mean_rho = 0.0;
    for (int i = 0; i < this->numPart; i++)
        mean_rho += this->particles[i]->rho[0];
    mean_rho = mean_rho / this->numPart;

    // calculation of the new smoothing length
    double new_h = this->h_0 * pow(this->eqState->rho0 / mean_rho, 1.0 / 3.0);

    // if the smoothing length is too large, it is limited
    if (new_h > 0.5 * this->sorter.dx)
    {
        new_h = 0.5 * this->sorter.dx;
        std::cout << "Warning: the smoothing has been limited" << std::endl;
    }

    // update of the smoothing length of all the particles
    for (int i = 0; i < this->numPart; i++)
        this->particles[i]->h = new_h;

    g_timers["update_h"].stop();
}

/// Send the particles to a file which will be read by Louis' FORTRAN code

void
Model::to_fortran()
{
    // particles

    std::ofstream file_mp("input.mp");
    if (!file_mp.is_open())
        throw std::runtime_error("error opening input.mp");

    std::ofstream file_fp("input.fp");
    if (!file_fp.is_open())
        throw std::runtime_error("error opening input.fp");

    for (auto p : this->particles)
    {
        if (dynamic_cast<FixedParticle *>(p.get()))
            p->to_fortran(file_fp);
        else
            p->to_fortran(file_mp);
    }

    file_mp.close();
    file_fp.close();

    // parameters input.prm

    std::ofstream file_prm("input.prm");
    if (!file_prm.is_open())
        throw std::runtime_error("error opening input.prm");

    if(!this->eqState)
        throw std::runtime_error("eqState is not set");
    if(!this->kernel)
        throw std::runtime_error("kernel is not set");

    file_prm << this->numFP << '\n';
    file_prm << this->numMP << '\n';
    file_prm << this->h_0 << '\n';
    file_prm << this->eqState->c0 << '\n';
    file_prm << this->eqState->rho0 << '\n';
    file_prm << this->dom_dim << '\n';
    file_prm << this->kernel->fortran_kind() << '\n';
    file_prm << this->alpha << '\n';
    file_prm << this->beta << '\n';

    IdealGas *idealGas = dynamic_cast<IdealGas *>(this->eqState.get());
    QincFluid *qincFluid = dynamic_cast<QincFluid *>(this->eqState.get());

    if (idealGas)
    {
        file_prm << LAW_IDEAL_GAS << '\n';
        file_prm << QincFluid().gamma << '\n';
        file_prm << idealGas->M << '\n';
    }
    else if(qincFluid)
    {
        file_prm << LAW_QINC_FLUID << '\n';
        file_prm << qincFluid->gamma << '\n';
        file_prm << IdealGas().M << '\n';
    }
    else
        throw std::runtime_error("Bad value of equation of state");
    file_prm << this->kernelCorrection << '\n';
    file_prm << this->maxTime << '\n';
    file_prm << this->saveInt << '\n';

    file_prm.close();

    // contains paths to the input files
    // TODO: remove the need of this file in the Fortran code

    std::ofstream file_paths("paths.txt");
    if (!file_paths.is_open())
        throw std::runtime_error("error opening paths.txt");
    file_paths << "input.prm\n";
    file_paths << "input.fp\n";
    file_paths << "input.mp\n";
    file_paths.close();
}

std::shared_ptr<Particle> 
Model::add(std::shared_ptr<Particle> p)
{
    if (dynamic_cast<FixedParticle *>(p.get()))
    {
        // TODO: attention, le code fait parfois l'hypothèse que les fixes précèdent les mobiles!
        // => à modifier!!
        if (this->numMP != 0)
            throw std::runtime_error("Fixed particles must be added before mobile particles");
        this->numFP++;
    }
    else
    {
        this->numMP++;
    }
    this->particles.push_back(p);
    this->numPart = this->numFP + this->numMP; // a supprimer
    p->model = this;

    return p;
}

void
Model::run()
{
    std::cout << *this << std::endl;

    print_banner();

    g_timers["TOTAL"].start();

    g_timers["initialisation"].start();
    for(auto &p : this->particles)
        p->initialise(this->h_0);
    g_timers["initialisation"].stop();

    this->solve();
    g_timers["TOTAL"].stop();

    print_timers();
    save_timers();
}

namespace sph {

SPH_API std::ostream &operator<<(std::ostream &os, const Model &m)
{
    os << "Model:\n";
    os << "  numFP = " << m.numFP << "\n";
    os << "  numMP = " << m.numMP << "\n";
    os << "  numPart = " << m.numPart << "\n";
    os << "  h_0 = " << m.h_0 << "\n";
    os << "  dom_dim = " << m.dom_dim << "\n";
    os << "  alpha = " << m.alpha << "\n";
    os << "  beta = " << m.beta << "\n";
    //os << "  kappa = " << m.kappa << "\n";
    os << "  kernelCorrection = " << m.kernelCorrection << "\n";
    os << "  maxTime = " << m.maxTime << "\n";
    os << "  saveInt = " << m.saveInt << "\n";
    os << "  timeStep = " << m.timeStep << "\n";
    os << "  currentTime = " << m.currentTime << "\n";
    os << "  RKstep = " << m.RKstep << "\n";
    os << "  kernel = " << *m.kernel << "\n";
    os << "  eqState = " << *m.eqState << "\n";

    return os;
}

}; // namespace sph

