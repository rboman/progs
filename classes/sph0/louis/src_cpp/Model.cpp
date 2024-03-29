#include "Model.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include "FixedParticle.h"
#include "MobileParticle.h"
#include "Sorter.h"
#include "Kernels.h"
#include "EqState.h"
#include "DisplayHook.h"

Model::Model()
    : sorter(*this), kernel(nullptr),
      eqState(nullptr), displayHook(nullptr)
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
    this->kappa = 0.0;
    this->kernelCorrection = 0.0;
    this->maxTime = 0.0;
    this->saveInt = 0.0;
    this->numPart = 0;
}

Model::~Model()
{
    for (int i = 0; i < this->numPart; i++)
        delete this->particles[i];
    delete this->kernel;
    delete this->eqState;
    delete this->displayHook;
}

void
Model::initialise()
{
    timers["initialisation"].start();

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

    for (int i = 0; i < this->numFP; i++)
    {
        FixedParticle *p = new FixedParticle(*this);
        p->load(file1, this->h_0);
        this->particles.push_back(p);
    }
    file1.close();

    // Reading and storing of the data for the mobile particles
    std::ifstream file2(mp_path);
    if (!file2.is_open())
        throw std::runtime_error(mp_path + " not found");

    for (int i = 0; i < this->numMP; i++)
    {
        MobileParticle *p = new MobileParticle(*this);
        p->load(file2, this->h_0);
        this->particles.push_back(p);
    }
    file2.close();

    std::cout << "Initialisation finished." << std::endl;
    timers["initialisation"].stop();
}

/// Solves the problem using a RK22 time integration scheme.

void
Model::solve()
{
    Timer hooktimer;
    hooktimer.start();

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

            timers["sort"].start();
            this->sorter.execute();
            timers["sort"].stop();

            timers["update_vars"].start();
#pragma omp parallel for schedule(dynamic)
            for (int i = 0; i < this->numPart; i++)
                this->particles[i]->update_vars();
            timers["update_vars"].stop();
        }

        // Update of the current time variables (currentTime = nextTime)
        timers["copy vars"].start();
#pragma omp parallel for schedule(dynamic)
        for (int i = 0; i < this->numPart; i++)
        {
            Particle *p = this->particles[i];
            p->rho[0] = p->rho[2];
            p->p[0] = p->p[2];
            p->c[0] = p->c[2];
            p->speed[0] = p->speed[2];
            p->coord[0] = p->coord[2];
        }
        timers["copy vars"].stop();

        // Test for the data saving
        if (to_save)
        {
            if (this->displayHook != nullptr && hooktimer.elapsed() > 0.2)
            {
                hooktimer.reset();
                this->displayHook->update_data();
            }

            this->save_particles("resMP", ite, this->numFP, this->numFP + this->numMP - 1);
            this->save_particles("resFP", ite, 0, this->numFP - 1);

            // Display time-step information

            double cpu = timers["TOTAL"].elapsed();
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
            to_save = false;
        }

        if (this->displayHook != nullptr)
        {
            this->displayHook->interact();
        }

        this->update_dt();
        this->update_h();

        ite++;
    }
}

// Reading and storing of the data in the parameter files

void
Model::load_parameters(std::string const &param_path)
{
    std::ifstream file(param_path);
    if (!file.is_open())
        throw std::runtime_error(param_path + " not found");

    file >> this->numFP;
    file >> this->numMP;
    file >> this->h_0;
    double c0, rho0;
    file >> c0;
    file >> rho0;
    file >> this->dom_dim;
    int kernelKind;
    file >> kernelKind;
    file >> this->alpha;
    file >> this->beta;
    int eqnState;
    int gamma;
    double molMass;
    file >> eqnState;
    file >> gamma;
    file >> molMass;
    file >> this->kernelCorrection;
    file >> this->maxTime;
    file >> this->saveInt;
    file.close();

    // create kernel
    switch (kernelKind)
    {
    case K_CUBIC_SPLINE:
        this->kernel = new CubicSplineKernel();
        break;
    case K_QUADRATIC:
        this->kernel = new QuadraticKernel();
        break;
    case K_QUINTIC_SPLINE:
        this->kernel = new QuinticSplineKernel();
        break;
    default:
        throw std::runtime_error("Bad value of kernel kind");
    }
    this->kappa = this->kernel->kappa;

    // create equation of state
    switch (eqnState)
    {
    case LAW_IDEAL_GAS:
        this->eqState = new IdealGas(rho0, c0, molMass);
        break;
    case LAW_QINC_FLUID:
        this->eqState = new QincFluid(rho0, c0, gamma);
        break;
    default:
        throw std::runtime_error("Bad value of equation of state");
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
    timers["save"].start();
    // build filename from name and ite
    std::ostringstream oss;
    oss << std::setw(8) << std::setfill('0') << ite;
    std::string filename = name + "_" + oss.str() + ".res";

    std::ofstream file;
    file.open(filename);
    for (int i = start; i <= end; ++i)
        this->particles[i]->save(file);
    file.close();
    timers["save"].stop();
}

/// Computes the next timestep using the properties of the particles.

void
Model::update_dt()
{
    timers["update_dt"].start();

    // timestep relative to the body forces
    // - mistake in Louis' thesis: the square root is missing
    // - mistake in Monagan-1989 paper: sq root is applied to g instead of h/g
    double g = 9.81;

    double dTf = std::numeric_limits<double>::max();
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        Particle const *p = this->particles[i];
        double dt = sqrt( p->h / g );
        if (dt < dTf)
            dTf = dt;
    }

    // timestep relative to the Courant number and the viscous forces

    double dTcv = std::numeric_limits<double>::max();
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        Particle *p = this->particles[i];
        double dt = p->h / (p->c[0] + 0.6 * (this->alpha * p->c[0] + this->beta * p->max_mu_ab));
        if (dt < dTcv)
            dTcv = dt;
    }

    // final timestep
    this->timeStep = std::min(0.4 * dTf, 0.25 * dTcv);

    // possibility to change the timestep if we use the ideal gas law
    this->timeStep *= this->eqState->dt_factor();

    // if (this->eqnState == LAW_IDEAL_GAS)
    //     this->timeStep = 5 * this->timeStep;

    timers["update_dt"].stop();
}

/// Updates the smoothing length at each timestep.
/// It is written to provide the same smoothing length for every particle.

void
Model::update_h()
{
    timers["update_h"].start();

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

    timers["update_h"].stop();
}
