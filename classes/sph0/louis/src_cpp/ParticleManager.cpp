#include "ParticleManager.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include "FixedParticle.h"
#include "MobileParticle.h"
#include "ParticleSort.h"

ParticleManager::ParticleManager() : sorter(*this)
{
    this->timeStep = 1.0e-15;
    this->currentTime = 0.0;
    this->RKstep = 0;
}

void
ParticleManager::initialise()
{
    timers["initialisation"].start();

    // Reading of the paths of the input files
    std::ifstream file("paths.txt");
    if (!file.is_open())
    {
        std::cout << "Error: paths.txt not found" << std::endl;
        exit(1);
    }
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

    switch (this->kernelKind)
    {
    case K_CUBIC_SPLINE:
        this->kappa = 2;
        break;
    case K_QUADRATIC:
        this->kappa = 2;
        break;
    case K_QUINTIC_SPLINE:
        this->kappa = 3;
        break;
    }

    // Reading and storing of the data for the fixed particles
    std::ifstream file1(fp_path);
    if (!file1.is_open())
        throw std::runtime_error("Error: " + fp_path + " not found");

    for (int i = 0; i < this->numFP; i++)
    {
        FixedParticle *p = new FixedParticle(*this);
        p->loadfromdisk(file1, this->h_0);
        this->particles.push_back(p);
    }
    file1.close();

    // Reading and storing of the data for the mobile particles
    std::ifstream file2(mp_path);
    if (!file2.is_open())
        throw std::runtime_error("Error: " + mp_path + " not found");

    for (int i = 0; i < this->numMP; i++)
    {
        FixedParticle *p = new MobileParticle(*this);
        p->loadfromdisk(file2, this->h_0);
        this->particles.push_back(p);
    }
    file2.close();

    std::cout << "Initialisation finished." << std::endl;
    timers["initialisation"].stop();
}

/// Solves the problem.
/// It loops over time and uses a RK22 time integration scheme.

void
ParticleManager::solve()
{
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
        this->currentTime = this->currentTime + this->timeStep;

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
        for (int i = 0; i < this->numPart; i++)
        {
            FixedParticle *p = this->particles[i];
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
            timers["save"].start();

            this->save_particles("resMP", ite, this->numFP, this->numFP + this->numMP - 1);
            this->save_particles("resFP", ite, 0, this->numFP - 1);
            timers["save"].stop();

            // Display information

            double cpu = timers["TOTAL"].elapsed();
            double eta = (this->maxTime - this->currentTime)*cpu/this->currentTime;

            auto f(std::cout.flags()); // better choice: "std::format" in C++20
            std::cout << "It #" << std::setw(8) << ite
                      << " t = " << std::scientific << std::setprecision(3) << std::setw(10) << this->currentTime 
                      << " dt = " << std::setw(9) << this->timeStep 
                      << " - CPU = " << std::fixed << std::setprecision(2) << std::setw(10) << cpu << "s"
                        << " ETA = " << std::setw(10) << eta << "s"
                      << std::endl;
            std::cout.flags(f); // restore flags
            to_save = false;
        }

        this->update_dt();
        this->update_h();

        ite++;
    }
}

// Reading and storing of the data in the parameter files

void
ParticleManager::load_parameters(std::string const &param_path)
{
    std::ifstream file(param_path);
    if (!file.is_open())
    {
        std::cout << "Error: " << param_path << " not found" << std::endl;
        exit(1);
    }
    file >> this->numFP;
    file >> this->numMP;
    file >> this->h_0;
    file >> this->c_0;
    file >> this->rho_0;
    file >> this->dom_dim;
    file >> this->kernelKind;
    file >> this->alpha;
    file >> this->beta;
    file >> this->eqnState;
    file >> this->state_gamma;
    file >> this->molMass;
    file >> this->kernelCorrection;
    file >> this->maxTime;
    file >> this->saveInt;
    file.close();
}

/// Save a particle set onto disk.
/// @param name  : name of the file
/// @param ite   : iteration number
/// @param start : first particle to save
/// @param end   : last particle to save

void
ParticleManager::save_particles(std::string const &name, int ite,
                                int start, int end) const
{
    // build filename from name and ite
    std::ostringstream oss;
    oss << std::setw(8) << std::setfill('0') << ite;
    std::string filename = name + "_" + oss.str() + ".res";

    std::ofstream file;
    file.open(filename);
    for (int i = start; i <= end; ++i)
        this->particles[i]->save2disk(file);
    file.close();
}

/// Computes the next timestep using the properties of the particles.

void
ParticleManager::update_dt()
{
    timers["update_dt"].start();
    // computes the timestep relative to the body forces
    double dTf = sqrt(this->particles[this->numFP]->h / 9.81);
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        FixedParticle *cur_ptr = this->particles[i];
        double dTftemp = sqrt(cur_ptr->h / 9.81);
        if (dTftemp < dTf)
            dTf = dTftemp;
    }

    // computes the timestep relative to the CN and the viscous forces
    double dTcv = this->particles[this->numFP]->h /
                  (this->particles[this->numFP]->c[0] +
                   0.6 * (this->alpha * this->particles[this->numFP]->c[0] +
                          this->beta * this->particles[this->numFP]->max_mu_ab));
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        FixedParticle *cur_ptr = this->particles[i];
        double dTcvtemp = cur_ptr->h /
                          (cur_ptr->c[0] +
                           0.6 * (this->alpha * cur_ptr->c[0] + this->beta * cur_ptr->max_mu_ab));
        if (dTcvtemp < dTcv)
            dTcv = dTcvtemp;
    }

    // computes the final timestep
    if (0.4 * dTf > 0.25 * dTcv)
        this->timeStep = 0.25 * dTcv;
    else
        this->timeStep = 0.4 * dTf;

    // possibility to change the timestep if we use the ideal gas law
    if (this->eqnState == LAW_IDEAL_GAS)
        this->timeStep = 5 * this->timeStep;
    timers["update_dt"].stop();
}

/// Updates the smoothing length at each timestep.
/// It is written to provide the same smoothing length for every particle.

void
ParticleManager::update_h()
{
    timers["update_h"].start();

    // calculation of the average density
    double mean_rho = 0.0;
    for (int i = 0; i < this->numPart; i++)
        mean_rho = mean_rho + this->particles[i]->rho[0];
    mean_rho = mean_rho / this->numPart;

    // calculation of the new smoothing length
    double new_h = this->h_0 * pow(this->rho_0 / mean_rho, 1.0 / 3.0);

    // if the smoothing length is too large, it is limited
    if (new_h > 0.5 * this->sorter.cellSize)
    {
        new_h = 0.5 * this->sorter.cellSize;
        std::cout << "Warning: the smoothing has been limited" << std::endl;
    }

    // update of the smoothing length of all the particles
    for (int i = 0; i < this->numPart; i++)
        this->particles[i]->h = new_h;

    timers["update_h"].stop();
}
