#include "ParticleManager.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include "FixedParticle.h"
#include "MobileParticle.h"
#include "ParticleSort.h"

ParticleManager::ParticleManager()
{
}

void
ParticleManager::initialisation()
{
    std::string param_path; ///< path of the parameters file
    std::string fp_path;    ///< path of the fixed particle(fp) file
    std::string mp_path;    ///< path of the mobile particle(mp) file

    this->timeStep = 1.0e-15; ///< initial time step
    this->currentTime = 0.0;  ///< current time initialisation
    this->RKstep = 0;         ///< RK step counter initialisation

    int i; ///< loop counter
    FixedParticle *cur_ptr;

    // Reading of the paths of the input files
    std::ifstream file("paths.txt");
    if (!file.is_open())
    {
        std::cout << "Error: paths.txt not found" << std::endl;
        exit(1);
    }
    file >> param_path;
    file >> fp_path;
    file >> mp_path;
    file.close();

    this->readPRM(param_path);

    // allocation of the particles array

    this->numPart = this->numFP + this->numMP;
    this->part.reserve(this->numPart);

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
    {
        std::cout << "Error: " << fp_path << " not found" << std::endl;
        exit(1);
    }
    for (i = 0; i < this->numFP; i++)
    {
        cur_ptr = new FixedParticle();
        cur_ptr->manager = this;
        cur_ptr->loadfromdisk(file1, this->h_0);
        cur_ptr->neighbours.clear();
        this->part.push_back(cur_ptr);
    }
    file1.close();

    // Reading and storing of the data for the mobile particles
    std::ifstream file2(mp_path);
    if (!file2.is_open())
    {
        std::cout << "Error: " << mp_path << " not found" << std::endl;
        exit(1);
    }
    for (i = 0; i < this->numMP; i++)
    {
        cur_ptr = new MobileParticle();
        cur_ptr->manager = this;
        cur_ptr->loadfromdisk(file2, this->h_0);
        cur_ptr->neighbours.clear();
        this->part.push_back(cur_ptr);
    }
    file2.close();

    // Particle sort
    this->sorter.manager = this;
    this->sorter.init = true;

    std::cout << "Initialisation finished." << std::endl;
}

// !> particle_manager/solver: solves the problem. It loops over time and uses
// !!      a RK22 time integration scheme.

void
ParticleManager::solver()
{
    int i, j;     ///< loop counters
    int ite;      ///< iteration counter
    bool to_save; ///< saving flag. If true a saving is done
    FixedParticle *p;

    ite = 0;

    while (this->currentTime <= this->maxTime)
    {
        // Time increment and saving status
        if ((floor(this->currentTime / this->saveInt) !=
             floor((this->currentTime + this->timeStep) / this->saveInt)) ||
            ite == 0)
        {
            to_save = true;
        }
        this->currentTime = this->currentTime + this->timeStep;

        // Runge-Kutta loop
        for (j = 0; j < 2; j++)
        {
            this->RKstep = j;
            this->sorter.particlesSort();
            // Loop over the particles
#pragma omp parallel for private(i) schedule(dynamic)
            for (i = 0; i < this->numPart; i++)
            {
                this->part[i]->update_vars();
            }
        }

        // Update of the current time variables (currentTime = nextTime)
        for (i = 0; i < this->numPart; i++)
        {
            p = this->part[i];
            p->rho[0] = p->rho[2];
            p->p[0] = p->p[2];
            p->c[0] = p->c[2];
            p->speed[0] = p->speed[2];
            p->coord[0] = p->coord[2];
        }

        // Test for the data saving
        if (to_save)
        {
            this->savePartSet("resMP", ite, this->numFP, this->numFP + this->numMP - 1);
            this->savePartSet("resFP", ite, 0, this->numFP - 1);
            std::cout << "Iteration nb " << ite << std::endl;
            std::cout << "   Time (s) = " << this->currentTime << std::endl;
            std::cout << "   Time step (s) = " << this->timeStep << std::endl;
            to_save = false;
        }

        this->update_dt();
        this->update_h();

        ite = ite + 1;
    }
}

// Reading and storing of the data in the parameter files

void
ParticleManager::readPRM(std::string const &param_path)
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
ParticleManager::savePartSet(std::string const &name, int ite,
                             int start, int end)
{
    // build filename from name and ite
    std::ostringstream oss;
    oss << std::setw(8) << std::setfill('0') << ite;
    std::string filename = name + "_" + oss.str() + ".res";

    std::ofstream file;
    // configure output stream to output double as in fortran
    file.precision(15);
    file.setf(std::ios::scientific, std::ios::floatfield);
    file.open(filename);
    for (int i = start; i <= end; ++i)
        this->part[i]->save2disk(file);
    file.close();
}

/// Computes the next timestep using the properties of the particles.

void
ParticleManager::update_dt()
{
    // computes the timestep relative to the body forces
    double dTf = sqrt(this->part[this->numFP]->h / 9.81);
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        FixedParticle *cur_ptr = this->part[i];
        double dTftemp = sqrt(cur_ptr->h / 9.81);
        if (dTftemp < dTf)
            dTf = dTftemp;
    }

    // computes the timestep relative to the CN and the viscous forces
    double dTcv = this->part[this->numFP]->h /
                  (this->part[this->numFP]->c[0] +
                   0.6 * (this->alpha * this->part[this->numFP]->c[0] +
                          this->beta * this->part[this->numFP]->max_mu_ab));
    for (int i = this->numFP + 1; i < this->numPart; i++)
    {
        FixedParticle *cur_ptr = this->part[i];
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
}

/// Updates the smoothing length at each timestep.
/// It is written to provide the same smoothing length for every particle.

void
ParticleManager::update_h()
{
    // calculation of the average density
    double mean_rho = 0.0;
    for (int i = 0; i < this->numPart; i++)
        mean_rho = mean_rho + this->part[i]->rho[0];
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
        this->part[i]->h = new_h;
}
