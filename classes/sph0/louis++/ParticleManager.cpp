#include "ParticleManager.h"
#include <fstream>
#include <iostream>
#include <string>
#include "FixedParticle.h"
#include "MobileParticle.h"
#include "ParticleSort.h"


ParticleManager::ParticleManager()
{
}


void
ParticleManager::initialisation()
{
    std::string param_path; // !< path of the parameters file
    std::string fp_path; // !< path of the fixed particle(fp) file
    std::string mp_path; // !< path of the mobile particle(mp) file

    this->timeStep = 1.0e-15; // !< initial time step
    this->currentTime = 0.0; // !< current time initialisation
    this->RKstep = 1; // !< RK step counter initialisation

    int i; // !< loop counter
    FixedParticle *cur_ptr;

    // Reading of the paths of the input files
    std::ifstream file("paths.txt");
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
    for (i = 0; i < this->numFP; i++)
    {
        cur_ptr = new FixedParticle();
        cur_ptr->manager = this;
        cur_ptr->loadfromdisk(file1, this->h_0);
        cur_ptr->neighbours.initList();
        this->part.push_back(cur_ptr);
    }
    file1.close();

    // Reading and storing of the data for the mobile particles
    std::ifstream file2(mp_path);
    for (i = 0; i < this->numMP; i++)
    {
        cur_ptr = new MobileParticle();
        cur_ptr->manager = this;
        cur_ptr->loadfromdisk(file2, this->h_0);
        cur_ptr->neighbours.initList();
        this->part.push_back(cur_ptr);
    }
    file2.close();

    // Particle sort
    this->sorting.manager = this;
    this->sorting.init = true;

    std::cout << "Initialisation finished." << std::endl;
}



    
//     ! Particle sort
//     this%sorting%manager => this
//     this%sorting%init = .true.

//     print *, 'Initialisation finished.'
// end subroutine initialisation





void
ParticleManager::solver()
{
}



// Reading and storing of the data in the parameter files

void
ParticleManager::readPRM(std::string const &param_path)
{
    std::ifstream file(param_path);
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


void
ParticleManager::timeStepUpdate()
{
}

void
ParticleManager::slUpdate()
{
}

void
ParticleManager::savePartSet()
{
}