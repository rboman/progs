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
    std::string param_path; // !< path of the parameters file
    std::string fp_path;    // !< path of the fixed particle(fp) file
    std::string mp_path;    // !< path of the mobile particle(mp) file

    this->timeStep = 1.0e-15; // !< initial time step
    this->currentTime = 0.0;  // !< current time initialisation
    this->RKstep = 1;         // !< RK step counter initialisation

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
        cur_ptr->neighbours.clear();
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
        cur_ptr->neighbours.clear();
        this->part.push_back(cur_ptr);
    }
    file2.close();

    // Particle sort
    this->sorting.manager = this;
    this->sorting.init = true;

    std::cout << "Initialisation finished." << std::endl;
}

// !> particle_manager/solver: solves the problem. It loops over time and uses
// !!      a RK22 time integration scheme.

// subroutine solver(this)
//     class(particle_manager) :: this

//     integer :: i, j
//     integer :: ite                  !< iteration counter
//     logical :: to_save              !< saving flag. If true a saving is done
//     class(fixed_particle), pointer :: p

//     ite = 0

//     do while(this%currentTime <= this%maxTime)

//         ! Time increment and saving status
//         if((floor(this%currentTime/this%saveInt) /= &
//             floor((this%currentTime+this%timeStep)/this%saveInt)) .or. ite == 0) then
//             to_save = .true.
//         end if
//         this%currentTime = this%currentTime + this%timeStep

//         ! Runge-Kutta loop
//         do j = 1, 2
//             this%RKstep = j
//             !if (j.eq.1) then ! [RB]
//                 call this%sorting%particlesSort()
//             !end if ! [RB]
//             ! Loop over the particles
//             !$OMP PARALLEL DO PRIVATE(i) SCHEDULE(DYNAMIC)
//             do i = 1, this%numPart
//                 call this%part(i)%ptr%varUpdate()
//             end do
//             !$OMP END PARALLEL DO
//         end do

//         ! Update of the current time variables (currentTime = nextTime)
//         do i = 1, this%numPart
//             p => this%part(i)%ptr
//             p%rho(1)      = p%rho(3)
//             p%p(1)        = p%p(3)
//             p%c(1)        = p%c(3)
//             p%speed(:, 1) = p%speed(:, 3)
//             p%coord(:, 1) = p%coord(:, 3)
//         end do

//         ! Test for the data saving
//         if(to_save) then
//             call this%savePartSet('resMP', ite, this%numFP+1, this%numFP+this%numMP)
//             call this%savePartSet('resFP', ite, 1, this%numFP)

//             print *, 'Iteration nb ', ite
//             print *, '   Time (s) = ', this%currentTime
//             print *, '   Time step (s) = ', this%timeStep
//             to_save = .false.
//         end if

//         call this%timeStepUpdate()
//         call this%slUpdate()

//         ite = ite + 1
//     end do

// end subroutine solver

void
ParticleManager::solver()
{
    int i, j;     // !< loop counters
    int ite;      // !< iteration counter
    bool to_save; // !< saving flag. If true a saving is done
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
        for (j = 1; j <= 2; j++)
        {
            this->RKstep = j;
            this->sorting.particlesSort();
            // Loop over the particles
#pragma omp parallel for private(i) schedule(dynamic)
            for (i = 0; i < this->numPart; i++)
            {
                this->part[i]->varUpdate();
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
            this->savePartSet("resMP", ite, this->numFP + 1, this->numFP + this->numMP);
            this->savePartSet("resFP", ite, 1, this->numFP);
            std::cout << "Iteration nb " << ite << std::endl;
            std::cout << "   Time (s) = " << this->currentTime << std::endl;
            std::cout << "   Time step (s) = " << this->timeStep << std::endl;
            to_save = false;
        }

        this->timeStepUpdate();
        this->slUpdate();

        ite = ite + 1;
    }
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

// !> save a particle set onto disk
// !! @param name  : name of the file
// !! @param ite   : iteration number
// !! @param start : first particle to save
// !! @param end   : last particle to save

void
ParticleManager::savePartSet(std::string const &name, int ite, int start, int end)
{
    int i;                // !< loop counter
    std::string filename; // !< file name
    std::ofstream file;   // !< file stream

    std::ostringstream oss;
    oss << std::setw(8) << std::setfill('0') << ite;

    filename = name + "_" + oss.str() + ".res";
    file.open(filename);
    for (i = start; i <= end; i++)
    {
        this->part[i]->save2disk(file);
    }
    file.close();
}
