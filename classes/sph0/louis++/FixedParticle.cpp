#include "FixedParticle.h"
#include "ParticleManager.h"
#include <fstream>
#include <iostream>

// loads the state of a particle from disk

void
FixedParticle::loadfromdisk(std::ifstream &ufile, double h_0)
{
    double x, y, z, u_x, u_y, u_z, rho, m;
    ufile >> x >> y >> z >> u_x >> u_y >> u_z >> rho >> m;
    this->coord[0] << x, y, z;
    this->speed[0] << u_x, u_y, u_z;
    this->rho[0] = rho;
    this->m = m;
    this->h = h_0;
    this->p[0] = this->calcPressure(rho);
    this->c[0] = this->calcCelerity(rho);
    this->max_mu_ab = 0.0;
}

// fixed_particle/calcPressure is a function that calculates the pressure according
//             to the equation of state chosen.
// @param rho  : actual density

double
FixedParticle::calcPressure(double rho)
{
    double calcPressure;
    const double idealGasCst = 8.3144621;

    switch (this->manager->eqnState)
    {
    case LAW_IDEAL_GAS:
    {
        calcPressure = (rho / this->manager->rho_0 - 1.0) *
                       idealGasCst * 293.15 / this->manager->molMass; // eq (3.24)
        break;
    }
    case LAW_QINC_FLUID:
    {
        double B = this->manager->c_0 * this->manager->c_0 *
                   this->manager->rho_0 / this->manager->state_gamma; // eq (3.27)
        calcPressure = B * (pow(rho / this->manager->rho_0, this->manager->state_gamma) - 1.0);
        break;
    }
    default:
    {
        std::cout << "Bad Equ of state (1,2)" << std::endl;
        exit(1);
    }
    }
    return calcPressure;
}

// fixed_particle/calcCelerity : calculates the celerity according
//              to the equation of state chosen.
//              The equation used is @f[c = \sqrt{\frac{dp}{d\rho}}@f]
// @param rho  : actual density

double
FixedParticle::calcCelerity(double rho)
{
    double celerity;

    switch (this->manager->eqnState)
    {
    case LAW_IDEAL_GAS:
        // 1 = considering the ideal gas law at 20 degrees C
        celerity = this->manager->c_0; // eq (3.36)
        break;
    case LAW_QINC_FLUID:
        // 2 = considering a quasi-incompressible fluid
        celerity = this->manager->c_0 *
                   pow(rho / this->manager->rho_0, (this->manager->state_gamma - 1) / 2); // eq (3.37)
        break;
    default:
        std::cout << "Bad Equ of state (1,2)" << std::endl;
        exit(1);
    }
    return celerity;
}

// saves the state of a particle onto disk

void
FixedParticle::save2disk(std::ofstream &file)
{
    file << this->coord[0].transpose() << " "
         << this->speed[0].transpose() << " "
         << this->rho[0] << " "
         << this->p[0] << " "
         << this->m << " "
         << this->c[0] << " "
         << this->h << " "
         << this->max_mu_ab << " "
         << this->numOfNeighbours << '\n';
}

void
FixedParticle::varUpdate()
{
    double Delta_rho = 0.0; // \f$d\rho/dt\f$

    int cur_RKstep = this->manager->RKstep; // pointer toward the value of the current RK step

    Eigen::Vector3d u_ab;                // relative velocity between the particle and a neighbour
    int i;                               // loop counter
    FixedParticle *cur_neigh;            // current neighbour
    double dt = this->manager->timeStep; // timestep

    this->getNeighbours();
    this->gradW();

    for (i = 0; i < this->numOfNeighbours; i++)
    {
        cur_neigh = this->neighbours[i].ptr;
        u_ab = this->speed[cur_RKstep] - cur_neigh->speed[cur_RKstep];
        Delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
    }

    if (cur_RKstep == 1)
    {
        this->rho[2] = this->rho[1] + Delta_rho * dt;
        this->rho[3] = this->rho[1] + Delta_rho * dt / 2.0;
        this->speed[2] = this->speed[1];
        this->coord[2] = this->coord[1];
        this->p[2] = this->calcPressure(this->rho[2]);
        this->c[2] = this->calcCelerity(this->rho[2]);
    }
    else
    {
        this->rho[3] = this->rho[3] + Delta_rho * dt / 2.0;
        this->speed[3] = this->speed[2];
        this->coord[3] = this->coord[2];
        this->p[3] = this->calcPressure(this->rho[3]);
        this->c[3] = this->calcCelerity(this->rho[3]);
    }
}

void
FixedParticle::getNeighbours()
{
    Eigen::Vector3d xyz = this->coord[this->manager->RKstep]; // position of the particle

    int xCell, yCell, zCell;                            // number of the cell according to x, y and z
    int nCellsSide = this->manager->sorting.nCellsSide; // number of cells on a row of the domain
    int i, j, k;                                        // loop counters
    int cellsToCheck[27];                               // number of the cells to check for the neighbours
    Eigen::Vector3d neighXYZ;                           // coordinates of a neighbour
    double r;                                           // distance between two particles
    FixedParticle *cur_ptr;                             // current pointer toward a particle
    Link *cur_neigh;                                    // current list of neighbours
    ParticleSort *srt = &this->manager->sorting;        // pointer toward the sorting machine
    std::vector<Link> *storage;                         // pointer toward the storage
    int cur_RKstep = this->manager->RKstep;             // RKstep
    int twice;                                          // [RB]

    if (cur_RKstep == 1)
    {
        // calculates the number of the cell in which the particle is
        xCell = (int)((xyz(0) - fmod(xyz(0), srt->cellSize)) / srt->cellSize) + 1;
        yCell = (int)((xyz(1) - fmod(xyz(1), srt->cellSize)) / srt->cellSize) + 1;
        zCell = (int)((xyz(2) - fmod(xyz(2), srt->cellSize)) / srt->cellSize) + 1;

        if (xCell < 1)
        {
            xCell = 1;
        }
        if (xCell > nCellsSide)
        {
            xCell = nCellsSide;
        }
        if (yCell < 1)
        {
            yCell = 1;
        }
        if (yCell > nCellsSide)
        {
            yCell = nCellsSide;
        }
        if (zCell < 1)
        {
            zCell = 1;
        }
        if (zCell > nCellsSide)
        {
            zCell = nCellsSide;
        }

        // calculates the number of the neighbouring cells
        for (i = -1; i < 2; i++)
        {
            for (j = -1; j < 2; j++)
            {
                for (k = -1; k < 2; k++)
                {
                    if ((xCell + i > 0) && (yCell + j > 0) && (zCell + k > 0) &&
                        (xCell + i <= nCellsSide) &&
                        (yCell + j <= nCellsSide) &&
                        (zCell + k <= nCellsSide))
                    {
                        cellsToCheck[(i + 1) * 9 + (j + 1) * 3 + (k + 2) - 1] =
                            (xCell + i - 1) * nCellsSide * nCellsSide +
                            (yCell + j - 1) * nCellsSide +
                            (zCell + k) - 1;
                    }
                    else
                    {
                        cellsToCheck[(i + 1) * 9 + (j + 1) * 3 + (k + 2) - 1] = 0;
                    }
                }
            }
        }
    }

    if (cur_RKstep == 1)
    {
        this->neighbours.clear();
        twice = 0; // [RB]
        for (i = 0; i < 27; i++)
        {
            if (cellsToCheck[i] > 0)
            {
                storage = &srt->storage[cellsToCheck[i]];
                for (j = 0; j < storage->size(); j++)
                {
                    cur_ptr = (*storage)[j].ptr;
                    neighXYZ = cur_ptr->coord[cur_RKstep];
                    r = (xyz - neighXYZ).norm();
                    if (r <= this->manager->kappa * this->h)
                    {
                        if (r > 1E-12) // [RB] why not cur_ptr /= this?
                        {
                            this->neighbours.push_back(Link(cur_ptr, r));
                        }
                        else
                        {
                            twice = twice + 1; // [RB]
                        }
                    }
                }
            }
        }
        // [RB] safeguard
        if (twice != 1)
        {
            std::cout << "safeguard activated!" << std::endl;
            std::cout << "    one particle has been taken into account " << twice << " times" << std::endl;
            std::cout << "    xCell = " << xCell << std::endl;
            std::cout << "    yCell = " << yCell << std::endl;
            std::cout << "    zCell = " << zCell << std::endl;
            std::cout << " cellsToCheck = " << cellsToCheck << std::endl;
            exit(1);
        }
        this->numOfNeighbours = this->neighbours.size();
    }
    else
    {
        // RK step2 - same neighbours and r is updated
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            cur_neigh = &this->neighbours[i];
            neighXYZ = cur_neigh->ptr->coord[cur_RKstep];
            cur_neigh->r = (xyz - neighXYZ).norm();
        }
    }
}



// !> gradW : creates a vector that contains the values
// !!       of the gradient for each neighbour.

// subroutine gradW(this)
//     class(fixed_particle), target :: this
//     integer  :: i
//     real(DP) :: alpha_d             !< normalisation coefficient
//     real(DP) :: r                   !< distance between a particle and a neighbour
//     class(fixed_particle), pointer :: cur_neigh     !< pointer toward a neighbour
//     real(DP) :: cur_h               !< value of h
//     integer  :: cur_RKstep          !< pointer toward the current RK step
    
//     if(this%numOfNeighbours>150) then
//         print *, 'Error: Number of neighbours greater than expected (max 150 for vec_gradW): ', this%numOfNeighbours
//         stop
//     end if
    
//     cur_h = this%h
//     cur_RKstep = this%manager%RKstep   
    
//     select case(this%manager%kernelKind)
//     case ( K_CUBIC_SPLINE) 
    
//         alpha_d = 3.d0/(2.d0*pi*cur_h**3)    ! [RB] efficiency of x**3.0d0 vs x**3 vs x*x*x ??
//                                                 ! values of alpha_d in table 2.1 p 23
//         do i = 1, this%numOfNeighbours
//             r = this%neighbours%lst(i)%r       ! [RB] a pointer is useless here!
//             cur_neigh => this%neighbours%lst(i)%ptr
//             if((r/cur_h>= 0.d0).and.(r/cur_h<1.d0)) then ! [RB] could "r/cur_h" be negative??
//                 this%vec_gradW(:, i) = alpha_d/cur_h &
//                             * (3.d0/2.d0*(r/cur_h)**2 - 2.d0*(r/cur_h) ) &
//                             * (this%coord(:, cur_RKstep) - cur_neigh%coord(:, cur_RKstep))/r       ! eq (2.26)
//             else if((r/cur_h>= 1.d0).and.(r/cur_h<2.d0)) then
//                 this%vec_gradW(:, i) = alpha_d/cur_h     &
//                             * (-0.5d0*(2.d0-r/cur_h)**2) &
//                             * (this%coord(:, cur_RKstep) - cur_neigh%coord(:, cur_RKstep))/r
//             else
//                 this%vec_gradW(:, i) = 0.d0
//             end if
//         end do
        
//     case ( K_QUADRATIC )
    
//         alpha_d = 5.d0/(4.d0*pi*cur_h**3)
        
//         do i = 1, this%numOfNeighbours
//             r = this%neighbours%lst(i)%r  ! [RB] a pointer is useless here!
//             cur_neigh => this%neighbours%lst(i)%ptr
            
//             if((r/cur_h>= 0.d0).and.(r/cur_h<= 2.d0)) then  ! [RB] could "r/cur_h" be negative??
//                 this%vec_gradW(:, i) = alpha_d/cur_h &
//                             * (3.d0/8.d0*r/cur_h-3.d0/4.d0)   &
//                             * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r            ! eq (2.28)
//             else
//                 this%vec_gradW(:, i) = 0.d0
//             end if
//         end do
        
//     case ( K_QUINTIC_SPLINE)
    
//         alpha_d = 3.d0/(359.d0*pi*cur_h**3)
        
//         do i = 1, this%numOfNeighbours
//             r = this%neighbours%lst(i)%r  ! [RB] a pointer is useless here!
//             cur_neigh => this%neighbours%lst(i)%ptr
//             if((r/cur_h>= 0.d0).and.(r/cur_h<1.d0)) then
//                 this%vec_gradW(:, i) = alpha_d/cur_h         &
//                             * ( -5.d0*(3.d0-r/cur_h)**4      &
//                                 + 30.d0*(2.d0-r/cur_h)**4    &
//                                 - 75.d0*(1.d0-r/cur_h)**4 )  &
//                             * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r      ! eq (2.32)
//             elseif((r/cur_h>= 1.d0).and.(r/cur_h<2.d0)) then
//                 this%vec_gradW(:, i) = alpha_d/cur_h         &
//                             * ( -5.d0*(3.d0-r/cur_h)**4      &
//                                 +30.d0*(2.d0-r/cur_h)**4 )   &
//                             * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r
//             elseif((r/cur_h>= 2.d0).and.(r/cur_h<3.d0)) then
//                 this%vec_gradW(:, i) = alpha_d/cur_h         &
//                             * (-5.d0*(3.d0-r/cur_h)**4)      &
//                             * (this%coord(:, cur_RKstep)-cur_neigh%coord(:, cur_RKstep))/r
//             else
//                 this%vec_gradW(:, i) = 0.d0
//             end if
//         end do
        
//     case default
//         print *, 'Bad value for kernel kind (1,2,3)'
//         stop
//     end select
    
//     if(this%manager%kernelCorrection == KCORR_ON) then
//         this%vec_gradW_mod = this%vec_gradW
//     end if
// end subroutine gradW
    
void
FixedParticle::gradW()
{
    double alpha_d; // normalisation coefficient
    double r;       // distance between a particle and a neighbour
    FixedParticle *cur_neigh; // pointer toward a neighbour
    double cur_h;   // value of h
    int cur_RKstep; // pointer toward the current RK step
    int i;          // loop counter

    if (this->numOfNeighbours > 150)
    {
        std::cout << "Error: Number of neighbours greater than expected (max 150 for vec_gradW): " << this->numOfNeighbours << std::endl;
        exit(1);
    }

    cur_h = this->h;
    cur_RKstep = this->manager->RKstep;

    switch (this->manager->kernelKind)
    {
    case K_CUBIC_SPLINE:
    {
        alpha_d = 3.0 / (2.0 * M_PI * cur_h * cur_h * cur_h);   // [RB] efficiency of x**3.0d0 vs x**3 vs x*x*x ??
                                                                // values of alpha_d in table 2.1 p 23
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            r = this->neighbours[i].r;
            cur_neigh = this->neighbours[i].ptr;
            if ((r / cur_h >= 0.0) && (r / cur_h < 1.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (1.5 * (r / cur_h) * (r / cur_h) - 2.0 * (r / cur_h)) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else if ((r / cur_h >= 1.0) && (r / cur_h < 2.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (-0.5 * (2.0 - r / cur_h) * (2.0 - r / cur_h)) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else
            {
                this->vec_gradW[i] = Eigen::Vector3d::Zero();
            }
        }
        break;
    }
    case K_QUADRATIC:
    {
        alpha_d = 5.0 / (4.0 * M_PI * cur_h * cur_h * cur_h);
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            r = this->neighbours[i].r;
            cur_neigh = this->neighbours[i].ptr;
            if ((r / cur_h >= 0.0) && (r / cur_h <= 2.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (0.375 * r / cur_h - 0.75) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else
            {
                this->vec_gradW[i] = Eigen::Vector3d::Zero();
            }
        }
        break;
    }
    case K_QUINTIC_SPLINE:
    {
        alpha_d = 3.0 / (359.0 * M_PI * cur_h * cur_h * cur_h);
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            r = this->neighbours[i].r;
            cur_neigh = this->neighbours[i].ptr;
            if ((r / cur_h >= 0.0) && (r / cur_h < 1.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (-5.0 * pow(3.0 - r / cur_h, 4) +
                                      30.0 * pow(2.0 - r / cur_h, 4) -
                                      75.0 * pow(1.0 - r / cur_h, 4)) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else if ((r / cur_h >= 1.0) && (r / cur_h < 2.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (-5.0 * pow(3.0 - r / cur_h, 4) +
                                      30.0 * pow(2.0 - r / cur_h, 4)) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else if ((r / cur_h >= 2.0) && (r / cur_h < 3.0))
            {
                this->vec_gradW[i] = alpha_d / cur_h *
                                     (-5.0 * pow(3.0 - r / cur_h, 4)) *
                                     (this->coord[cur_RKstep] - cur_neigh->coord[cur_RKstep]) / r;
            }
            else
            {
                this->vec_gradW[i] = Eigen::Vector3d::Zero();
            }
        }
        break;
    }
    default:
        std::cout << "Bad value for kernel kind (1,2,3)" << std::endl;
        exit(1);
    }

}
