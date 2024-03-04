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

