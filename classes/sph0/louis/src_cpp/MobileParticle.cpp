#include "MobileParticle.h"
#include "ParticleManager.h"
#include <iostream>

/// updates the density, the velocity and the position of a mobile particle.
/// The integration scheme is a RK22 scheme.

void
MobileParticle::varUpdate()
{
    this->getNeighbours();
    this->gradW();

    if (this->manager->kernelCorrection == KCORR_ON)
    {
        this->kernel_corr();
    }

    // reset max_mu_ab
    int RKstep = this->manager->RKstep;
    if (RKstep == 0)
        this->max_mu_ab = 0.0;

    double delta_rho = 0.0;                            ///< \f$d\rho/dt\f$
    Eigen::Vector3d delta_u = Eigen::Vector3d::Zero(); ///< \f$du/dt\f$

    switch (this->manager->kernelCorrection)
    {
    case KCORR_ON:
    {
        for (int i = 0; i < this->numOfNeighbours; i++)
        {
            FixedParticle *neigh = this->neighbours[i].ptr;
            Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
            double pi_ab = this->artificialViscosity(neigh, this->manager->alpha, this->manager->beta);
            delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
            delta_u += this->m * (neigh->p[RKstep] / pow(neigh->rho[RKstep], 2) + this->p[RKstep] / pow(this->rho[RKstep], 2) + pi_ab) * this->vec_gradW_mod[i];
        }
        break;
    }
    case KCORR_OFF:
    {
        for (int i = 0; i < this->numOfNeighbours; i++)
        {
            FixedParticle *neigh = this->neighbours[i].ptr;
            Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
            double pi_ab = this->artificialViscosity(neigh, this->manager->alpha, this->manager->beta);
            delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
            delta_u += this->m * (neigh->p[RKstep] / pow(neigh->rho[RKstep], 2) + this->p[RKstep] / pow(this->rho[RKstep], 2) + pi_ab) * this->vec_gradW[i];
        }
        break;
    }
    default:
        throw std::runtime_error("Bad value of kernel correction");
    }

    Eigen::Vector3d F = Eigen::Vector3d(0.0, 0.0, -9.81); ///< Volume forces
    delta_u = -delta_u + F;

    double dt = this->manager->timeStep;

    if (RKstep == 0) // 1st RK step
    {
        this->rho[1] = this->rho[0] + delta_rho * dt;
        this->rho[2] = this->rho[0] + delta_rho * dt / 2.0;
        this->speed[1] = this->speed[0] + delta_u * dt;
        this->speed[2] = this->speed[0] + delta_u * dt / 2.0;
        this->coord[1] = this->coord[0] + this->speed[0] * dt;
        this->coord[2] = this->coord[0] + this->speed[0] * dt / 2.0;
        this->p[1] = this->calcPressure(this->rho[1]);
        this->c[1] = this->calcCelerity(this->rho[1]);
    }
    else // 2nd RK step
    {
        this->rho[2] = this->rho[2] + delta_rho * dt / 2.0;
        this->speed[2] = this->speed[2] + delta_u * dt / 2.0;
        this->coord[2] = this->coord[2] + this->speed[1] * dt / 2.0;
        this->p[2] = this->calcPressure(this->rho[2]);
        this->c[2] = this->calcCelerity(this->rho[2]);
    }
}

// Calculates the viscosity term in the momentum equation.
// Update also "this->max_mu_ab"
// @param neighObj : neighbouring object
// @param alpha    : coefficicent in the artificial viscosity formulation
// @param beta     : coefficicent in the artificial viscosity formulation

double
MobileParticle::artificialViscosity(FixedParticle *neigh, double alpha, double beta)
{
    double viscosity = 0.0;
    int RKstep = this->manager->RKstep;
    // relative velocity of a in comparison with b
    Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
    // the distance between a and b
    Eigen::Vector3d x_ab = this->coord[RKstep] - neigh->coord[RKstep];

    double mu_ab = 0.0;   // this term represents a kind of viscosity     
    if (u_ab.dot(x_ab) < 0.0)
    {
        // mu_ab is calculated using \f[\mu_{ab} = \frac{h\vec{u}_{ab}\cdot\vec{x}_{ab}}{\vec{x}_{ab}^2+\eta^2}\f]
        mu_ab = this->h * u_ab.dot(x_ab) / (x_ab.dot(x_ab) + 0.01 * this->h * this->h);
        double c_ab = 0.5 * (this->c[RKstep] + neigh->c[RKstep]);// mean speed of sound
        double rho_ab = 0.5 * (this->rho[RKstep] + neigh->rho[RKstep]);  // mean density
        viscosity = (-alpha * c_ab * mu_ab + beta * mu_ab * mu_ab) / rho_ab;
    }

    // update of max_mu_ab for the calculation of the timestep
    if ((RKstep == 0) && (mu_ab > this->max_mu_ab))
        this->max_mu_ab = mu_ab;

    return viscosity;
}
