#include "MobileParticle.h"
#include "ParticleManager.h"
#include <iostream>

// updates the density, the velocity and the position of a mobile particle.
// The integration scheme is a RK22 scheme.

void
MobileParticle::varUpdate()
{
    double Delta_rho;         // !< \f$d\rho/dt\f$
    Eigen::Vector3d Delta_u;  // !< \f$du/dt\f$
    Eigen::Vector3d Delta_x;  // !< \f$dx/dt\f$
    Eigen::Vector3d u_ab;     // !< relative velocity between the particle and a neighbour
    Eigen::Vector3d F;        // !< Volume forces
    double pi_ab;             // !< Artificial viscosity
    int i;                    // !< loopcounter
    int cur_RKstep;           // !< pointer toward the value of the current RK step
    FixedParticle *cur_neigh; // !< current neighbour
    double dt;

    Delta_rho = 0.0;
    Delta_u = Eigen::Vector3d::Zero();
    Delta_x = Eigen::Vector3d::Zero();
    F = Eigen::Vector3d(0.0, 0.0, -9.81);

    cur_RKstep = this->manager->RKstep;

    this->getNeighbours();
    this->gradW();

    if (this->manager->kernelCorrection == KCORR_ON)
    {
        this->kernel_corr();
    }

    // reset max_mu_ab
    if (cur_RKstep == 0)
    {
        this->max_mu_ab = 0.0;
    }

    switch (this->manager->kernelCorrection)
    {
    case KCORR_ON:
    {
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            cur_neigh = this->neighbours[i].ptr;
            u_ab = this->speed[cur_RKstep] - cur_neigh->speed[cur_RKstep];
            pi_ab = this->ArtificialViscosity(cur_neigh, this->manager->alpha, this->manager->beta);
            Delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
            Delta_u += this->m * (cur_neigh->p[cur_RKstep] / pow(cur_neigh->rho[cur_RKstep], 2) + this->p[cur_RKstep] / pow(this->rho[cur_RKstep], 2) + pi_ab) * this->vec_gradW_mod[i];
        }
        break;
    }
    case KCORR_OFF:
    {
        for (i = 0; i < this->numOfNeighbours; i++)
        {
            cur_neigh = this->neighbours[i].ptr;
            u_ab = this->speed[cur_RKstep] - cur_neigh->speed[cur_RKstep];
            pi_ab = this->ArtificialViscosity(cur_neigh, this->manager->alpha, this->manager->beta);
            Delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
            Delta_u += this->m * (cur_neigh->p[cur_RKstep] / pow(cur_neigh->rho[cur_RKstep], 2) + this->p[cur_RKstep] / pow(this->rho[cur_RKstep], 2) + pi_ab) * this->vec_gradW[i];
        }
        break;
    }
    default:
        std::cerr << "Bad value of kernel correction" << std::endl;
        exit(1);
    }

    Delta_u = -Delta_u + F;
    dt = this->manager->timeStep;

    if (cur_RKstep == 0) // 1st RK step
    {
        this->rho[1] = this->rho[0] + Delta_rho * dt;
        this->rho[2] = this->rho[0] + Delta_rho * dt / 2.0;
        this->speed[1] = this->speed[0] + Delta_u * dt;
        this->speed[2] = this->speed[0] + Delta_u * dt / 2.0;
        this->coord[1] = this->coord[0] + this->speed[0] * dt;
        this->coord[2] = this->coord[0] + this->speed[0] * dt / 2.0;
        this->p[1] = this->calcPressure(this->rho[1]);
        this->c[1] = this->calcCelerity(this->rho[1]);
    }
    else // 2nd RK step
    {
        this->rho[2] = this->rho[2] + Delta_rho * dt / 2.0;
        this->speed[2] = this->speed[2] + Delta_u * dt / 2.0;
        this->coord[2] = this->coord[2] + this->speed[1] * dt / 2.0;
        this->p[2] = this->calcPressure(this->rho[2]);
        this->c[2] = this->calcCelerity(this->rho[2]);
    }
}

// calculates the viscosity term in the momentum equation.
// @param neighObj : neighbouring object
// @param alpha    : coefficicent in the artificial viscosity formulation
// @param beta     : coefficicent in the artificial viscosity formulation

double
MobileParticle::ArtificialViscosity(FixedParticle *neighObj, double alpha, double beta)
{
    double mu_ab = 0.0;   // this term represents a kind of viscosity
    Eigen::Vector3d u_ab; // relative velocity of a in comparison with b
    Eigen::Vector3d x_ab; // the distance between a and b
    double c_ab;          // mean speed of sound
    double rho_ab;        // mean density

    u_ab = this->speed[this->manager->RKstep] - neighObj->speed[this->manager->RKstep];
    x_ab = this->coord[this->manager->RKstep] - neighObj->coord[this->manager->RKstep];

    if (u_ab.dot(x_ab) < 0.0)
    {
        // mu_ab is calculated using \f[\mu_{ab} = \frac{h\vec{u}_{ab}\cdot\vec{x}_{ab}}{\vec{x}_{ab}^2+\eta^2}\f]
        mu_ab = this->h * u_ab.dot(x_ab) / (x_ab.dot(x_ab) + 0.01 * this->h * this->h);
        c_ab = 0.5 * (this->c[this->manager->RKstep] + neighObj->c[this->manager->RKstep]);
        rho_ab = 0.5 * (this->rho[this->manager->RKstep] + neighObj->rho[this->manager->RKstep]);

        return (-alpha * c_ab * mu_ab + beta * mu_ab * mu_ab) / rho_ab;
    }
    else
    {
        return 0.0;
    }

    // update of max_mu_ab for the calculation of the timestep
    if ((this->manager->RKstep == 0) && (mu_ab > this->max_mu_ab))
    {
        this->max_mu_ab = mu_ab;
    }
}
