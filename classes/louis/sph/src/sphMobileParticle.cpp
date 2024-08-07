#include "sphMobileParticle.h"
#include "sphModel.h"
#include "sphEqState.h"
#include <iostream>

using namespace sph;

MobileParticle::MobileParticle(double x, double y, double z,
                               double vx, double vy, double vz,
                               double rho0, double m0)
    : Particle(x, y, z, vx, vy, vz, rho0, m0)
{
}

/// updates the density, the velocity and the position of a mobile particle.
/// The integration scheme is a RK22 scheme.

void
MobileParticle::update_vars()
{
    assert(this->model != nullptr);

    this->getNeighbours();
    this->gradW();

    if (this->model->kernelCorrection == KCORR_ON)
        this->kernel_corr();

    // reset max_mu_ab
    int RKstep = this->model->RKstep;
    if (RKstep == 0)
        this->max_mu_ab = 0.0;

    double drho_dt = 0.0;
    Eigen::Vector3d du_dt = Eigen::Vector3d::Zero();

    switch (this->model->kernelCorrection)
    {
    case KCORR_ON:
    {
        double rho2a = this->rho[RKstep] * this->rho[RKstep];

        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            Particle *neigh = this->neighbours[i].p;
            Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
            double pi_ab = this->compute_viscosity(neigh, this->model->alpha, this->model->beta);
            drho_dt += this->m * u_ab.dot(this->vec_gradW[i]);
            double rho2b = neigh->rho[RKstep] * neigh->rho[RKstep];
            du_dt += this->m * (neigh->p[RKstep] / rho2b + this->p[RKstep] / rho2a + pi_ab) * this->vec_gradW_mod[i];
        }
        break;
    }
    case KCORR_OFF:
    {
        double rho2a = this->rho[RKstep] * this->rho[RKstep];

        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            Particle *neigh = this->neighbours[i].p;
            Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
            double pi_ab = this->compute_viscosity(neigh, this->model->alpha, this->model->beta);
            drho_dt += this->m * u_ab.dot(this->vec_gradW[i]);
            double rho2b = neigh->rho[RKstep] * neigh->rho[RKstep];
            du_dt += this->m * (neigh->p[RKstep] / rho2b + this->p[RKstep] / rho2a + pi_ab) * this->vec_gradW[i];
        }
        break;
    }
    default:
        throw std::runtime_error("Bad value of kernel correction");
    }

    Eigen::Vector3d F = Eigen::Vector3d(0.0, 0.0, -9.81); ///< Volume forces
    du_dt = -du_dt + F;

    double dt = this->model->timeStep;

    if (RKstep == 0) // 1st RK step
    {
        this->rho[1] = this->rho[0] + drho_dt * dt;
        this->rho[2] = this->rho[0] + drho_dt * dt / 2.0;
        this->speed[1] = this->speed[0] + du_dt * dt;
        this->speed[2] = this->speed[0] + du_dt * dt / 2.0;
        this->coord[1] = this->coord[0] + this->speed[0] * dt;
        this->coord[2] = this->coord[0] + this->speed[0] * dt / 2.0;
        this->p[1] = this->model->eqState->pressure(this->rho[1]);
        this->c[1] = this->model->eqState->speed_of_sound(this->rho[1]);
    }
    else // 2nd RK step
    {
        this->rho[2] = this->rho[2] + drho_dt * dt / 2.0;
        this->speed[2] = this->speed[2] + du_dt * dt / 2.0;
        this->coord[2] = this->coord[2] + this->speed[1] * dt / 2.0;
        this->p[2] = this->model->eqState->pressure(this->rho[2]);
        this->c[2] = this->model->eqState->speed_of_sound(this->rho[2]);
    }
}

// Calculates the viscosity term in the momentum equation.
// Update also "this->max_mu_ab" for the calculation of the timestep.

double
MobileParticle::compute_viscosity(Particle *neigh, double alpha, double beta)
{
    assert(this->model != nullptr);

    double viscosity = 0.0;
    int RKstep = this->model->RKstep;

    Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
    Eigen::Vector3d x_ab = this->coord[RKstep] - neigh->coord[RKstep];

    double mu_ab = 0.0; // this term represents a kind of viscosity
    double uab_xab = u_ab.dot(x_ab);
    if (uab_xab < 0.0)
    {
        // mistake in the formula in Louis' thesis: missing negative sign
        // (see Monagan-1989 eq 4.11 p8)
        //   => max(mu_ab) calculated later for the timestep will always be 0!
        // note: some papers use a negative mu_ab with an absolute value when finding the max.
        mu_ab = -this->h * uab_xab / (x_ab.dot(x_ab) + 0.01 * this->h * this->h);

        double c_ab = 0.5 * (this->c[RKstep] + neigh->c[RKstep]);
        double rho_ab = 0.5 * (this->rho[RKstep] + neigh->rho[RKstep]);
        viscosity = (alpha * c_ab * mu_ab + beta * mu_ab * mu_ab) / rho_ab;
        // negative sign removed here due to sign correction in mu_ab
    }

    if ((RKstep == 0) && (mu_ab > this->max_mu_ab))
        this->max_mu_ab = mu_ab;

    return viscosity;
}
