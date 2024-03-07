#include "FixedParticle.h"
#include "ParticleManager.h"
#include <fstream>
#include <iostream>

FixedParticle::FixedParticle(ParticleManager &m) : Particle(m)
{
}

void
FixedParticle::update_vars()
{
    this->getNeighbours();
    this->gradW();

    int RKstep = this->manager.RKstep;

    double delta_rho = 0.0; // \f$d\rho/dt\f$
    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Particle *neigh = this->neighbours[i].ptr;
        Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
        delta_rho += this->m * u_ab.dot(this->vec_gradW[i]);
    }

    double dt = this->manager.timeStep;

    if (RKstep == 0) // 1st RK step
    {
        this->rho[1] = this->rho[0] + delta_rho * dt;
        this->rho[2] = this->rho[0] + delta_rho * dt / 2.0; // prepare second step
        this->speed[1] = this->speed[0];
        this->coord[1] = this->coord[0];
        this->p[1] = this->compute_pressure(this->rho[1]);
        this->c[1] = this->compute_speedofsound(this->rho[1]);
    }
    else // 2nd RK step
    {
        this->rho[2] = this->rho[2] + delta_rho * dt / 2.0;
        this->speed[2] = this->speed[1];
        this->coord[2] = this->coord[1];
        this->p[2] = this->compute_pressure(this->rho[2]);
        this->c[2] = this->compute_speedofsound(this->rho[2]);
    }
}
