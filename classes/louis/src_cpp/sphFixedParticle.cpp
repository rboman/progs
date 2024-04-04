#include "sphFixedParticle.h"
#include "sphEqState.h"
#include "sphModel.h"
#include <fstream>
#include <iostream>

FixedParticle::FixedParticle(Model &m) : Particle(m)
{
}

void
FixedParticle::update_vars()
{
    this->getNeighbours();
    this->gradW();

    int RKstep = this->model.RKstep;

    double drho_dt = 0.0;
    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Particle *neigh = this->neighbours[i].p;
        Eigen::Vector3d u_ab = this->speed[RKstep] - neigh->speed[RKstep];
        drho_dt += this->m * u_ab.dot(this->vec_gradW[i]);
    }

    double dt = this->model.timeStep;

    if (RKstep == 0) // 1st RK step
    {
        this->rho[1] = this->rho[0] + drho_dt * dt;
        this->rho[2] = this->rho[0] + drho_dt * dt / 2.0; // prepare second step
        this->speed[1] = this->speed[0];
        this->coord[1] = this->coord[0];
        this->p[1] = this->model.eqState->pressure(this->rho[1]);
        this->c[1] = this->model.eqState->speed_of_sound(this->rho[1]);
    }
    else // 2nd RK step
    {
        this->rho[2] = this->rho[2] + drho_dt * dt / 2.0;
        this->speed[2] = this->speed[1];
        this->coord[2] = this->coord[1];
        this->p[2] = this->model.eqState->pressure(this->rho[2]);
        this->c[2] = this->model.eqState->speed_of_sound(this->rho[2]);
    }
}
