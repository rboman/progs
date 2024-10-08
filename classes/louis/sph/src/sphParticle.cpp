#include "sphParticle.h"
#include "sphModel.h"
#include "sphKernels.h"
#include "sphEqState.h"
#include <fstream>
#include <iostream>

using namespace sph;

Particle::Particle(double x, double y, double z,
                   double vx, double vy, double vz,
                   double rho0, double m0) : model(nullptr)
{
    this->coord[0] << x, y, z;
    this->speed[0] << vx, vy, vz;
    this->rho[0] = rho0;
    this->m = m0;
}

/// Loads the state of a particle from disk

void
Particle::load(std::ifstream &ufile, double h_0)
{
    assert(this->model != nullptr);

    double x, y, z, u_x, u_y, u_z, rho, m;
    ufile >> x >> y >> z >> u_x >> u_y >> u_z >> rho >> m;

    this->coord[0] << x, y, z;
    this->speed[0] << u_x, u_y, u_z;
    this->rho[0] = rho;
    this->m = m;

    this->initialise(h_0);
}

void
Particle::initialise(double h_0)
{
    assert(this->model != nullptr);

    this->h = h_0;
    this->p[0] = this->model->eqState->pressure(this->rho[0]);
    this->c[0] = this->model->eqState->speed_of_sound(this->rho[0]);
    this->max_mu_ab = 0.0;

    assert(this->m > 0.0); // TODO: do more tests
}

/// Saves the state of a particle onto disk

void
Particle::save(std::ofstream &file) const
{
    file << this->coord[0].transpose() << " "
         << this->speed[0].transpose() << " "
         << this->rho[0] << " "
         << this->p[0] << " "
         << this->m << " "
         << this->c[0] << " "
         << this->h << " "
         << this->max_mu_ab << " "
         << this->neighbours.size() << '\n';
}

/// Send the particle to a file which will be read by Louis' FORTRAN code

void
Particle::to_fortran(std::ofstream &file) const
{
    file << this->coord[0].transpose() << " "
         << this->speed[0].transpose() << " "
         << this->rho[0] << " "
         << this->m << '\n';
}

void
Particle::getNeighbours()
{
    int RKstep = this->model->RKstep;
    Eigen::Vector3d const &xyz = this->coord[RKstep]; // position of the particle

    if (RKstep == 0)
    {
        Sorter *sorter = &this->model->sorter;
        int nx = this->model->sorter.nx; // number of cells on a row of the domain
        int cellsToCheck[27];            // number of the cells to check for the neighbours

        // calculates the number of the cell in which the particle is
        int xCell = round((xyz(0) - fmod(xyz(0), sorter->dx)) / sorter->dx) + 1;
        int yCell = round((xyz(1) - fmod(xyz(1), sorter->dx)) / sorter->dx) + 1;
        int zCell = round((xyz(2) - fmod(xyz(2), sorter->dx)) / sorter->dx) + 1;

        if (xCell < 1)
            xCell = 1;
        else if (xCell > nx)
            xCell = nx;
        if (yCell < 1)
            yCell = 1;
        else if (yCell > nx)
            yCell = nx;
        if (zCell < 1)
            zCell = 1;
        else if (zCell > nx)
            zCell = nx;

        // calculates the number of the neighbouring cells
        for (int i = -1; i < 2; i++)
            for (int j = -1; j < 2; j++)
                for (int k = -1; k < 2; k++)
                {
                    if ((xCell + i > 0) &&
                        (yCell + j > 0) &&
                        (zCell + k > 0) &&
                        (xCell + i <= nx) &&
                        (yCell + j <= nx) &&
                        (zCell + k <= nx))
                    {
                        cellsToCheck[(i + 1) * 9 + (j + 1) * 3 + (k + 2) - 1] =
                            (xCell + i - 1) * nx * nx +
                            (yCell + j - 1) * nx +
                            (zCell + k) - 1;
                    }
                    else
                    {
                        cellsToCheck[(i + 1) * 9 + (j + 1) * 3 + (k + 2) - 1] = -1;
                    }
                }

        // stores the neighbours of the particle in the neighbours list.
        // First, the list is reset, then the neighbouring cells are scanned.
        // In each cell, the distance between the two particles is calculated.
        // If it is lower than the support domain and it is not the particle we
        // are working with (r>0 but here r>1E-12 for numerical errors), an  element
        // link(p+r) is added to the neighbours list.
        // For the second RK step, only the distances r are recalculated. It is assumed that
        // the neighbours remain the same between 2 RK step.

        double kappa = this->model->kernel->kappa;  

        this->neighbours.clear();
        int twice = 0; // [RB]
        for (int i = 0; i < 27; i++)
        {
            if (cellsToCheck[i] > -1)
            {
                std::vector<Particle *> *cells = &sorter->cells[cellsToCheck[i]];
                for (size_t j = 0; j < cells->size(); j++)
                {
                    Particle *p = (*cells)[j];
                    Eigen::Vector3d const &neighXYZ = p->coord[RKstep];
                    double r = (xyz - neighXYZ).norm();
                    if (r <= kappa * this->h)
                    {
                        if (p != this)
                            this->neighbours.push_back(Neighbour(p, r));
                        else
                            twice++;
                    }
                }
            }
        }
        // [RB] safeguard (useless?)
        if (twice != 1)
        {
            std::stringstream str;
            str << "safeguard activated!\n";
            str << "    one particle has been taken into account " << twice << " times\n";
            str << "    xCell = " << xCell << '\n';
            str << "    yCell = " << yCell << '\n';
            str << "    zCell = " << zCell << '\n';
            str << "    cellsToCheck = ";
            for (int i = 0; i < 27; i++)
                str << cellsToCheck[i] << ",";
            str << '\n';
            throw std::runtime_error(str.str());
        }
    }
    else
    {
        // RK step2 - same neighbours and r is updated
        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            Neighbour *neigh = &this->neighbours[i];
            Eigen::Vector3d const &neighXYZ = neigh->p->coord[RKstep];
            neigh->r = (xyz - neighXYZ).norm();
        }
    }
}

/// Creates a vector that contains the values of the gradient of the kernel
/// for each neighbour.

void
Particle::gradW()
{
    // if(this->vec_gradW.size()<this->neighbours.size())
    //     this->vec_gradW.resize(this->neighbours.size());

    if (this->neighbours.size() > 150)
        throw std::runtime_error("number of neighbours greater than expected (max 150 for vec_gradW): " + std::to_string(this->neighbours.size()));

    // double h = this->h;
    int RKstep = this->model->RKstep;

    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        double r = this->neighbours[i].r;
        Particle *neigh = this->neighbours[i].p;
        if (r > 0.0)
            this->vec_gradW[i] = (this->coord[RKstep] - neigh->coord[RKstep]) / r * this->model->kernel->dW(r, h);
        else
            this->vec_gradW[i] = Eigen::Vector3d::Zero();
    }
}

/// Takes into account the fact that the kernel may be truncated.
/// It corrects the gradient of the kernel.

void
Particle::kernel_corr()
{
    // if(this->vec_gradW_mod.size()<this->neighbours.size())
    //     this->vec_gradW_mod.resize(this->neighbours.size());

    int RKstep = this->model->RKstep;

    Eigen::Matrix3d M;
    M.setZero();

    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Particle *neigh = this->neighbours[i].p;
        Eigen::Vector3d const &pb = neigh->coord[RKstep];
        Eigen::Vector3d const &pa = this->coord[RKstep];

        double factor = neigh->m / neigh->rho[RKstep];
        M += factor * (pb - pa) * this->vec_gradW[i].transpose();
        // TODO: check if should not be transposed
    }

    Eigen::Matrix3d L = M.inverse();

    for (size_t i = 0; i < this->neighbours.size(); ++i)
        this->vec_gradW_mod[i] = L * this->vec_gradW[i];
}
