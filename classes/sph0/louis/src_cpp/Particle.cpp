#include "Particle.h"
#include "Model.h"
#include "Kernels.h"
#include <fstream>
#include <iostream>

Particle::Particle(Model &m) : model(m)
{
}

/// Loads the state of a particle from disk

void
Particle::load(std::ifstream &ufile, double h_0)
{
    double x, y, z, u_x, u_y, u_z, rho, m;
    ufile >> x >> y >> z >> u_x >> u_y >> u_z >> rho >> m;

    this->coord[0] << x, y, z;
    this->speed[0] << u_x, u_y, u_z;
    this->rho[0] = rho;
    this->m = m;
    this->h = h_0;
    this->p[0] = this->compute_pressure(rho);
    this->c[0] = this->compute_speedofsound(rho);
    this->max_mu_ab = 0.0;
}

// calculates the pressure according to the equation of state chosen.
// @param rho  : actual density

double
Particle::compute_pressure(double rho) const
{
    double pressure;
    const double idealGasCst = 8.3144621;

    switch (this->model.eqnState)
    {
    case LAW_IDEAL_GAS:
    {
        pressure = (rho / this->model.rho_0 - 1.0) *
                   idealGasCst * 293.15 / this->model.molMass; // eq (3.24)
        break;
    }
    case LAW_QINC_FLUID:
    {
        double B = this->model.c_0 * this->model.c_0 *
                   this->model.rho_0 / this->model.state_gamma; // eq (3.27)
        pressure = B * (pow(rho / this->model.rho_0, this->model.state_gamma) - 1.0);
        break;
    }
    default:
        throw std::runtime_error("Bad value for equation of state (1,2)");
    }
    return pressure;
}

/// Calculates the celerity according to the equation of state chosen.
/// The equation used is @f[c = \sqrt{\frac{dp}{d\rho}}@f]
/// @param rho  : actual density

double
Particle::compute_speedofsound(double rho) const
{
    double celerity;

    switch (this->model.eqnState)
    {
    case LAW_IDEAL_GAS:
        // 1 = considering the ideal gas law at 20 degrees C
        celerity = this->model.c_0; // eq (3.36)
        break;
    case LAW_QINC_FLUID:
        // 2 = considering a quasi-incompressible fluid
        celerity = this->model.c_0 *
                   pow(rho / this->model.rho_0, (this->model.state_gamma - 1) / 2); // eq (3.37)
        break;
    default:
        throw std::runtime_error("Bad value for equation of state (1,2)");
    }
    return celerity;
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

void
Particle::getNeighbours()
{
    int RKstep = this->model.RKstep;
    Eigen::Vector3d &xyz = this->coord[RKstep]; // position of the particle

    if (RKstep == 0)
    {
        Sorter *sorter = &this->model.sorter;
        int nx = this->model.sorter.nx; // number of cells on a row of the domain
        int cellsToCheck[27];           // number of the cells to check for the neighbours

        // calculates the number of the cell in which the particle is
        int xCell = (int)((xyz(0) - fmod(xyz(0), sorter->dx)) / sorter->dx) + 1;
        int yCell = (int)((xyz(1) - fmod(xyz(1), sorter->dx)) / sorter->dx) + 1;
        int zCell = (int)((xyz(2) - fmod(xyz(2), sorter->dx)) / sorter->dx) + 1;

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
                    Eigen::Vector3d &neighXYZ = p->coord[RKstep];
                    double r = (xyz - neighXYZ).norm();
                    if (r <= this->model.kappa * this->h)
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
            Eigen::Vector3d &neighXYZ = neigh->p->coord[RKstep];
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

    double h = this->h;
    int RKstep = this->model.RKstep;

    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        double r = this->neighbours[i].r;
        Particle *neigh = this->neighbours[i].p;
        if (r > 0.0)
            this->vec_gradW[i] = (this->coord[RKstep] - neigh->coord[RKstep]) / r * this->model.kernel->dW(r, h);
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

    int RKstep = this->model.RKstep;

    Eigen::Matrix3d M;
    M.setZero();

    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Particle *neigh = this->neighbours[i].p;
        Eigen::Vector3d &pb = neigh->coord[RKstep];
        Eigen::Vector3d &pa = this->coord[RKstep];

        double factor = neigh->m / neigh->rho[RKstep];
        M += factor * (pb - pa) * this->vec_gradW[i].transpose();
        // TODO: check if should not be transposed
    }

    Eigen::Matrix3d L = M.inverse();

    for (size_t i = 0; i < this->neighbours.size(); ++i)
        this->vec_gradW_mod[i] = L * this->vec_gradW[i];
}
