#include "Particle.h"
#include "ParticleManager.h"
#include <fstream>
#include <iostream>

Particle::Particle(ParticleManager &m) : manager(m)
{
}

/// Loads the state of a particle from disk

void
Particle::loadfromdisk(std::ifstream &ufile, double h_0)
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

    switch (this->manager.eqnState)
    {
    case LAW_IDEAL_GAS:
    {
        pressure = (rho / this->manager.rho_0 - 1.0) *
                   idealGasCst * 293.15 / this->manager.molMass; // eq (3.24)
        break;
    }
    case LAW_QINC_FLUID:
    {
        double B = this->manager.c_0 * this->manager.c_0 *
                   this->manager.rho_0 / this->manager.state_gamma; // eq (3.27)
        pressure = B * (pow(rho / this->manager.rho_0, this->manager.state_gamma) - 1.0);
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

    switch (this->manager.eqnState)
    {
    case LAW_IDEAL_GAS:
        // 1 = considering the ideal gas law at 20 degrees C
        celerity = this->manager.c_0; // eq (3.36)
        break;
    case LAW_QINC_FLUID:
        // 2 = considering a quasi-incompressible fluid
        celerity = this->manager.c_0 *
                   pow(rho / this->manager.rho_0, (this->manager.state_gamma - 1) / 2); // eq (3.37)
        break;
    default:
        throw std::runtime_error("Bad value for equation of state (1,2)");
    }
    return celerity;
}

/// Saves the state of a particle onto disk

void
Particle::save2disk(std::ofstream &file) const
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
    int RKstep = this->manager.RKstep;
    Eigen::Vector3d xyz = this->coord[RKstep]; // position of the particle

    if (RKstep == 0)
    {
        ParticleSorter *sorter = &this->manager.sorter;   // pointer toward the sorter machine
        int nCellsSide = this->manager.sorter.nCellsSide; // number of cells on a row of the domain
        int cellsToCheck[27];                             // number of the cells to check for the neighbours

        // calculates the number of the cell in which the particle is
        int xCell = (int)((xyz(0) - fmod(xyz(0), sorter->cellSize)) / sorter->cellSize) + 1;
        int yCell = (int)((xyz(1) - fmod(xyz(1), sorter->cellSize)) / sorter->cellSize) + 1;
        int zCell = (int)((xyz(2) - fmod(xyz(2), sorter->cellSize)) / sorter->cellSize) + 1;

        if (xCell < 1)
            xCell = 1;
        else if (xCell > nCellsSide)
            xCell = nCellsSide;
        if (yCell < 1)
            yCell = 1;
        else if (yCell > nCellsSide)
            yCell = nCellsSide;
        if (zCell < 1)
            zCell = 1;
        else if (zCell > nCellsSide)
            zCell = nCellsSide;

        // calculates the number of the neighbouring cells
        for (int i = -1; i < 2; i++)
            for (int j = -1; j < 2; j++)
                for (int k = -1; k < 2; k++)
                {
                    if ((xCell + i > 0) &&
                        (yCell + j > 0) &&
                        (zCell + k > 0) &&
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
                        cellsToCheck[(i + 1) * 9 + (j + 1) * 3 + (k + 2) - 1] = -1;
                    }
                }

        // stores the neighbours of the particle in the neighbours list.
        // First, the list is reset, then the neighbouring cells are scanned.
        // In each cell, the distance between the two particles is calculated.
        // If it is lower than the support domain and it is not the particle we
        // are working with (r>0 but here r>1E-12 for numerical errors), an  element
        // link(ptr+r) is added to the neighbours list.
        // For the second RK step, only the distances r are recalculated. It is assumed that
        // the neighbours remain the same between 2 RK step.

        this->neighbours.clear();
        int twice = 0; // [RB]
        for (int i = 0; i < 27; i++)
        {
            if (cellsToCheck[i] > -1)
            {
                std::vector<Link> *storage = &sorter->storage[cellsToCheck[i]];
                for (size_t j = 0; j < storage->size(); j++)
                {
                    Particle *neigh = (*storage)[j].ptr;
                    Eigen::Vector3d neighXYZ = neigh->coord[RKstep];
                    double r = (xyz - neighXYZ).norm();
                    if (r <= this->manager.kappa * this->h)
                    {
                        // if (r > 1e-12) // Louis
                        if (neigh != this)
                            this->neighbours.push_back(Link(neigh, r));
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
            Link *link = &this->neighbours[i];
            Eigen::Vector3d neighXYZ = link->ptr->coord[RKstep];
            link->r = (xyz - neighXYZ).norm();
        }
    }
}

/// Creates a vector that contains the values of the gradient of the kernel
/// for each neighbour.

void
Particle::gradW()
{
    if (this->neighbours.size() > 150)
        throw std::runtime_error("Error: Number of neighbours greater than expected (max 150 for vec_gradW): " + std::to_string(this->neighbours.size()));

    double h = this->h;
    int RKstep = this->manager.RKstep;

    switch (this->manager.kernelKind)
    {
    case K_CUBIC_SPLINE:
    {
        double alpha_d = 3.0 / (2.0 * M_PI * h * h * h);
        // values of alpha_d in table (2.1) p 23
        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            double r = this->neighbours[i].r;
            Particle *neigh = this->neighbours[i].ptr;
            if ((r / h >= 0.0) && (r / h < 1.0))
            {
                this->vec_gradW[i] = alpha_d / h *
                                     (1.5 * (r / h) * (r / h) - 2.0 * (r / h)) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;
            }
            else if ((r / h >= 1.0) && (r / h < 2.0))
            {
                this->vec_gradW[i] = alpha_d / h *
                                     (-0.5 * (2.0 - r / h) * (2.0 - r / h)) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;
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
        double alpha_d = 5.0 / (4.0 * M_PI * h * h * h);
        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            double r = this->neighbours[i].r;
            Particle *neigh = this->neighbours[i].ptr;
            if ((r / h >= 0.0) && (r / h <= 2.0))
                this->vec_gradW[i] = alpha_d / h *
                                     (0.375 * r / h - 0.75) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;

            else
                this->vec_gradW[i] = Eigen::Vector3d::Zero();
        }
        break;
    }
    case K_QUINTIC_SPLINE:
    {
        double alpha_d = 3.0 / (359.0 * M_PI * h * h * h);
        for (size_t i = 0; i < this->neighbours.size(); i++)
        {
            double r = this->neighbours[i].r;
            Particle *neigh = this->neighbours[i].ptr;
            if ((r / h >= 0.0) && (r / h < 1.0))
            {
                this->vec_gradW[i] = alpha_d / h *
                                     (-5.0 * pow(3.0 - r / h, 4) +
                                      30.0 * pow(2.0 - r / h, 4) -
                                      75.0 * pow(1.0 - r / h, 4)) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;
            }
            else if ((r / h >= 1.0) && (r / h < 2.0))
            {
                this->vec_gradW[i] = alpha_d / h *
                                     (-5.0 * pow(3.0 - r / h, 4) +
                                      30.0 * pow(2.0 - r / h, 4)) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;
            }
            else if ((r / h >= 2.0) && (r / h < 3.0))
            {
                this->vec_gradW[i] = alpha_d / h *
                                     (-5.0 * pow(3.0 - r / h, 4)) *
                                     (this->coord[RKstep] - neigh->coord[RKstep]) / r;
            }
            else
            {
                this->vec_gradW[i] = Eigen::Vector3d::Zero();
            }
        }
        break;
    }
    default:
        throw std::runtime_error("Bad value for kernel kind (1,2,3)");
    }
}

/// Takes into account the fact that the kernel may be truncated.
/// It corrects the gradient of the kernel.

void
Particle::kernel_corr()
{
    int RKstep = this->manager.RKstep;

    Eigen::Matrix3d M;
    M.setZero();

    for (size_t i = 0; i < this->neighbours.size(); i++)
    {
        Particle *neigh = this->neighbours[i].ptr;
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
