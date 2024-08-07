#include "sphSorter.h"
#include "sphModel.h"
#include "sphFixedParticle.h"
#include "sphNeighbour.h"
#include "sphEqState.h"
#include "sphKernels.h"
#include <fstream>
#include <iostream>

using namespace sph;

Sorter::Sorter(Model &m) : model(m)
{
    this->dx = 0.0;
    this->nx = 0;
}

/// Put every particle in their corresponding cell.
/// This will be useful in order to find the neighbours.

void
Sorter::execute()
{
    if (this->cells.size() == 0)
        this->init_cells();

    // the lists of every cell are reset
    for(auto &cell : this->cells)
        cell.clear();

    for(auto const &p : this->model.particles)
    {
        Eigen::Vector3d const &pos = p->coord[this->model.RKstep];

        int ix = round((pos(0) - fmod(pos(0), this->dx)) / this->dx) + 1;
        int iy = round((pos(1) - fmod(pos(1), this->dx)) / this->dx) + 1;
        int iz = round((pos(2) - fmod(pos(2), this->dx)) / this->dx) + 1;

        if (ix < 1)
            ix = 1;
        else if (ix > nx)
            ix = nx;
        if (iy < 1)
            iy = 1;
        else if (iy > nx)
            iy = nx;
        if (iz < 1)
            iz = 1;
        else if (iz > nx)
            iz = nx;

        int idx = (ix - 1) * nx * nx + (iy - 1) * nx + iz - 1;

        this->cells[idx].push_back(p.get());
    }

}

// Sets the size of the cells in which the particles
// will be sorted. A cell must be cubic. The domain is assumed to be cubic.
// This routine also sets the number of cells and allocates the vector which contains
// the lists of particles.
// In ordrer to be as efficient as possible, the cells vector is not deallocated and
// reallocated at each iteration.

void
Sorter::init_cells()
{
    // calculates the necessary number of cells on a side
    double hmax = this->compute_hmax();
    this->nx = 0;
    while (this->model.dom_dim / (this->nx + 1) > this->model.kernel->kappa * hmax)
        this->nx++;

    int nCells = this->nx * this->nx * this->nx;

    // allocated with the necessary number of cells.
    this->cells.resize(nCells);
    this->dx = this->model.dom_dim / this->nx;

    // [RB] info
    std::cout << "INFO:\n";
    std::cout << "   .nx      = " << this->nx << '\n';
    std::cout << "   .nCells  = " << this->cells.size() << '\n';
    std::cout << "   .dx      = " << this->dx << '\n';

    // save info to disk
    std::ofstream file("grid.out");
    file << this->nx << " " << this->dx << '\n';
    file.close();
}

/// Finds the largest smoothing length of the particles.
/// This is useful when it is not constant over the particles.

double 
Sorter::compute_hmax()
{
    // TODO: reduction openmp??
    double hmax = 0.0;
    for(auto const &p : this->model.particles)
        if (p->h > hmax)
            hmax = p->h;
    hmax *= this->model.eqState->h_factor();
    return hmax;
}
