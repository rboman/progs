#include "ParticleSort.h"
#include "ParticleManager.h"
#include "FixedParticle.h"
#include "Link.h"
#include <fstream>
#include <iostream>

/// Put every particle in their corresponding cell.
/// This will be useful in order to find the neighbours.

void
ParticleSort::particlesSort()
{
    if (this->init)
    {
        this->setCells();
        this->init = false;
    }

    // the lists of every cell are reset
    for (int i = 0; i < this->nCells; i++)
        this->storage[i].clear();

    int nCellsSide = this->nCellsSide; ///< number of cells on a row

    for (int i = 0; i < this->manager->numPart; i++)
    {
        FixedParticle *prt = this->manager->part[i];
        Eigen::Vector3d xyz = prt->coord[this->manager->RKstep];

        int xCell = (int)((xyz(0) - fmod(xyz(0), this->cellSize)) / this->cellSize) + 1;
        int yCell = (int)((xyz(1) - fmod(xyz(1), this->cellSize)) / this->cellSize) + 1;
        int zCell = (int)((xyz(2) - fmod(xyz(2), this->cellSize)) / this->cellSize) + 1;

        if (xCell < 1)
            xCell = 1;
        if (xCell > nCellsSide)
            xCell = nCellsSide;
        if (yCell < 1)
            yCell = 1;
        else if (yCell > nCellsSide)
            yCell = nCellsSide;
        if (zCell < 1)
            zCell = 1;
        else if (zCell > nCellsSide)
            zCell = nCellsSide;

        int part_pos = (xCell - 1) * nCellsSide * nCellsSide + (yCell - 1) * nCellsSide + zCell - 1;

        this->storage[part_pos].push_back(Link(prt, 0.0));
    }
}

// Sets the size of the cells in which the particles
// will be sorted. A cell must be cubic. The domain is assumed to be cubic.
// This routine also sets the number of cells and allocates the vector which contains
// the lists of particles.
// In ordrer to be as efficient as possible, the storage vector is not deallocated and
// reallocated at each iteration.

void
ParticleSort::setCells()
{
    // calculates the necessary number of cells on a side
    this->get_h_max();

    this->nCellsSide = 0;
    while (this->manager->dom_dim / (this->nCellsSide + 1) > this->manager->kappa * this->h_max)
        this->nCellsSide++;

    this->nCells = this->nCellsSide * this->nCellsSide * this->nCellsSide;

    // allocated with the necessary number of cells.
    this->storage.resize(this->nCells);
    this->cellSize = this->manager->dom_dim / this->nCellsSide;

    // [RB] info
    std::cout << "INFO particle_sort/setCells()\n";
    std::cout << "   .nCellsSide = " << this->nCellsSide << '\n';
    std::cout << "   .nCells     = " << this->nCells << '\n';
    std::cout << "   .cellSize   = " << this->cellSize << '\n';

    // save info to disk
    std::ofstream file("grid.out");
    // configure output stream to output double as in fortran
    file.precision(15);
    file.setf(std::ios::scientific, std::ios::floatfield);
    file << this->nCellsSide << " " << this->cellSize << '\n';
    file.close();
}

/// Finds the largest smoothing length of the particles.
/// This is useful when it is not constant over the particles.

void
ParticleSort::get_h_max()
{
    this->h_max = 0.0;
    for (int i = 0; i < this->manager->numPart; i++)
        if (this->manager->part[i]->h > this->h_max)
            this->h_max = this->manager->part[i]->h;

    // Increase of h_max in order to have a security if h changes.
    // This is done according to the equation of state used.
    if (this->manager->eqnState == LAW_IDEAL_GAS)
        this->h_max = 1.1 * this->h_max;
    else
        this->h_max = 1.02 * this->h_max;
}
