#include "ParticleSort.h"
#include "ParticleManager.h"
#include "FixedParticle.h"
#include "Link.h"
#include <fstream>
#include <iostream>

// put every particle in their corresponding cell.
//     This will be useful in order to find the neighbours.

void
ParticleSort::particlesSort()
{
    int i;
    int xCell, yCell, zCell;           ///< number of the cell in the x, y and z direction
    int part_pos;                      ///< absolute position of a particle
    Eigen::Vector3d xyz;               ///< position of a particle
    int nCellsSide = this->nCellsSide; ///< number of cells on a row

    FixedParticle *prt;

    // std::cout << "sorting particles" << std::endl;
    if (this->init)
    {
        this->setCells();
        this->init = false;
    }

    // the lists of every cell are reset
    for (i = 0; i < this->nCells; i++)
    {
        this->storage[i].clear();
    }

    for (i = 0; i < this->manager->numPart; i++)
    {
        prt = this->manager->part[i];
        xyz = prt->coord[this->manager->RKstep];

        xCell = (int)((xyz(0) - fmod(xyz(0), this->cellSize)) / this->cellSize) + 1;
        yCell = (int)((xyz(1) - fmod(xyz(1), this->cellSize)) / this->cellSize) + 1;
        zCell = (int)((xyz(2) - fmod(xyz(2), this->cellSize)) / this->cellSize) + 1;

        if (xCell < 1)
        {
            xCell = 1;
        }
        if (xCell > nCellsSide)
        {
            xCell = nCellsSide;
        }
        if (yCell < 1)
        {
            yCell = 1;
        }
        if (yCell > nCellsSide)
        {
            yCell = nCellsSide;
        }
        if (zCell < 1)
        {
            zCell = 1;
        }
        if (zCell > nCellsSide)
        {
            zCell = nCellsSide;
        }

        part_pos = (xCell - 1) * nCellsSide * nCellsSide + (yCell - 1) * nCellsSide + zCell;

        this->storage[part_pos].push_back(Link(prt, 0.0));
    }
}

// sets the size of the cells in which the particles
// will be sorted. A cell must be cubic. The domain is assumed to be cubic.
// This routine also sets the number of cells and allocates the vector which contains
// the lists of particles.
// In ordrer to be as efficient as possible, the storage vector is not deallocated and
// reallocated at each iteration.

void
ParticleSort::setCells()
{
    // int ios;

    // calculates the necessary number of cells on a side
    this->get_h_max();

    this->nCellsSide = 0;
    while (this->manager->dom_dim / (this->nCellsSide + 1) > this->manager->kappa * this->h_max)
    {
        this->nCellsSide = this->nCellsSide + 1;
    }

    this->nCells = this->nCellsSide * this->nCellsSide * this->nCellsSide;

    // allocated with the necessary number of cells.
    this->storage.resize(this->nCells);
    this->cellSize = this->manager->dom_dim / this->nCellsSide;

    // [RB] info
    std::cout << "INFO particle_sort/setCells()" << std::endl;
    std::cout << "   .nCellsSide = " << this->nCellsSide << std::endl;
    std::cout << "   .nCells     = " << this->nCells << std::endl;
    std::cout << "   .cellSize   = " << this->cellSize << std::endl;

    // save info 2 disk
    std::ofstream file("grid.out");
    file << this->nCellsSide << " " << this->cellSize << std::endl;
    file.close();
}

// !> particle_sort/get_h_max: finds the largest smoothing length of the particles.
// !!           This is useful when it is not constant over the particles.

void
ParticleSort::get_h_max()
{
    int i;

    this->h_max = 0;
    for (i = 0; i < this->manager->numPart; i++)
    {
        if (this->manager->part[i]->h > this->h_max)
        {
            this->h_max = this->manager->part[i]->h;
        }
    }

    // increase of h_max in order to have a security if h changes.
    // This is done according to the equation of state used.
    if (this->manager->eqnState == LAW_IDEAL_GAS)
    {
        this->h_max = 1.1 * this->h_max;
    }
    else
    {
        this->h_max = 1.02 * this->h_max;
    }
}
