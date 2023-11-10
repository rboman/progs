#ifndef PARTICLESORT_H
#define PARTICLESORT_H

#include "sph.h"

/// This class is able to sort the particles. A grid is generated
/// and the particles are sorted in each cell.

class ParticleSort
{
public:
    double h_max;             ///< maximum smoothing length
    double cellSize;          ///< length of a side of a cube
    int nCells = 0;           ///< number of cells in the domain
    int nCellsSide;           ///< number of cells on a row
    bool init;                ///< true if the cells must be initialised
    List *storage;            ///< vector of lists that contain
                              /// the particles in a cell
    ParticleManager *manager; ///< pointer toward the object particle_manager

    ParticleSort() {}

    void get_h_max() {}
    void setCells() {}
    void particlesSort() {}
};

#endif // PARTICLESORT_H
