#ifndef PARTICLESORT_H
#define PARTICLESORT_H

#include "sph.h"
#include "Link.h"

/// This class is able to sort the particles. A grid is generated
/// and the particles are sorted in each cell.

class ParticleSort
{
    ParticleManager &manager;

public:
    double h_max;    ///< maximum smoothing length
    double cellSize; ///< length of a side of a cube
    int nCells = 0;  ///< number of cells in the domain
    int nCellsSide;  ///< number of cells on a row

    std::vector<std::vector<Link>> storage; ///< vector of lists that contain
                                            /// the particles in a cell
public:
    ParticleSort(ParticleManager &m);

    void get_h_max();
    void setCells();
    void execute();
};

#endif // PARTICLESORT_H
