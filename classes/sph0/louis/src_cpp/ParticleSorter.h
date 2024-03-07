#ifndef PARTICLESORTER_H
#define PARTICLESORTER_H

#include "sph.h"
#include "Link.h"

/// This class is able to sort the particles with the Linked-List method. 
/// A grid is generated and the particles are sorted in each cell.

class ParticleSorter
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
    ParticleSorter(ParticleManager &m);

    void get_h_max();
    void setCells();
    void execute();
};

#endif // PARTICLESORTER_H
