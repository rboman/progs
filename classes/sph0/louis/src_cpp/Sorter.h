#ifndef SPH_SORTER_H
#define SPH_SORTER_H

#include "sph.h"
#include "Neighbour.h"

/// This class is able to sort the particles with the Linked-List method. 
/// A grid is generated and the particles are sorted in each cell.

class Sorter
{
    Model &model;
    double h_max;    ///< maximum smoothing length

public:
    double dx; ///< length of a side of a cube
    int nx;    ///< number of cells on a row

    /// vector of particles in each cell
    std::vector<std::vector<Particle *>> cells; 

public:
    Sorter(Model &m);

    void execute();

private:
    void init_cells();
    double compute_hmax();
};

#endif // SPH_SORTER_H
