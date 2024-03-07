#ifndef SPH_PARTICLESORTER_H
#define SPH_PARTICLESORTER_H

#include "sph.h"
#include "Neighbour.h"

/// This class is able to sort the particles with the Linked-List method. 
/// A grid is generated and the particles are sorted in each cell.

class ParticleSorter
{
    ParticleManager &manager;
    double h_max;    ///< maximum smoothing length

public:
    double dx; ///< length of a side of a cube
    int nx;  ///< number of cells on a row

    std::vector<std::vector<Particle *>> cells; ///< vector of lists that contain
                                          /// the particles in a cell
public:
    ParticleSorter(ParticleManager &m);

    void execute();

private:
    void init_cells();
    double compute_hmax();
};

#endif // SPH_PARTICLESORTER_H
