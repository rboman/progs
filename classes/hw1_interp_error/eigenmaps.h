#ifndef HW1_EIGENMAPS_H
#define HW1_EIGENMAPS_H

#include "hw1.h"
#include "entities.h"
#include <Eigen/Dense>

inline Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
getLocalCoords(ElemPrp &prp)
{
    return Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>(&prp.localCoords[0], prp.localCoords.size() / 3, 3);
}

// build map: basisF(pg_i, shape_j)
inline Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>
getBasisF(ElemPrp &prp)
{
    return Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>(&prp.basisF[0], prp.weights.size(), prp.numNodes);
}

extern Eigen::IOFormat fmt;

#endif // HW1_EIGENMAPS_H
