#include "sph.h"


double eval_r(Eigen::Vector3d const &xyz, Eigen::Vector3d const &xyz2)
{
    return (xyz-xyz2).norm();
}

