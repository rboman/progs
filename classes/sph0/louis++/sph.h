#ifndef SPH_H
#define SPH_H

class FixedParticle;
class Link;
class List;
class MobileParticle;
class ParticleManager;
class ParticleSort;

enum Kernel
{
    K_CUBIC_SPLINE = 1,
    K_QUADRATIC = 2,
    K_QUINTIC_SPLINE = 3
};

enum KernelCorrection
{
    KCORR_OFF = 0,
    KCORR_ON = 1
};

enum Law
{
    LAW_IDEAL_GAS = 1,
    LAW_QINC_FLUID = 2
};

#include <Eigen/Dense>

double eval_r(Eigen::Vector3d const &xyz, Eigen::Vector3d const &xyz2);

#endif // SPH_H
