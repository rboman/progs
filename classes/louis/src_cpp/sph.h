#ifndef SPH_SPH_H
#define SPH_SPH_H

#include "sph_config.h"

class Particle;
class Neighbour;
class FixedParticle;
class MobileParticle;
class Model;
class Sorter;
class Kernel;
class CubicSplineKernel;
class QuadraticKernel;
class QuinticSplineKernel;
class DisplayHook;
class EqState;
class IdealGas;
class QIncFluid;

enum KernelKind
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
#include "Timer.h"
#include <map>

extern std::map<std::string, Timer> timers; ///< global timers

void print_banner();
void print_timers();

#endif // SPH_SPH_H
