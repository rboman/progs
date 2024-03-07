#ifndef SPH_H
#define SPH_H

class FixedParticle;
class Link;
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
#include "Timer.h"
#include <map>

extern std::map<std::string, Timer> timers; ///< global timers

void print_banner();
void print_timers();

#endif // SPH_H
