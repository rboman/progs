#ifndef SPH_SPH_H
#define SPH_SPH_H

#if defined(WIN32)
#ifdef sph_EXPORTS
#define SPH_API __declspec(dllexport)
#else
#define SPH_API __declspec(dllimport)
#endif
#else
#define SPH_API
#endif

#include "sph_config.h"
#include "sphTimer.h"
#include <Eigen/Dense>
#include <map>

namespace sph {

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
class Timer;

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

#ifndef SWIG
extern SPH_API std::map<std::string, Timer> g_timers; ///< global g_timers
#endif

SPH_API void print_banner();
SPH_API void print_timers();
SPH_API void save_timers();

}; // namespace sph



#endif // SPH_SPH_H
