#ifndef SPH_KERNELS_H
#define SPH_KERNELS_H

#include "sph.h"

namespace sph
{

/// This set of classes contain the kernels used in the SPH method.
/// TODO: futur: envoyer un vecteur de "r" et "h"!

class Kernel
{
public:
    const double kappa;

public:
    Kernel(double _kappa);
    virtual ~Kernel() = default;

    virtual double dW(double r, double h) const = 0;
#ifndef SWIG
    virtual KernelKind fortran_kind() const = 0;
#endif
};

class CubicSplineKernel : public Kernel
{
public:
    CubicSplineKernel();

    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_CUBIC_SPLINE; }
#endif
};

class QuadraticKernel : public Kernel
{
public:
    QuadraticKernel();

    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_QUADRATIC; }
#endif
};

class QuinticSplineKernel : public Kernel
{
public:
    QuinticSplineKernel();
    
    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_QUINTIC_SPLINE; }
#endif
};

}; // namespace sph

#endif // SPH_KERNELS_H
