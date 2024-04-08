#ifndef SPH_KERNELS_H
#define SPH_KERNELS_H

#include "sph.h"

namespace sph
{

/// This set of classes contain the kernels used in the SPH method.
/// TODO: futur: envoyer un vecteur de "r" et "h"!

class SPH_API Kernel
{
public:
    const double kappa;

public:
    Kernel(double _kappa);
    virtual ~Kernel() = default;

    virtual double dW(double r, double h) const = 0;
#ifndef SWIG
    virtual KernelKind fortran_kind() const = 0;
    friend SPH_API std::ostream &operator<<(std::ostream &os, const Kernel &k);
    virtual void write(std::ostream &os) const = 0;
#endif
};

class SPH_API CubicSplineKernel : public Kernel
{
public:
    CubicSplineKernel();

    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_CUBIC_SPLINE; }
    virtual void write(std::ostream &os) const override { os << "CubicSplineKernel"; }
#endif
};

class SPH_API QuadraticKernel : public Kernel
{
public:
    QuadraticKernel();

    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_QUADRATIC; }
    virtual void write(std::ostream &os) const override { os << "QuadraticKernel"; }
#endif
};

class SPH_API QuinticSplineKernel : public Kernel
{
public:
    QuinticSplineKernel();
    
    virtual double dW(double r, double h) const override;
#ifndef SWIG
    virtual KernelKind fortran_kind() const override { return K_QUINTIC_SPLINE; }
    virtual void write(std::ostream &os) const override { os << "QuinticSplineKernel"; }
#endif
};

}; // namespace sph

#endif // SPH_KERNELS_H
