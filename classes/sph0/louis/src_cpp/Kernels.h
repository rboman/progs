#ifndef SPH_KERNELS_H
#define SPH_KERNELS_H

#include "sph.h"

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
};

class CubicSplineKernel : public Kernel
{
public:
    CubicSplineKernel();
    virtual double dW(double r, double h) const;
};

class QuadraticKernel : public Kernel
{
public:
    QuadraticKernel();
    virtual double dW(double r, double h) const;
};

class QuinticSplineKernel : public Kernel
{
public:
    QuinticSplineKernel();
    virtual double dW(double r, double h) const;
};

#endif // SPH_KERNELS_H
