#include "sphKernels.h"

    Kernel::Kernel(double _kappa) : kappa(_kappa)
    {
    }

CubicSplineKernel::CubicSplineKernel() : Kernel(2)
{
}

double
CubicSplineKernel::dW(double r, double h) const
{
    // values of alpha_d in table (2.1) p 23
    double alpha_d = 3.0 / (2.0 * M_PI * h * h * h);
    if (r / h < 1.0)
        return alpha_d / h * (1.5 * (r / h) * (r / h) - 2.0 * (r / h));
    else if (r / h < 2.0)
        return alpha_d / h * (-0.5 * (2.0 - r / h) * (2.0 - r / h));
    else
        return 0.0;
}

// -----------------------------------------------------------------------------

QuadraticKernel::QuadraticKernel() : Kernel(2)
{
}
double
QuadraticKernel::dW(double r, double h) const
{
    // values of alpha_d in table (2.1) p 23
    double alpha_d = 5.0 / (4.0 * M_PI * h * h * h);
    if (r / h < 2.0)
        return alpha_d / h * (0.375 * r / h - 0.75);
    else
        return 0.0;
}

// -----------------------------------------------------------------------------

QuinticSplineKernel::QuinticSplineKernel() : Kernel(3)
{
}

double
QuinticSplineKernel::dW(double r, double h) const
{
    // values of alpha_d in table (2.1) p 23
    double alpha_d = 3.0 / (359.0 * M_PI * h * h * h);
    if (r / h < 1.0)
        return alpha_d / h *
               (-5.0 * pow(3.0 - r / h, 4) +
                30.0 * pow(2.0 - r / h, 4) -
                75.0 * pow(1.0 - r / h, 4));
    else if (r / h < 2.0)
        return alpha_d /
               h *
               (-5.0 * pow(3.0 - r / h, 4) +
                30.0 * pow(2.0 - r / h, 4));
    else if (r / h < 3.0)
        return alpha_d / h *
               (-5.0 * pow(3.0 - r / h, 4));
    else
        return 0.0;
}