#ifndef SPH_EQSTATE_H
#define SPH_EQSTATE_H

#include "sph.h"

namespace sph {

class SPH_API EqState
{
public:
    double rho0; ///< reference density
    double c0;   ///< speed of sound  (public for the export to Fortran)

public:
#ifndef SWIG
    EqState(double _rho0, double _c0) : rho0(_rho0), c0(_c0) {}
    virtual ~EqState() = default;

    virtual double pressure(double rho) const = 0;
    virtual double speed_of_sound(double rho) const = 0;
    virtual double h_factor() const = 0;
    virtual double dt_factor() const = 0;
    friend std::ostream &operator<<(std::ostream &os, const EqState &k) 
    {
        k.write(os);
        return os;
    }
    virtual void write(std::ostream &os) const = 0;
#endif

};

class SPH_API IdealGas : public EqState
{
public:
    double M;

public:
    explicit IdealGas(double _rho0=1000., double _c0=35., double _M=28.97e-3)
        : EqState(_rho0, _c0), M(_M)
    {
    }

#ifndef SWIG
    virtual double pressure(double rho) const override
    {
        static const double RT = 8.3144621 * 293.15; // J/(mol K) * K
        return RT / M * (rho / rho0 - 1.0); // eq (3.24)
    }
    virtual double speed_of_sound(double rho) const override
    {
        // d'après la wikipedia: (https://en.wikipedia.org/wiki/Speed_of_sound) 
        // c = sqrt(gamma * R * T / M) 
        // avec gamma = adiabatic index ou coef. de Laplace
        //            = Cp/Cv = 1.4 pour l'air (gas diatomique O2, N2)
        // => fixer RT, M _et_ c0 ne semble pas avoir de sens!
        // => à réfléchir
        return c0;
    }
    virtual double h_factor() const override
    {
        return 1.1;
    }
    virtual double dt_factor() const override
    {
        return 5.0;
    }
    virtual void write(std::ostream &os) const override
    {
        os << "IdealGas M=" << M << " rho0=" << rho0 << " c0=" << c0;
    }
#endif
};

class SPH_API QincFluid : public EqState
{
public:
    double gamma;

public:
    explicit QincFluid(double _rho0=1000., double _c0=35., double _gamma=7.)
        : EqState(_rho0, _c0), gamma(_gamma) {}

#ifndef SWIG
    virtual double pressure(double rho) const override
    {
        double B = c0 * c0 * rho0 / gamma; // eq (3.27)
        return B * (pow(rho / rho0, gamma) - 1.0);
    }
    virtual double speed_of_sound(double rho) const override
    {
        return c0 * pow(rho / rho0, (gamma - 1.0) / 2.0);
    }
    virtual double h_factor() const override
    {
        return 1.02;
    }
    virtual double dt_factor() const override
    {
        return 1.0;
    }
    virtual void write(std::ostream &os) const override
    {
        os << "QincFluid gamma=" << gamma << " rho0=" << rho0 << " c0=" << c0;
    }
#endif
};

}; // namespace sph

#endif // SPH_EQSTATE_H
