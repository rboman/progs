#ifndef EQSTATE_H
#define EQSTATE_H

#include "sph.h"

class EqState
{
public:
    const double rho0; ///< reference density
protected:
    const double c0;   ///< speed of sound
public:
    EqState(double _rho0, double _c0) : rho0(_rho0), c0(_c0) {}
    virtual ~EqState() = default;
    virtual double pressure(double rho) const = 0;
    virtual double speed_of_sound(double rho) const = 0;
    virtual double h_factor() const = 0;
    virtual double dt_factor() const = 0;

};

class IdealGas : public EqState
{
    double M;

public:
    IdealGas(double _rho0, double _c0, double _M)
        : EqState(_rho0, _c0), M(_M)
    {
    }
    virtual double pressure(double rho) const override
    {
        static const double RT = 8.3144621 * 293.15; // J/(mol K) * K
        return RT / M * (rho / rho0 - 1.0); // eq (3.24)
    }
    virtual double speed_of_sound(double rho) const override
    {
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
};

class QincFluid : public EqState
{
    int gamma;

public:
    QincFluid(double _rho0, double _c0, double _gamma)
        : EqState(_rho0, _c0), gamma(_gamma) {}

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
};

#endif // EQSTATE_H
