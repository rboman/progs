#include "Bar.h"

Bar::Bar()
{
    kappa = 170;
    rho = 2300;
    cv = 711;
    Q = 1e8;
    f = 1e5;
    E = 1.58e11;
    alpha = 2.5e-3;
    T0 = 273;
    L = 45e-6;
}

std::ostream &
operator<<(std::ostream &out, Bar const &obj)
{
    out << "kappa = " << obj.kappa << '\n';
    out << "rho   = " << obj.rho << '\n';
    out << "cv    = " << obj.cv << '\n';
    out << "Q     = " << obj.Q << '\n';
    out << "f     = " << obj.f << '\n';
    out << "E     = " << obj.E << '\n';
    out << "alpha = " << obj.alpha << '\n';
    out << "T0    = " << obj.T0 << '\n';
    out << "L     = " << obj.L << '\n';
    return out;
}
