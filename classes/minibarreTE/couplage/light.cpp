#include "light.h"
#define _USE_MATH_DEFINES
#include <cmath>
#include <fstream>

Light::Light()
{
    Q = 1e8;
    f = 1e5;
}

COUPLAGE_API std::ostream &
operator<<(std::ostream &out, Light const &obj)
{
    out << "light.Q = " << obj.Q << ";\n";
    out << "light.f = " << obj.f << ";\n";
    return out;
}

void
Light::save(std::string const &filename) const
{
    std::ofstream file(filename.c_str());
    file << *this;
    file.close();
}

double
Light::eval(double t)
{
    if (f != 0.0)
        return Q * (1 + cos(2 * M_PI * f * t - M_PI));
    else
        return Q;
}
