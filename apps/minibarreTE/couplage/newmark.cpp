#include "newmark.h"
#include <fstream>

Newmark::Newmark()
{
    double tfin = 6e-4;
    dt = 1e-6;
    nt = floor(tfin / dt);

    gamma = 0.501;
    beta = 0.255;
}

COUPLAGE_API std::ostream &
operator<<(std::ostream &out, Newmark const &obj)
{
    out << "nmark.nt    = " << obj.nt << ";\n";
    out << "nmark.dt    = " << obj.dt << ";\n";
    out << "nmark.gamma = " << obj.gamma << ";\n";
    out << "nmark.beta  = " << obj.beta << ";\n";
    return out;
}

void Newmark::save(std::string const &filename) const
{
    std::ofstream file(filename.c_str());
    file << *this;
    file.close();
}
