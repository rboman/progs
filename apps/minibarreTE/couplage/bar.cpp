
#include "bar.h"
#include <fstream>

Bar::Bar() 
{
    k     = 170;
    rho   = 2300;
    cv    = 711;
    E     = 1.58e11;
    alpha = 2.5e-3;
    T0    = 273;
    L     = 45e-6;    
}

COUPLAGE_API std::ostream &
operator<<(std::ostream &out, Bar const &obj)
{
    out << "bar.k     = " << obj.k  << ";\n";
    out << "bar.rho   = " << obj.rho << ";\n";
    out << "bar.cv    = " << obj.cv << ";\n";
    out << "bar.E     = " << obj.E << ";\n";
    out << "bar.alpha = " << obj.alpha << ";\n";
    out << "bar.T0    = " << obj.T0 << ";\n";
    out << "bar.L     = " << obj.L << ";\n";
    return out;
}
	
void 
Bar::save(std::string const &filename) const
{
	std::ofstream file(filename.c_str());
	file << *this;
	file.close();
}
