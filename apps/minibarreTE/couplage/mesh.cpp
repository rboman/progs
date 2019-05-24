#include "mesh.h"
#include <fstream>

Mesh::Mesh()
{
    m = 101;
}

COUPLAGE_API std::ostream &
operator<<(std::ostream &out, Mesh const &obj)
{
    out << "msh.m = " << obj.m << ";\n";
    return out;
}

void Mesh::save(std::string const &filename) const
{
    std::ofstream file(filename.c_str());
    file << *this;
    file.close();
}
