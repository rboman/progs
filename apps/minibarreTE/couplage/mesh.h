#ifndef MESH_H
#define MESH_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Mesh
{
public:
    int m;

public:
    Mesh();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out,
                                                 Mesh const &obj);
    void save(std::string const &filename) const;
};

#endif
