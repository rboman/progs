#ifndef SPHEREBAD_H
#define SPHEREBAD_H

#include "mailsph.h"
#include "Mesh.h"

class SphereBAD : public Mesh
{
public:
    SphereBAD();
    void build();
};

#endif