#ifndef SPHERE_H
#define SPHERE_H

#include "mailsph.h"
#include "Mesh.h"

class Sphere : public Mesh
{
public:
    Sphere();
    void build();
private:
    void prog1(double **coord, int noe1, int noe2, int louc, int nbe, double *xyz, double rext);
    vtkSmartPointer<vtkUnstructuredGrid> reflect(vtkSmartPointer<vtkUnstructuredGrid> ugrid);
};

#endif