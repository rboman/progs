#ifndef SPHERE_H
#define SPHERE_H

#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>

class Sphere
{
public:
    vtkSmartPointer<vtkUnstructuredGrid> ugrid;
    Sphere();
    void build();
};

#endif