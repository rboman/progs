#ifndef CYLINDER_H
#define CYLINDER_H

#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>

class Cylinder
{
public:
    vtkSmartPointer<vtkUnstructuredGrid> ugrid;
    int cyl_creux;   
    double centre[3];

    Cylinder();

    void build();
};

#endif