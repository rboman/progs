#ifndef MESH_H
#define MESH_H

#include "mailsph.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>

class Mesh
{
public:
    vtkSmartPointer<vtkUnstructuredGrid> ugrid;

    Mesh();

protected:
    void insertvtkcell(vtkUnstructuredGrid *ugrid, int id1, int id2, int id3, int id4, int id5, int id6, int id7, int id8);
    void insertvtkcell(vtkUnstructuredGrid *ugrid, int id1, int id2, int id3, int id4);
};

#endif