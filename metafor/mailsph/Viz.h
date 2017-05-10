#ifndef VIZ_H
#define VIZ_H

#include "mailsph.h"
#include <vector>
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>

class Viz 
{
public:
    std::vector<vtkSmartPointer<vtkUnstructuredGrid>> grids;
    void display();
};

#endif