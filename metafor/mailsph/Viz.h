#ifndef VIZ_H
#define VIZ_H

#include "mailsph.h"


class Viz 
{
public:
    std::vector<vtkSmartPointer<vtkUnstructuredGrid>> grids;
    void display();
};

#endif