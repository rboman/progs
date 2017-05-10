#ifndef MAILSPH_H
#define MAILSPH_H

#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <string>


void exportvtu(vtkSmartPointer<vtkUnstructuredGrid> ugrid, std::string const &fname);
void displayugrid(std::vector<vtkSmartPointer<vtkUnstructuredGrid>> grids);


vtkSmartPointer<vtkUnstructuredGrid> reflect(vtkSmartPointer<vtkUnstructuredGrid> ugrid);

#endif
