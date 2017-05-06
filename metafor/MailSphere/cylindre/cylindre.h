#ifndef CYLINDRE_H
#define CYLINDRE_H

#include <vtkUnstructuredGrid.h>
#include <string>

void exportvtu(vtkUnstructuredGrid *ugrid, std::string const &fname);

#endif
