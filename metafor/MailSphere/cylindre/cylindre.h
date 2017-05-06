#ifndef CYLINDRE_H
#define CYLINDRE_H

#include <vtkUnstructuredGrid.h>
#include <string>


void writemco(FILE *fp_out2, int type1, int noe_ini, int nbe2, int nbz, int nbc,  
                int mat1rig, int loi1rig, int mat2rig, int loi2rig, 
                int *liste, int ***tab, 
                int mat1def, int loi1def, int mat2def, int loi2def, 
                int cyl_ouvert, int type2 );

void exportvtu(vtkUnstructuredGrid *ugrid, std::string const &fname);
void displayugrid(vtkUnstructuredGrid *ugrid);

#endif
