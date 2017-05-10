#ifndef CYLINDER_H
#define CYLINDER_H

#include "mailsph.h"
#include "Mesh.h"

class Cylinder : public Mesh
{
public:
    int cyl_creux;   
    double centre[3];

    Cylinder();

    void build();
private:
    void writemco(FILE *fp_out2, int type1, int noe_ini, int nbe2, int nbz, int nbc,  
                int mat1rig, int loi1rig, int mat2rig, int loi2rig, 
                int *liste, int ***tab, 
                int mat1def, int loi1def, int mat2def, int loi2def, 
                int cyl_ouvert, int type2 );
};

#endif