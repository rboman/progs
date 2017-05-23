#ifndef SPHERE_H
#define SPHERE_H

#include "mailsph.h"
#include "Mesh.h"

class Sphere : public Mesh
{
public:
    int sphere_creuse;  // 1 si creuse, 0 si pleine
    double rint;	// rayon interne si creuse, demi-diagonale du cube central si pleine
    double rext;	// rayon externe
    double centre[3];       // coor du centre
    double r[3];            // coor du reducteur
    int nbe;		    // nombre d elements sur 1/6 d un meridien
    int nbc;		    // nombre d elements sur l epaisseur
    int noe_ini;
    int maille_ini;
    int nosph;

    Sphere();

    void build();

private:
    void prog1(double **coord, int noe1, int noe2, int louc, int nbe, double *xyz, double rext);
    vtkSmartPointer<vtkUnstructuredGrid> reflect(vtkSmartPointer<vtkUnstructuredGrid> ugrid);
};

#endif