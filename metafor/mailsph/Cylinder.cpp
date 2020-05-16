#include "Cylinder.h"
#include "arrays.h"
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

Cylinder::Cylinder() : Mesh()
{
    cyl_creux = 1;      // 1 si creux, 0 si plein
    centre[0] = -100.1; // coor x du centre
    centre[1] = 0.0;    // coor y du centre
    centre[2] = -230.0; // coor z du centre

    rint = 90.;         // rayon interne si creuse, demi-diagonale du cube central si pleine
    rext = 100.;        // rayon externe
    longext = 460.;     // longueur du cylindre
    cyl_ouvert = 0;     // 1 si ouvert, 0 si ferme
    theta0 = 360.;      // ouverture en degre si ouvert

    ///vec1 = { 1, 0, 0};
    
    vec1[0] = 1.0;      // position du premier noeud
    vec1[1] = 0.0;      //
    vec1[2] = 0.0;      //

    norm[0] = 0.0;      // coor x de la normale au plan de l arc
    norm[1] = 0.0;      // coor y de la normale au plan de l arc
    norm[2] = 1.0;      // coor z de la normale au plan de l arc

    ext[0] = 0.0;       // coor x de la direction d extrusion
    ext[1] = 0.0;       // coor y de la direction d extrusion
    ext[2] = 1.0;       // coor z de la direction d extrusion

    nbe = 24;           // nombre d elements sur l arc d un méridien
    nbc = 3;            // nombre d elements sur l epaisseur
    nbz = 15;           // nombre d elements sur la hauteur

    noe_ini = 0;
    maille_ini = 0;
    nocyl = 1;
}

void Cylinder::build()
{
    // inversion des rayons pour rext > rint

    if (theta0 != 360.)
        cyl_ouvert = 1;

    theta0 = theta0 * atan(1.) / 45.;
    if (rext < rint)
    {
        double ray = rext;
        rext = rint;
        rint = ray;
    }

    if (cyl_creux == 0)
    {
        if (cyl_ouvert == 1)
            throw std::runtime_error("bad parameters!");

        if (nbe % 4 != 0)
            nbe = (nbe / 4 + 1) * 4;
    }

    double nbe2;
    if (cyl_ouvert == 1)
        nbe2 = (nbe + 1);
    else if (cyl_ouvert == 0)
        nbe2 = nbe;

    // norme les vecteurs de definition

    double vrot[3];
    vrot[0] = vec1[1] * norm[2] - vec1[2] * norm[1];
    vrot[1] = vec1[2] * norm[0] - vec1[0] * norm[2];
    vrot[2] = vec1[0] * norm[1] - vec1[1] * norm[0];

    vec1[0] = norm[1] * vrot[2] - norm[2] * vrot[1];
    vec1[1] = norm[2] * vrot[0] - norm[0] * vrot[2];
    vec1[2] = norm[0] * vrot[1] - norm[1] * vrot[0];

    double vabs = sqrt(vec1[0] * vec1[0] + vec1[1] * vec1[1] + vec1[2] * vec1[2]);
    if (vabs < 1.E-8)
        throw std::runtime_error("bad parameters!");

    for (int i = 0; i < 3; i++)
        vec1[i] = vec1[i] / vabs;

    vabs = sqrt(norm[0] * norm[0] + norm[1] * norm[1] + norm[2] * norm[2]);
    for (int i = 0; i < 3; i++)
        norm[i] = norm[i] / vabs;

    vabs = sqrt(ext[0] * ext[0] + ext[1] * ext[1] + ext[2] * ext[2]);
    for (int i = 0; i < 3; i++)
        ext[i] = ext[i] / vabs;

    // allocation du tableau tab(nz,couche,ne)
	int ***tab=nullptr;
    array3D_alloc(tab, nbz + 1, nbc + 1, nbe2);

    // allocation du tableau cube(nz,lig,col)
    int ***cube=nullptr;
    if (cyl_creux == 0)
        array3D_alloc(cube, nbz + 1, nbe / 4 + 1, nbe / 4 + 1);

    // allocation du tableau coord(numint, xyz)
    int taille = (nbe2) * (nbc + 1) * (nbz + 1);
    if (cyl_creux == 0)
        taille = taille + (nbe / 4 - 1) * (nbe / 4 - 1) * (nbz + 1);
    double **coord;
    array2D_alloc(coord, taille, 3);

    //  allocation du vecteur liste(numint)=numdao
    taille = (nbe2) * (nbc + 1) * (nbz + 1);
    if (cyl_creux == 0)
        taille = taille + (nbe / 4 - 1) * (nbe / 4 - 1) * (nbz + 1);
    int *liste;
    array1D_alloc(liste, taille);

    // fin des allocations


    // remplissage de tab

    int no = -1;

    for (int nz = 0; nz < nbz + 1; nz++)
        for (int couche = 0; couche < nbc + 1; couche++)
            for (int ne = 0; ne < nbe2; ne++)
            {
                no = no + 1;
                tab[nz][couche][ne] = no;
            }

    // remplissage de cube

    if (cyl_creux == 0)
        for (int nz = 0; nz < nbz + 1; nz++)
            for (int c = 0; c < nbe / 4 + 1; c++)
                for (int l = 0; l < nbe / 4 + 1; l++)
                {
                    if (l == 0)
                        cube[nz][l][c] = tab[nz][nbc][c];
                    else if (l == (nbe / 4))
                        cube[nz][l][c] = tab[nz][nbc][(3 * (nbe / 4)) - c];
                    else if (c == 0)
                        cube[nz][l][c] = tab[nz][nbc][(4 * (nbe / 4)) - l];
                    else if (c == (nbe / 4))
                        cube[nz][l][c] = tab[nz][nbc][((nbe / 4)) + l];
                    else
                    {
                        no = no + 1;
                        cube[nz][l][c] = no;
                    }
                }

    // Calcul des coordonnees

    // le point 1

    int noe1 = tab[0][0][0];
    double ray = rext;
    for (int i = 0; i < 3; i++)
        coord[noe1][i] = vec1[i] * ray;


    // la couche exterieure du level 0

    for (int ne = 1; ne < nbe2; ne++)
    {
        int nint = tab[0][0][ne];
        double theta = theta0 * (ne * 1.) / (nbe * 1.);
        coord[nint][0] = coord[noe1][0] * cos(theta) + (norm[1] * coord[noe1][2] - norm[2] * coord[noe1][1]) * sin(theta);
        coord[nint][1] = coord[noe1][1] * cos(theta) + (norm[2] * coord[noe1][0] - norm[0] * coord[noe1][2]) * sin(theta);
        coord[nint][2] = coord[noe1][2] * cos(theta) + (norm[0] * coord[noe1][1] - norm[1] * coord[noe1][0]) * sin(theta);
    }

    // couche interieure du level 0

    if (cyl_creux == 1)
    {
        for (int ne = 0; ne < nbe2; ne++)
        {
            int noe2 = tab[0][nbc][ne];
            int noe1 = tab[0][0][ne];
            double ray = rint / rext;
            for (int i = 0; i < 3; i++)
                coord[noe2][i] = coord[noe1][i] * ray;
        }
    }
    else if (cyl_creux == 0)
    {
        int n0[4], n[4];
        for (int cote = 0; cote < 4; cote++)
        {
            n0[cote] = tab[0][0][cote * nbe / 4];
            n[cote] = tab[0][nbc][cote * nbe / 4];
            double ray = rint / rext;
            for (int i = 0; i < 3; i++)
                coord[n[cote]][i] = coord[n0[cote]][i] * ray;
        }
        for (int cote = 0; cote < 4; cote++)
        {
            int noe1 = n[cote];
            int noe2;
            if (cote == 3)
                noe2 = n[0];
            else
                noe2 = n[cote + 1];

            for (int c = 1; c < nbe / 4; c++)
            {
                int nint = tab[0][nbc][cote * nbe / 4 + c];
                for (int i = 0; i < 3; i++)
                    coord[nint][i] = coord[noe1][i] + (c * 1.) / ((nbe / 4) * 1.) * (coord[noe2][i] - coord[noe1][i]);
            }
        }
    }

    // Toutes les couches de la face 0

    if (nbc > 1)
        for (int couche = 1; couche < nbc; couche++)
            for (int ne = 0; ne < nbe2; ne++)
            {
                int noe = tab[0][couche][ne];
                int noe1 = tab[0][0][ne];
                int noe2 = tab[0][nbc][ne];
                for (int i = 0; i < 3; i++)
                    coord[noe][i] = coord[noe1][i] + (couche * 1.) / (nbc * 1.) * (coord[noe2][i] - coord[noe1][i]);
            }


    // generation du cube central

    if (cyl_creux == 0)
        for (int l = 1; l < (nbe / 4); l++)
            for (int c = 1; c < (nbe / 4); c++)
            {
                int noe = cube[0][l][c];
                int noe1 = cube[0][0][c];
                int noe2 = cube[0][(nbe / 4)][c];
                for (int i = 0; i < 3; i++)
                    coord[noe][i] = coord[noe1][i] + (l * 1.) / ((nbe / 4) * 1.) * (coord[noe2][i] - coord[noe1][i]);
            }


    // mise a jour des positions avec le centre centre[i]

    for (int couche = 0; couche < nbc + 1; couche++)
        for (int ne = 0; ne < nbe2; ne++)
        {
            int nint = tab[0][couche][ne];
            for (int i = 0; i < 3; i++)
                coord[nint][i] = coord[nint][i] + centre[i];
        }


    if (cyl_creux == 0)
        for (int l = 1; l < (nbe / 4); l++)
            for (int c = 1; c < (nbe / 4); c++)
            {
                int nint = cube[0][l][c];
                for (int i = 0; i < 3; i++)
                    coord[nint][i] = coord[nint][i] + centre[i];
            }

    // extrusion de la face de base

    for (int nz = 1; nz < nbz + 1; nz++)
    {
        for (int couche = 0; couche < nbc + 1; couche++)
            for (int ne = 0; ne < nbe2; ne++)
            {
                int noe1 = tab[0][couche][ne];
                int noe2 = tab[nz][couche][ne];
                for (int i = 0; i < 3; i++)
                    coord[noe2][i] = coord[noe1][i] + (nz * 1.) / (nbz * 1.) * longext * ext[i];
            }

        if (cyl_creux == 0)
            for (int l = 1; l < (nbe / 4); l++)
                for (int c = 1; c < (nbe / 4); c++)
                {
                    int noe1 = cube[0][l][c];
                    int noe2 = cube[nz][l][c];
                    for (int i = 0; i < 3; i++)
                        coord[noe2][i] = coord[noe1][i] + (nz * 1.) / (nbz * 1.) * longext * ext[i];
                }
    }

    // fin du remplissage du tableau des coord.


    // -VTK----------------------------------------------------------------------------------------------
    auto ugrid = vtkSmartPointer<vtkUnstructuredGrid>::New();
    auto points = vtkSmartPointer<vtkPoints>::New();
    ugrid->SetPoints(points);

    //  impression du .NOE

    int noe = noe_ini;

    for (int ne = 0; ne < nbe2; ne++)
        for (int nz = 0; nz < nbz + 1; nz++)
        {
            int nint = tab[nz][0][ne];
            if (liste[nint] == 0)
            {
                noe = noe + 1;
                liste[nint] = noe;
                points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]); // shift index to start from 0
            }
        }

    for (int ne = 0; ne < nbe2; ne++)
        for (int nz = 0; nz < nbz + 1; nz++)
        {
            int nint = tab[nz][nbc][ne];
            if (liste[nint] == 0)
            {
                noe = noe + 1;
                liste[nint] = noe;
                points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]);
            }
        }

    if (nbc > 1)
        for (int couche = 1; couche < nbc; couche++)
            for (int ne = 0; ne < nbe2; ne++)
                for (int nz = 0; nz < nbz + 1; nz++)
                {
                    int nint = tab[nz][couche][ne];
                    if (liste[nint] == 0)
                    {
                        noe = noe + 1;
                        liste[nint] = noe;
                        points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]);
                    }
                }

    if (cyl_creux == 0)
        for (int l = 1; l < (nbe / 4); l++)
            for (int c = 1; c < (nbe / 4); c++)
                for (int nz = 0; nz < nbz + 1; nz++)
                {
                    int nint = cube[nz][l][c];
                    if (liste[nint] == 0)
                    {
                        noe = noe + 1;
                        liste[nint] = noe;
                        points->InsertPoint(noe-1, coord[nint][0], coord[nint][1], coord[nint][2]);
                    }
                }

    // impression du .MAI

    int maille = maille_ini;

    for (int nz = 0; nz < nbz; nz++)
        for (int couche = 0; couche < nbc; couche++)
            for (int ne = 0; ne < nbe; ne++)
            {
                maille = maille + 1;
                int ne2 = ne + 1;
                if (cyl_ouvert == 0 && ne == nbe - 1)
                    ne2 = 0;
                int noe1 = tab[nz][couche][ne];
                int noe2 = tab[nz][couche][ne2];
                int noe3 = tab[nz][couche + 1][ne2];
                int noe4 = tab[nz][couche + 1][ne];
                int noe5 = tab[nz + 1][couche][ne];
                int noe6 = tab[nz + 1][couche][ne2];
                int noe7 = tab[nz + 1][couche + 1][ne2];
                int noe8 = tab[nz + 1][couche + 1][ne];

                insertvtkcell(ugrid, liste[noe1]-1, liste[noe2] - 1, liste[noe3] - 1, liste[noe4] - 1,
                        liste[noe5] - 1, liste[noe6] - 1, liste[noe7] - 1, liste[noe8] - 1);
            }

    if (cyl_creux == 0)
        for (int nz = 0; nz < nbz; nz++)
            for (int l = 0; l < (nbe / 4); l++)
                for (int c = 0; c < (nbe / 4); c++)
                {
                    maille = maille + 1;
                    int noe1 = cube[nz][l][c];
                    int noe2 = cube[nz][l][c + 1];
                    int noe3 = cube[nz][l + 1][c + 1];
                    int noe4 = cube[nz][l + 1][c];
                    int noe5 = cube[nz + 1][l][c];
                    int noe6 = cube[nz + 1][l][c + 1];
                    int noe7 = cube[nz + 1][l + 1][c + 1];
                    int noe8 = cube[nz + 1][l + 1][c];

                    insertvtkcell(ugrid, liste[noe1] - 1, liste[noe2] - 1, liste[noe3] - 1, liste[noe4] - 1,
                            liste[noe5] - 1, liste[noe6] - 1, liste[noe7] - 1, liste[noe8] - 1);
                }

    this->ugrid = ugrid;

    // free memory

    // allocation du tableau tab(nz,couche,ne)
	array3D_free(tab, nbz + 1, nbc + 1);

    // allocation du tableau cube(nz,lig,col)
    if (cyl_creux == 0)
        array3D_free(cube, nbz + 1, nbe / 4 + 1);

    // allocation du tableau coord(numint, xyz)
    taille = (nbe2) * (nbc + 1) * (nbz + 1);
    if (cyl_creux == 0)
        taille = taille + (nbe / 4 - 1) * (nbe / 4 - 1) * (nbz + 1);
    array2D_free(coord, taille);

    //  allocation du vecteur liste(numint)=numdao
    array1D_free(liste);
}
