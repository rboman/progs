// pour compiler : cc -lm *.cpp

// mkdir build
// cd build
// cmake -G "Visual Studio 14 2015 Win64" ..
// cmake --build . --config Release

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkHexahedron.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkZLibDataCompressor.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderWindow.h>
#include <vtkRenderer.h>
#include <vtkProperty.h>
#include <vtkDataSetMapper.h>
#include <vtkInteractorStyleTrackballCamera.h>


int main()
{
    FILE *fp_out, *fp_out2;

    fp_out = fopen("cyl_noe.dat", "w");
    fp_out2 = fopen("cyl_mco.dat", "w");

    fprintf(fp_out, ".DEL.*\n");

    // PARAMETRES

    int cyl_creux = 1;      // 1 si creuse, 0 si pleine
    double rint = 90.;         // rayon interne si creuse, demi-diagonale du cube central si pleine
    double rext = 100.;        // rayon externe
    double longext = 460.;     // longueur du cylindre
    int cyl_ouvert = 0;     // 1 si ouvert, 0 si ferm�
    double theta0 = 360.;      // ouverture en degr� si ouvert
    double centre[3];
    centre[0] = -100.1; // coor x du centre
    centre[1] = 0.0;    // coor y du centre
    centre[2] = -230.0; // coor z du centre
    double vec1[3];
    vec1[0] = 1.0;      // position du premier noeud
    vec1[1] = 0.0;      //
    vec1[2] = 0.0;      //
    double norm[3];
    norm[0] = 0.0;      // coor x de la normale au plan de l arc
    norm[1] = 0.0;      // coor y de la normale au plan du l arc
    norm[2] = 1.0;      // coor z de la normale au plan du c arc
    double ext[3];
    ext[0] = 0.0;       // coor x de la direction d extrusion
    ext[1] = 0.0;       // coor y de la direction d extrusion
    ext[2] = 1.0;       // coor z de la direction d extrusion
    int nbe = 24;           // nombre d elements sur l arc d un m�ridien
    int nbc = 3;            // nombre d elements sur l epaisseur
    int nbz = 15;           // nombre d elements sur la hauteur
    int noe_ini = 0;
    int maille_ini = 0;
    int nocyl = 1;

    // face externe

    int mat1rig = 1;
    int mat1def = 1;
    int loi1rig = 1;
    int loi1def = 1;
    int type1 = 1; // -1 pas contact, 0 si rigide, 1 si defo-defo, 2 si les deux




    // face interne
    int mat2rig, loi2rig, mat2def, loi2def, type2;
    if (cyl_creux == 1)
    {
        mat2rig = 3;
        mat2def = 4;
        loi2rig = 3;
        loi2def = 4;
        type2 = -1; // -1 pas contact, 0 si rigide, 1 si defo-defo, 2 si les deux
    }
    else
    {
        type2 = -1;
    }

    // FIN DES PARAMETRES




    // inversion des rayons pour rext > rint

    if (theta0 != 360.)
    {
        cyl_ouvert = 1;
    }
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
        {
            return 1;
        }
        if (nbe % 4 != 0)
        {
            nbe = (nbe / 4 + 1) * 4;
        }
    }

    double nbe2;
    if (cyl_ouvert == 1)
    {
        nbe2 = (nbe + 1);
    }
    else if (cyl_ouvert == 0)
    {
        nbe2 = nbe;
    }

    mat1def = -mat1def;
    mat2def = -mat2def;



    // norme les vecteur de definition

    double vrot[3];
    vrot[0] = vec1[1] * norm[2] - vec1[2] * norm[1];
    vrot[1] = vec1[2] * norm[0] - vec1[0] * norm[2];
    vrot[2] = vec1[0] * norm[1] - vec1[1] * norm[0];

    vec1[0] = norm[1] * vrot[2] - norm[2] * vrot[1];
    vec1[1] = norm[2] * vrot[0] - norm[0] * vrot[2];
    vec1[2] = norm[0] * vrot[1] - norm[1] * vrot[0];

    double vabs = sqrt(vec1[0] * vec1[0] + vec1[1] * vec1[1] + vec1[2] * vec1[2]);
    if (vabs < 1.E-8)
    {
        return 1;
    }
    for (int i = 0; i < 3; i++)
    {
        vec1[i] = vec1[i] / vabs;
    }
    vabs = sqrt(norm[0] * norm[0] + norm[1] * norm[1] + norm[2] * norm[2]);
    for (int i = 0; i < 3; i++)
    {
        norm[i] = norm[i] / vabs;
    }
    vabs = sqrt(ext[0] * ext[0] + ext[1] * ext[1] + ext[2] * ext[2]);
    for (int i = 0; i < 3; i++)
    {
        ext[i] = ext[i] / vabs;
    }



    // allocation du tableau tab(nz,couche,ne)

    int ***tab;
    {
        int taille1 = (nbz + 1);
        tab = (int ***)calloc(taille1, sizeof(int **));
        for (int i = 0; i < taille1; i++)
        {
            int taille2 = (nbc + 1);
            tab[i] = (int **)calloc(taille2, sizeof(int *));
            for (int j = 0; j < taille2; j++)
            {
                int taille3 = nbe2;
                tab[i][j] = (int *)calloc(taille3, sizeof(int));
            }
        }
    }
    // allocation du tableau cube(nz,lig,col)
    int ***cube;
    {
        if (cyl_creux == 0)
        {
            int taille1 = (nbz + 1);
            cube = (int ***)calloc(taille1, sizeof(int **));
            for (int i = 0; i < taille1; i++)
            {
                int taille2 = (nbe / 4 + 1);
                cube[i] = (int **)calloc(taille2, sizeof(int *));
                for (int j = 0; j < taille2; j++)
                {
                    int taille3 = (nbe / 4 + 1);
                    cube[i][j] = (int *)calloc(taille3, sizeof(int));
                }
            }
        }
    }


    // allocation du tableau coord(numint, xyz)

    int taille = (nbe2) * (nbc + 1) * (nbz + 1);
    if (cyl_creux == 0)
    {
        taille = taille + (nbe / 4 - 1) * (nbe / 4 - 1) * (nbz + 1);
    }

    double **coord = (double **)calloc(taille, sizeof(double *));
    for (int i = 0; i < taille; i++)
    {
        coord[i] = (double *)calloc(3, sizeof(double));
    }

    //  allocation du vecteur liste(numint)=numdao

    taille = (nbe2) * (nbc + 1) * (nbz + 1);
    if (cyl_creux == 0)
    {
        taille = taille + (nbe / 4 - 1) * (nbe / 4 - 1) * (nbz + 1);
    }
    int *liste = (int *)calloc(taille, sizeof(int));

    // fin des allocations

    int i, j,
        nz, couche, ne, no, nint,
        n[4], n0[4], don[10][2], level1, level2, noe, noe1, noe2, noe3, noe4, noe5, noe6, noe7, noe8,
        out, n1, n2, nbnoe, l, c, maille, ne2, cote;
    double theta;

    // remplissage de tab

    no = -1;

    for (nz = 0; nz < nbz + 1; nz++)
    {
        for (couche = 0; couche < nbc + 1; couche++)
        {
            for (ne = 0; ne < nbe2; ne++)
            {
                no = no + 1;
                tab[nz][couche][ne] = no;
            }
        }
    }

    // remplissage de cube

    if (cyl_creux == 0)
    {
        for (nz = 0; nz < nbz + 1; nz++)
        {
            for (c = 0; c < nbe / 4 + 1; c++)
            {
                for (l = 0; l < nbe / 4 + 1; l++)
                {
                    if (l == 0)
                    {
                        cube[nz][l][c] = tab[nz][nbc][c];
                    }
                    else if (l == (nbe / 4))
                    {
                        cube[nz][l][c] = tab[nz][nbc][(3 * (nbe / 4)) - c];
                    }
                    else if (c == 0)
                    {
                        cube[nz][l][c] = tab[nz][nbc][(4 * (nbe / 4)) - l];
                    }
                    else if (c == (nbe / 4))
                    {
                        cube[nz][l][c] = tab[nz][nbc][((nbe / 4)) + l];
                    }
                    else
                    {
                        no = no + 1;
                        cube[nz][l][c] = no;
                    }
                }
            }
        }
    }

    // Calcul des coordonn�es

    // le point 1

    noe1 = tab[0][0][0];
    double ray = rext;
    for (i = 0; i < 3; i++)
    {
        coord[noe1][i] = vec1[i] * ray;
    }

    // la couche exterieure du level 0

    for (ne = 1; ne < nbe2; ne++)
    {
        nint = tab[0][0][ne];
        theta = theta0 * (ne * 1.) / (nbe * 1.);
        coord[nint][0] = coord[noe1][0] * cos(theta) + (norm[1] * coord[noe1][2] - norm[2] * coord[noe1][1]) * sin(theta);
        coord[nint][1] = coord[noe1][1] * cos(theta) + (norm[2] * coord[noe1][0] - norm[0] * coord[noe1][2]) * sin(theta);
        coord[nint][2] = coord[noe1][2] * cos(theta) + (norm[0] * coord[noe1][1] - norm[1] * coord[noe1][0]) * sin(theta);
    }

    // couche interieure du level 0

    if (cyl_creux == 1)
    {
        for (ne = 0; ne < nbe2; ne++)
        {
            noe2 = tab[0][nbc][ne];
            noe1 = tab[0][0][ne];
            double ray = rint / rext;
            for (i = 0; i < 3; i++)
            {
                coord[noe2][i] = coord[noe1][i] * ray;
            }
        }
    }
    else if (cyl_creux == 0)
    {
        for (cote = 0; cote < 4; cote++)
        {
            n0[cote] = tab[0][0][cote * nbe / 4];
            n[cote] = tab[0][nbc][cote * nbe / 4];
            double ray = rint / rext;
            for (i = 0; i < 3; i++)
            {
                coord[n[cote]][i] = coord[n0[cote]][i] * ray;
            }
        }
        for (cote = 0; cote < 4; cote++)
        {
            noe1 = n[cote];
            if (cote == 3)
            {
                noe2 = n[0];
            }
            else
            {
                noe2 = n[cote + 1];
            }
            for (c = 1; c < nbe / 4; c++)
            {
                nint = tab[0][nbc][cote * nbe / 4 + c];
                for (i = 0; i < 3; i++)
                {
                    coord[nint][i] = coord[noe1][i] + (c * 1.) / ((nbe / 4) * 1.) * (coord[noe2][i] - coord[noe1][i]);
                }
            }
        }
    }

    // Toutes les couches de la face 0

    if (nbc > 1)
    {
        for (couche = 1; couche < nbc; couche++)
        {
            for (ne = 0; ne < nbe2; ne++)
            {
                noe = tab[0][couche][ne];
                noe1 = tab[0][0][ne];
                noe2 = tab[0][nbc][ne];
                for (i = 0; i < 3; i++)
                {
                    coord[noe][i] = coord[noe1][i] + (couche * 1.) / (nbc * 1.) * (coord[noe2][i] - coord[noe1][i]);
                }
            }
        }
    }

    // generation du cube central

    if (cyl_creux == 0)
    {
        for (l = 1; l < (nbe / 4); l++)
        {
            for (c = 1; c < (nbe / 4); c++)
            {
                noe = cube[0][l][c];
                noe1 = cube[0][0][c];
                noe2 = cube[0][(nbe / 4)][c];
                for (i = 0; i < 3; i++)
                {
                    coord[noe][i] = coord[noe1][i] + (l * 1.) / ((nbe / 4) * 1.) * (coord[noe2][i] - coord[noe1][i]);
                }
            }
        }
    }

    // mise � jour des positions avec le centre centre[i]

    for (couche = 0; couche < nbc + 1; couche++)
    {
        for (ne = 0; ne < nbe2; ne++)
        {
            nint = tab[0][couche][ne];
            for (i = 0; i < 3; i++)
            {
                coord[nint][i] = coord[nint][i] + centre[i];
            }
        }
    }

    if (cyl_creux == 0)
    {
        for (l = 1; l < (nbe / 4); l++)
        {
            for (c = 1; c < (nbe / 4); c++)
            {
                nint = cube[0][l][c];
                for (i = 0; i < 3; i++)
                {
                    coord[nint][i] = coord[nint][i] + centre[i];
                }
            }
        }
    }

    // extrusion de la face de base

    for (nz = 1; nz < nbz + 1; nz++)
    {
        for (couche = 0; couche < nbc + 1; couche++)
        {
            for (ne = 0; ne < nbe2; ne++)
            {
                noe1 = tab[0][couche][ne];
                noe2 = tab[nz][couche][ne];
                for (i = 0; i < 3; i++)
                {
                    coord[noe2][i] = coord[noe1][i] + (nz * 1.) / (nbz * 1.) * longext * ext[i];
                }
            }
        }
        if (cyl_creux == 0)
        {
            for (l = 1; l < (nbe / 4); l++)
            {
                for (c = 1; c < (nbe / 4); c++)
                {
                    noe1 = cube[0][l][c];
                    noe2 = cube[nz][l][c];
                    for (i = 0; i < 3; i++)
                    {
                        coord[noe2][i] = coord[noe1][i] + (nz * 1.) / (nbz * 1.) * longext * ext[i];
                    }
                }
            }
        }
    }

    // fin du remplissage du tableau des coord.

    //  impression des donn�es du probl�me

    if (cyl_creux == 1)
    {
        fprintf(fp_out, "! Cylindre %2d Creux \n\n", nocyl);
    }
    if (cyl_creux == 0)
    {
        fprintf(fp_out, "! Cylindre %2d Plein \n\n", nocyl);
    }
    fprintf(fp_out, "abrev '/rext' '%15.8E' ! Rayon Exterieur \n", rext);
    if (cyl_creux == 1)
    {
        fprintf(fp_out, "abrev '/rint' '%15.8E' ! Rayon Interieur \n", rint);
    }
    if (cyl_creux == 0)
    {
        fprintf(fp_out, "abrev '/rap' '%15.8E' ! Rapport de la diagonale du carre central au rayon exterieur \n", (rint / rext));
    }

    fprintf(fp_out, "abrev '/xcentre' '%15.8E' ! Coordonnee X du centre \n", centre[0]);
    fprintf(fp_out, "abrev '/ycentre' '%15.8E' ! Coordonnee Y du centre \n", centre[1]);
    fprintf(fp_out, "abrev '/zcentre' '%15.8E' ! Coordonnee Z du centre \n", centre[2]);

    fprintf(fp_out, "abrev '/xnormplan' '%15.8E' ! Coordonnee X de la normale au plan \n", norm[0]);
    fprintf(fp_out, "abrev '/ynormplan' '%15.8E' ! Coordonnee Y de la normale au plan \n", norm[1]);
    fprintf(fp_out, "abrev '/znormplan' '%15.8E' ! Coordonnee Z de la normale au plan \n", norm[2]);

    fprintf(fp_out, "abrev '/xextru' '%15.8E' ! Coordonnee X du vecteur d extrusion \n", ext[0]);
    fprintf(fp_out, "abrev '/yextru' '%15.8E' ! Coordonnee Y du vecteur d extrusion \n", ext[1]);
    fprintf(fp_out, "abrev '/zextru' '%15.8E' ! Coordonnee Z du vecteur d extrusion \n", ext[2]);

    fprintf(fp_out, "abrev '/nbe' '%3d' ! Nombre d'elements sur l arc de cercle \n", nbe);
    fprintf(fp_out, "abrev '/nbc' '%3d' ! Nombre de couches d'�l�ments (hors carre central si plein) \n", nbc);
    fprintf(fp_out, "abrev '/noeini' '%3d' ! Numero du noeud initial - 1\n", noe_ini);
    fprintf(fp_out, "abrev '/maiini' '%3d' ! Numero de la maille initiale - 1\n", maille_ini);
    if (type1 == 0 || type1 == 2)
    {
        fprintf(fp_out, "! Sa surface ext�rieure est la matrice de contact rigide numero %2d \n", mat1rig);
    }
    if (type1 > 0)
    {
        fprintf(fp_out, "! Sa surface ext�rieure est la matrice de contact deformable numero %2d \n", mat1def);
    }
    if (type2 == 0 || type2 == 2)
    {
        fprintf(fp_out, "! Sa surface int�rieure est la matrice de contact rigide numero %2d \n", mat2rig);
    }
    if (type2 > 0)
    {
        fprintf(fp_out, "! Sa surface int�rieure est la matrice de contact deformable numero %2d \n", mat2def);
    }
    fprintf(fp_out, "! Tout ceci pour le cylindre numero %2d  \n", nocyl);
    fprintf(fp_out, " \n");
    fprintf(fp_out, " \n");

    // -VTK----------------------------------------------------------------------------------------------
    auto ugrid = vtkSmartPointer<vtkUnstructuredGrid>::New();
    auto points = vtkSmartPointer<vtkPoints>::New();
    ugrid->SetPoints(points);
    //std::map<



    //  impression du .NOE

    fprintf(fp_out, "\n.NOEUD\n");
    noe = noe_ini;

    for (ne = 0; ne < nbe2; ne++)
    {
        for (nz = 0; nz < nbz + 1; nz++)
        {
            nint = tab[nz][0][ne];
            if (liste[nint] == 0)
            {
                noe = noe + 1;
                if (noe % 11111 == 0 || noe == 9999)
                {
                    noe = noe + 1;
                }
                if (noe > 99998)
                {
                    fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
                    return 1;
                }
                fprintf(fp_out, "I %5d  X %11.4E Y %11.4E Z %11.4E\n",
                        noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                liste[nint] = noe;
                // VTK
                points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);
            }
        }
    }
    for (ne = 0; ne < nbe2; ne++)
    {
        for (nz = 0; nz < nbz + 1; nz++)
        {
            nint = tab[nz][nbc][ne];
            if (liste[nint] == 0)
            {
                noe = noe + 1;
                if (noe % 11111 == 0 || noe == 9999)
                {
                    noe = noe + 1;
                }
                if (noe > 99998)
                {
                    fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
                    return 1;
                }
                fprintf(fp_out, "I %5d X %11.4E Y %11.4E Z %11.4E\n",
                        noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                liste[nint] = noe;
                // VTK
                points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);
            }
        }
    }
    if (nbc > 1)
    {
        for (couche = 1; couche < nbc; couche++)
        {
            for (ne = 0; ne < nbe2; ne++)
            {
                for (nz = 0; nz < nbz + 1; nz++)
                {
                    nint = tab[nz][couche][ne];
                    if (liste[nint] == 0)
                    {
                        noe = noe + 1;
                        if (noe % 11111 == 0 || noe == 9999)
                        {
                            noe = noe + 1;
                        }
                        if (noe > 99998)
                        {
                            fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
                            return 1;
                        }
                        fprintf(fp_out, "I %5d X %11.4E Y %11.4E Z %11.4E\n",
                                noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                        liste[nint] = noe;
                        // VTK
                        points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                    }
                }
            }
        }
    }

    if (cyl_creux == 0)
    {
        for (l = 1; l < (nbe / 4); l++)
        {
            for (c = 1; c < (nbe / 4); c++)
            {
                for (nz = 0; nz < nbz + 1; nz++)
                {
                    nint = cube[nz][l][c];
                    if (liste[nint] == 0)
                    {
                        noe = noe + 1;
                        if (noe % 11111 == 0 || noe == 9999)
                        {
                            noe = noe + 1;
                        }
                        if (noe > 99998)
                        {
                            fprintf(fp_out, "Erreur, numero de noeuds trop grand pour BACON\n");
                            return 1;
                        }
                        fprintf(fp_out, "I %5d X %11.4E Y %11.4E Z %11.4E\n",
                                noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                        liste[nint] = noe;
                        // VTK
                        points->InsertPoint(noe, coord[nint][0], coord[nint][1], coord[nint][2]);
                    }
                }
            }
        }
    }

    // impression du .MAI

    fprintf(fp_out, "\n.MAI\n");
    maille = maille_ini;

    for (nz = 0; nz < nbz; nz++)
    {
        for (couche = 0; couche < nbc; couche++)
        {
            for (ne = 0; ne < nbe; ne++)
            {
                maille = maille + 1;
                ne2 = ne + 1;
                if (cyl_ouvert == 0 && ne == nbe - 1)
                {
                    ne2 = 0;
                }
                noe1 = tab[nz][couche][ne];
                noe2 = tab[nz][couche][ne2];
                noe3 = tab[nz][couche + 1][ne2];
                noe4 = tab[nz][couche + 1][ne];
                noe5 = tab[nz + 1][couche][ne];
                noe6 = tab[nz + 1][couche][ne2];
                noe7 = tab[nz + 1][couche + 1][ne2];
                noe8 = tab[nz + 1][couche + 1][ne];
                fprintf(fp_out, "I %5d N %4d %4d %4d %4d 0 %4d %4d %4d %4d AT %1d\n", maille,
                        liste[noe1], liste[noe2], liste[noe3], liste[noe4],
                        liste[noe5], liste[noe6], liste[noe7], liste[noe8], nocyl);
                // VTK-
                vtkSmartPointer<vtkHexahedron> hexa = vtkSmartPointer<vtkHexahedron>::New();
                vtkIdList *ids = hexa->GetPointIds();  
                ids->SetId( 0, liste[noe1]);
                ids->SetId( 1, liste[noe2]);
                ids->SetId( 2, liste[noe3]);
                ids->SetId( 3, liste[noe4]);
                ids->SetId( 4, liste[noe5]);
                ids->SetId( 5, liste[noe6]);
                ids->SetId( 6, liste[noe7]);
                ids->SetId( 7, liste[noe8]);
                ugrid->InsertNextCell(hexa->GetCellType(), ids);
            }
        }
    }
    if (cyl_creux == 0)
    {
        for (nz = 0; nz < nbz; nz++)
        {
            for (l = 0; l < (nbe / 4); l++)
            {
                for (c = 0; c < (nbe / 4); c++)
                {
                    maille = maille + 1;
                    noe1 = cube[nz][l][c];
                    noe2 = cube[nz][l][c + 1];
                    noe3 = cube[nz][l + 1][c + 1];
                    noe4 = cube[nz][l + 1][c];
                    noe5 = cube[nz + 1][l][c];
                    noe6 = cube[nz + 1][l][c + 1];
                    noe7 = cube[nz + 1][l + 1][c + 1];
                    noe8 = cube[nz + 1][l + 1][c];
                    fprintf(fp_out, "I %5d N %4d %4d %4d %4d 0 %4d %4d %4d %4d AT %1d\n", maille,
                            liste[noe1], liste[noe2], liste[noe3], liste[noe4],
                            liste[noe5], liste[noe6], liste[noe7], liste[noe8], nocyl);
                }
            }
        }
    }

    //  impression du .MCO

    fprintf(fp_out2, "\n.MCO\n\n");

    //   couche exterieure

    //   matrice rigide

    if (type1 == 0 || type1 == 2)
    {

        for (i = 0; i < 10; i++)
        {
            for (j = 0; j < 2; j++)
            {
                don[i][j] = 0;
            }
        }
        nbnoe = nbe2 * (nbz + 1);
        n1 = noe_ini + 1;
        n2 = n1 + nbnoe - 1;
        i = -1;
        out = 0;
        do
        {
            i = i + 1;
            if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
            {
                don[i][0] = n1;
                level1 = i;
                out = 1;
            }
        } while (out == 0 && i < 10);
        out = 0;
        i = i - 1;
        do
        {
            i = i + 1;
            if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
            {
                don[i][1] = n2;
                level2 = i;
                out = 1;
            }
            else
            {
                if (i == 0)
                {
                    don[i][1] = 9998;
                    don[i + 1][0] = 10000;
                    n2 = n2 + 1;
                }
                else
                {
                    don[i][1] = (i * 11111) - 1;
                    don[i + 1][0] = (i * 11111) + 1;
                    n2 = n2 + 1;
                }
            }
        } while (out == 0 && i < 10);
        for (i = level1; i <= level2; i++)
        {
            if (don[i][0] != 0)
            {
                fprintf(fp_out2, "I %6d      J %6d    MAT %2d   LOI %2d \n",
                        don[i][0], don[i][1], mat1rig, loi1rig);
            }
        }
        fprintf(fp_out2, "\n");
    }

    //   matrice souple

    if (type1 > 0)
    {
        couche = 0;

        for (ne = 0; ne < nbe2; ne++)
        {
            noe1 = liste[tab[0][0][ne]];
            noe2 = liste[tab[nbz][0][ne]];
            fprintf(fp_out2, "I %6d  J %6d  K 1  MAT %3d  LOI %2d \n", noe1, noe2, mat1def, loi1def);
            if (ne != nbe2 - 1)
            {
                fprintf(fp_out2, "I %8d \n", -3);
            }
            if (cyl_ouvert == 0 && ne == nbe2 - 1)
            {
                fprintf(fp_out2, "I -2 \n");
            }
        }
    }
    fprintf(fp_out2, "\n");

    //   couche interieure

    //   matrice rigide

    if (type2 == 0 || type2 == 2)
    {

        for (i = 0; i < 10; i++)
        {
            for (j = 0; j < 2; j++)
            {
                don[i][j] = 0;
            }
        }
        nbnoe = nbe2 * (nbz + 1);
        n1 = liste[tab[0][nbc][0]];
        n2 = n1 + nbnoe - 1;
        i = -1;
        out = 0;
        do
        {
            i = i + 1;
            if ((i == 0 && n1 < 9999) || (i > 0 && n1 < (i * 11111)))
            {
                don[i][0] = n1;
                level1 = i;
                out = 1;
            }
        } while (out == 0 && i < 10);
        out = 0;
        i = i - 1;
        do
        {
            i = i + 1;
            if ((i == 0 && n2 < 9999) || (i > 0 && n2 < (i * 11111)))
            {
                don[i][1] = n2;
                level2 = i;
                out = 1;
            }
            else
            {
                if (i == 0)
                {
                    don[i][1] = 9998;
                    don[i + 1][0] = 10000;
                    n2 = n2 + 1;
                }
                else
                {
                    don[i][1] = (i * 11111) - 1;
                    don[i + 1][0] = (i * 11111) + 1;
                    n2 = n2 + 1;
                }
            }
        } while (out == 0 && i < 10);
        for (i = level1; i <= level2; i++)
        {
            if (don[i][0] != 0)
            {
                fprintf(fp_out2, "I %6d      J %6d    MAT %2d   LOI %2d \n",
                        don[i][0], don[i][1], mat2rig, loi2rig);
            }
        }
        fprintf(fp_out2, "\n");
    }

    //   matrice souple

    if (type2 > 0)
    {
        for (ne = 0; ne < nbe2; ne++)
        {
            noe1 = liste[tab[0][nbc][ne]];
            noe2 = liste[tab[nbz][nbc][ne]];
            fprintf(fp_out2, "I %6d  J %6d  K -1  MAT %3d  LOI %2d \n", noe2, noe1, mat2def, loi2def);
            if (ne != nbe2 - 1)
            {
                fprintf(fp_out2, "I %8d \n", -3);
            }
            if (cyl_ouvert == 0 && ne == nbe2 - 1)
            {
                fprintf(fp_out2, "I -2 \n");
            }
        }
    }
    //     fprintf(fp_out2,"I -4 \n") ;
    fprintf(fp_out2, "return \n");
    fprintf(fp_out, "return \n");
    //   fprintf(fp_out,".DES\n") ;
    //   fprintf(fp_out,"grap effa \n") ;
    //   fprintf(fp_out,"grap remp 0 \n") ;
    //   fprintf(fp_out,"grap divise cloture 4 \n") ;
    //   fprintf(fp_out,"grap sel clo 1 \n") ;
    //   fprintf(fp_out,"/g vise 1 0 0 \n") ;
    //   fprintf(fp_out,"vi \n") ;
    //   fprintf(fp_out,"grap effa 0 \n") ;
    //   fprintf(fp_out,"/g sel clo 2 \n") ;
    //   fprintf(fp_out,"/g vise 0 1 0 \n") ;
    //   fprintf(fp_out,"vi \n") ;
    //   fprintf(fp_out,"/g sel clo 3 \n") ;
    //   fprintf(fp_out,"/g vise 0 0 1 \n") ;
    //   fprintf(fp_out,"VI\n") ;
    //   fprintf(fp_out,"/g sel clo 4 \n") ;
    //   fprintf(fp_out,"grap vc visee 2 3 1\n") ;
    //   fprintf(fp_out,"grap remp 0 visee 2 3 1\n") ;
    //   fprintf(fp_out,"VI\n") ;

    std::cout << ugrid->GetNumberOfPoints() << " points and " << ugrid->GetNumberOfCells() << " cells created\n";

    // export to vtu file
    auto writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    auto compressor = vtkSmartPointer<vtkZLibDataCompressor>::New();
    writer->SetCompressor(compressor);
    writer->SetDataModeToBinary();
    writer->SetInputData(ugrid);
    writer->SetFileName("cylindre.vtu");
    writer->Write();
    std::cout << "file saved to disk.\n";

    // display

    auto meshMapper = vtkSmartPointer<vtkDataSetMapper>::New(); 
    meshMapper->SetInputData(ugrid);
    vtkSmartPointer<vtkActor> meshActor = vtkSmartPointer<vtkActor>::New();
    meshActor->SetMapper(meshMapper);

    auto gridMapper = vtkSmartPointer<vtkDataSetMapper>::New();  
    gridMapper->SetResolveCoincidentTopologyToPolygonOffset();
    gridMapper->ScalarVisibilityOff();
    gridMapper->SetInputData(ugrid);
    auto gridActor = vtkSmartPointer<vtkActor>::New();
    gridActor->GetProperty()->SetRepresentationToWireframe();
    gridActor->GetProperty()->SetColor(0.,0.,0.);
    gridActor->GetProperty()->SetAmbient(1.0);
    gridActor->GetProperty()->SetDiffuse(0.0);
    gridActor->GetProperty()->SetSpecular(0.0);
    gridActor->SetMapper(gridMapper);  

    auto ren = vtkSmartPointer<vtkRenderer>::New();
    ren->SetBackground(48./255,10./255,36./255); // unity terminal
    ren->AddActor(meshActor);
    ren->AddActor(gridActor);
    ren->ResetCamera();

    auto renWin = vtkSmartPointer<vtkRenderWindow>::New();
    renWin->SetSize(640, 480);    
    renWin->AddRenderer(ren);

    auto iren = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    iren->SetRenderWindow(renWin);

    auto style = vtkSmartPointer<vtkInteractorStyleTrackballCamera>::New();
    iren->SetInteractorStyle(style);
    iren->Initialize();
    //renWin->Render();
    iren->Start();


    return 0;
}
