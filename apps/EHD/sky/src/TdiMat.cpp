/*
 *   Copyright 2000-2017 Romain Boman
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

/*
 * TdiLib
 * ======
 *   - Gestion de Matrices TriDIagonales
 *   - Solver non symetrique (sans gestion de pivots nuls)
 *
 * RoBo 27-09-00
 *
 * Remarques: - l'utilisation est identique a "skylib"
 *            - la matrice est ecrasee par sa decomposition LU !!
 */

/**************************************************************************
                                    Headers
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "TdiMat.h"
#include "mlab.h"

/**************************************************************************
                                 Macros locales
 **************************************************************************/

// Tolerance relative aux pivots nuls
#define TDI_EPS 1.0e-18

// Compile la routine "main"
//#define TDI_STANDALONE

// Infos de debug
#undef VERBOSE
#define VERBOSE 0

TdiMat::TdiMat(std::string const &_name) : name(_name)
{
    this->nsys = 0;
    this->nsys_a = 0;
    for (int i = 0; i < 3; i++)
        this->s[i] = NULL;
}

TdiMat::~TdiMat()
{
}

/**************************************************************************/

/*
 *              Reinit la matrice utilisee (libere la memoire)
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int TdiMat::reinit()
{
    int iop = 0;
    int i, mem;

    // PURGE LA MEMOIRE

    // s[]
    mem = 0;
    if (this->nsys_a > 0)
    {
        for (i = 0; i < 3; i++)
            free(this->s[i]);
        mem = 3 * this->nsys_a;
        this->nsys_a = 0;
    }

#if VERBOSE
    printf("liberation de %d doubles\n", mem);
#endif
    // INITIALISATION

    this->nsys = 0;
    this->nsys_a = 0;
    for (int i = 0; i < 3; i++)
        this->s[i] = NULL;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: la matrice n'est pas initialisee !");
    iop = 990;
    goto FIN;
}

/**************************************************************************/

/*
 *              Definit la taille de la matrice
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int TdiMat::setsize(int nsys)
{
    int iop = 0;
    int i;

    if (nsys > 0)
    {
        if (this->nsys_a < nsys)
        {
            for (i = 0; i < 3; i++)
            {
                this->s[i] = (double *)realloc(this->s[i], nsys * sizeof(double));
                if (this->s[i] == NULL)
                    goto ERR2;
                this->nsys_a = nsys;
            }
        }
        this->nsys = nsys;
    }
    else
        goto ERR3;

    // Init

    iop = this->fill(0.0);
    if (iop != 0)
        goto FIN;

/***/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: la matrice n'est pas initialisee !");
    iop = 990;
    goto FIN;
ERR2:
    printf("\nerreur: pas assez de memoire !");
    iop = 990;
    goto FIN;
ERR3:
    printf("\nerreur: taille matrice <0 !");
    iop = 990;
    goto FIN;
}

/**************************************************************************
                  Routines de manipulation de la matrice
 **************************************************************************/

/*
 *              Assemble un element dans la matrice TRIDIAG
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int TdiMat::ass(int i, int j, double val)
{
    int iop = 0;

    if (abs(i - j) > 1)
        goto ERR1;

    this->s[1 + i - j][i] += val;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: (i,j)=(%d,%d) hors des 3 diagonales !", i, j);
    iop = 990;
    goto FIN;
}

/**************************************************************************/

/*
 *              Assemble un element dans la matrice TRIDIAG
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int TdiMat::set(int i, int j, double val)
{
    int iop = 0;

    if (abs(i - j) > 1)
        goto ERR1;

    this->s[1 + i - j][i] = val;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: (i,j)=(%d,%d) hors des 3 diagonales !", i, j);
    iop = 990;
    goto FIN;
}

/**************************************************************************/

/*
 *              Remplit un matrice TRIDIAG avec "val"
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int TdiMat::fill(double val)
{
    int iop = 0;
    int i, j;

    for (i = 0; i < 3; i++)
        for (j = 0; j < this->nsys; j++)
            this->s[i][j] = val;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: la matrice n'est pas initialisee !");
    iop = 990;
    goto FIN;
}

/**************************************************************************
                                  Solveur
 **************************************************************************/

int TdiMat::solve(double *q, double *x, int type)
{
    int iop = TDI_ERR_OK;
    double *s[3];
    int i, nn;
    double prec;

    // Raccourcis

    for (i = 0; i < 3; i++)
        s[i] = this->s[i];
    nn = this->nsys;

    // Decomposition LU

    if ((type & TDI_DO_LU) == TDI_DO_LU)
    {

        prec = 0.0;
        for (i = 0; i < nn; i++)
        {
            prec += fabs(s[1][i]);
        }
        prec = prec / nn * TDI_EPS;

        if (fabs(s[1][0]) < prec)
            goto ERR1;
        for (i = 1; i < nn; i++)
        {
            s[2][i] /= s[1][i - 1];
            s[1][i] -= s[2][i] * s[0][i - 1];
            if (fabs(s[1][i]) < prec)
                goto ERR1;
        }
    }

    if ((type & TDI_DO_SUBST) == TDI_DO_SUBST)
    {

        // Subst avant

        x[0] = q[0];
        for (i = 1; i < nn; i++)
            x[i] = q[i] - s[2][i] * x[i - 1];

        // Subst arriere (on suppose qu'il n'y a pas de pivots nuls)

        x[nn - 1] = x[nn - 1] / s[1][nn - 1];
        for (i = nn - 2; i >= 0; i--)
            x[i] = (x[i] - s[0][i] * x[i + 1]) / s[1][i];
    }

/***/

FIN:
    return iop;

ERR1:
    iop = SKY_ERR_PIV0;
    goto FIN;
}

/**************************************************************************/

/*
 *             Traitement des codes renvoyes par les solveurs
 *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

void TdiMat::print_err(FILE *fich, int code)
{
    char *err[3] = {"ok", "pivot nul", "code inconnu"};
    char *e;

    switch (code)
    {
    case TDI_ERR_OK:
        e = err[0];
        break;
    case TDI_ERR_PIV0:
        e = err[1];
        break;
    default:
        e = err[2];
        break;
    }
    fprintf(fich, "code solver: %s\n", e);
}

/**************************************************************************
                       Routines de test de la librairie
 **************************************************************************/

int TdiMat::test()
{
    int iop = 0;

    double A[5][5] = {{1, 3, 0, 0, 0},
                      {1, 2, 0, 0, 0},
                      {0, 2, 3, 0, 0},
                      {0, 0, 11, 4, 0},
                      {0, 0, 0, 0, 7}};

    double q[5] = {1, 1, 1, 1, 1};
    double x[5], xs[5];
    int n = 5;

    // initialisation

    for (int i = 0; i < n; i++)
    {
        x[i] = 0;
        xs[i] = 0;
    }

    TdiMat K("K");

    iop = K.setsize(n);
    if (iop != 0)
        goto FIN;

    // assemblage Ks

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            if (A[i][j] != 0.0)
            {
                iop = K.ass(i, j, A[i][j]);
                if (iop != 0)
                    goto FIN;
            }
        }
    }

    K.mlab("tri.m", "1", TDI_A, MLAB_NEW, MLAB_VERBOSE);

    // resolution

    iop = K.solve(q, x, TDI_DO_LU | TDI_DO_SUBST);
    K.print_err(stdout, iop);

    // verification matlab

    K.mlab("tri.m", "2", TDI_LU, MLAB_OLD, MLAB_VERBOSE);
    mlab_vec("tri.m", "q", q, n, MLAB_OLD, MLAB_VERBOSE);
    mlab_vec("tri.m", "x", x, n, MLAB_OLD, MLAB_VERBOSE);

    // purge memoire (facultatif)

    iop = K.reinit();
    if (iop != 0)
        goto FIN;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}

/*
 *  Ecriture d'une matrice TRIDIAG dans un fichier MATLAB existant ou non
 *  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

SKY_API int TdiMat::mlab(char *filename, char *id_txt, int type, int nfile, int opt)
{
    int iop = 0;

    char *pre[3] = {"", "L", "U"};

    char *pre1 = (type == TDI_LU) ? pre[1] : pre[0];
    char *pre2 = (type == TDI_LU) ? pre[2] : pre[0];

    FILE *fich;
    if (nfile == MLAB_OLD)
        fich = fopen(filename, "a");
    else
        fich = fopen(filename, "w");
    if (fich == NULL)
        throw std::runtime_error("impossible d'ouvrir le fichier "+name);

    for (int i = 0; i < this->nsys; i++)
    {

        int i1 = i + 1;
        if (type == TDI_LU)
            fprintf(fich, "%s%s%s(%d,%d) = 1.0;\n",
                    name.c_str(), id_txt, pre1, i1, i1);
        fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                name.c_str(), id_txt, pre2, i1, i1, this->s[1][i]);
        if (i != 0)
            fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                    name.c_str(), id_txt, pre1, i1, i1 - 1, this->s[2][i]);
        if (i != this->nsys - 1)
            fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                    name.c_str(), id_txt, pre2, i1, i1 + 1, this->s[0][i]);
    }

    fclose(fich);

    if (VERBOSE || (opt == MLAB_VERBOSE))
        printf("matrice \"%s\" sauvee dans le fichier matlab \"%s\" (%s)\n",
               name.c_str(), filename,
               (nfile == MLAB_OLD) ? "append" : "new");

    return iop;
}
