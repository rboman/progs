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
 * Interfaces MATLAB
 * -----------------
 *   Ecriture ds un fichier :
 *     - d'un vecteur
 *     - d'une matrice skyline (cfr. skylib)
 *     - d'une matrice tridiag (cfr. tdilib)
 *
 * RoBo 27-09-00
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "mlab.h"

#undef VERBOSE
#define VERBOSE 0

/**************************************************************************/

/*
 *       Ecriture d'un vecteur dans un fichier MATLAB existant ou non
 *       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

SKY_API int mlab_vec(char *filename, char *id_txt, double *v, int n,
             int nfile, int opt)
{
    int iop = 0;
    int i;
    FILE *fich;

    if (nfile == MLAB_OLD)
        fich = fopen(filename, "a");
    else
        fich = fopen(filename, "w");
    if (fich == NULL)
        goto ERR1;

    fprintf(fich, "%s(%d)=0;\n", id_txt, n);

    for (i = 0; i < n; i++)
        fprintf(fich, "%s(%d)=%20.15E;\n", id_txt, i + 1, v[i]);

    fclose(fich);

    if (VERBOSE || (opt == MLAB_VERBOSE))
        printf("vecteur \"%s\" sauve dans le fichier matlab \"%s\" (%s)\n",
               id_txt, filename,
               (nfile == MLAB_OLD) ? "append" : "new");

FIN:
    if (iop > 900)
        printf("\n\t-->"  __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: impossible d'ouvrir le fichier \"%s\" !", filename);
    iop = 990;
    goto FIN;
}

/**************************************************************************/

/*
 *       Ecriture d'une matrice dans un fichier MATLAB existant ou non
 *       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

SKY_API int mlab_mat(char *filename, char *id_txt, double **v, int m, int n,
             int nfile, int opt)
{
    int iop = 0;
    int i, j;
    FILE *fich;

    if (nfile == MLAB_OLD)
        fich = fopen(filename, "a");
    else
        fich = fopen(filename, "w");
    if (fich == NULL)
        goto ERR1;

    fprintf(fich, "%s(%d,%d)=0;\n", id_txt, m, n);

    for (i = 0; i < m; i++)
        for (j = 0; j < n; j++)
            fprintf(fich, "%s(%d,%d)=%20.15E;\n", id_txt, i + 1, j + 1, v[i][j]);

    fclose(fich);

    if (VERBOSE || (opt == MLAB_VERBOSE))
        printf("matrice \"%s\" sauve dans le fichier matlab \"%s\" (%s)\n",
               id_txt, filename,
               (nfile == MLAB_OLD) ? "append" : "new");

FIN:
    if (iop > 900)
        printf("\n\t-->"  __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: impossible d'ouvrir le fichier \"%s\" !", filename);
    iop = 990;
    goto FIN;
}
/**************************************************************************/

SKY_API int mlab_mat_mxn(char *filename, char *id_txt, int m, int n,
                 double **v, int nfile, int opt)
{
    int iop = 0;
    int i;
    double **tmp = (double **)malloc(m * sizeof(double *));

    for (i = 0; i < m; i++)
        tmp[i] = v[i];

    iop = mlab_mat(filename, id_txt, tmp, m, n, nfile, opt);
    free(tmp);
    if (iop != 0)
        goto FIN;

FIN:
    if (iop > 900)
        printf("\n\t-->"  __FILE__
               "\n");
    return iop;
}

/**************************************************************************/

/*
 *  Ecriture d'une matrice SKYLINE dans un fichier MATLAB existant ou non
 *  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

SKY_API int mlab_sky(char *filename, char *id_txt, SkyMat *A,
             int type, int nfile, int opt)
{
    int iop = 0;
    FILE *fich;
    int *loc;
    int i, j, ic, il;
    char *name;
    char *pre1, *pre2;
    char *pre[3] = {"", "L", "U"};

    if (A->init != 1)
        goto ERR1;

    name = A->name;
    loc = A->locsit;
    pre1 = (type == SKY_LU) ? pre[1] : pre[0];
    pre2 = (type == SKY_LU) ? pre[2] : pre[0];

    if (nfile == MLAB_OLD)
        fich = fopen(filename, "a");
    else
        fich = fopen(filename, "w");
    if (fich == NULL)
        goto ERR2;

    fprintf(fich, "%s%s_nsit = %d;\n", name, id_txt, A->nsit);

    for (i = 0; i < A->nsys; i++)
    {
        for (j = loc[i]; j < loc[i + 1]; j++)
        {
            ic = i + 1;
            il = i + loc[i] - j + 1;

            if (A->sym == SKY_MAT_USYM)
            {
                if (ic == il)
                {
                    if (type == SKY_LU)
                        fprintf(fich, "%s%s%s(%d,%d) = 1.0;\n", name, id_txt, pre1, il, ic);
                    fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name, id_txt, pre2, il, ic, A->situ[j]);
                }
                else
                {
                    fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name, id_txt, pre1, ic, il, A->sitl[j]);
                    fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name, id_txt, pre2, il, ic, A->situ[j]);
                }
            }
            else
            {
                if (ic == il)
                {
                    if (type == SKY_LU)
                    {
                        fprintf(fich, "%s%sD(%d,%d) = %20.15E;\n",
                                name, id_txt, il, ic, A->sitl[j]);
                        fprintf(fich, "%s%s%s(%d,%d) = 1.0;\n",
                                name, id_txt, pre1, il, ic);
                    }
                    else
                    {
                        fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                                name, id_txt, pre1, il, ic, A->sitl[j]);
                    }
                }
                else
                {
                    fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                            name, id_txt, pre1, ic, il, A->sitl[j]);
                    if (type != SKY_LU)
                        fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n",
                                name, id_txt, pre1, il, ic, A->sitl[j]);
                }
            }
        }
    }

    fclose(fich);

    if (VERBOSE || (opt == MLAB_VERBOSE))
        printf("matrice \"%s\" sauvee dans le fichier matlab \"%s\" (%s)\n",
               name, filename,
               (nfile == MLAB_OLD) ? "append" : "new");

FIN:
    if (iop > 900)
        printf("\n\t-->"  __FILE__
               "\n");
    return iop;
ERR1:
    printf("\nerreur: la matrice n'est pas initialisee !");
    iop = 990;
    goto FIN;
ERR2:
    printf("\nerreur: impossible d'ouvrir le fichier \"%s\" !", filename);
    iop = 990;
    goto FIN;
}

/**************************************************************************/
