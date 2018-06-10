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
 * Splines cubiques de LITT
 */

#include "ehd.h"
#include "TdiMat.h"

//#define STANDALONE

/**
 * @brief Calcule un morceau de spline en n points x[0-(n-1)] 
 * 
 * si xi[0-1], yi[0-1], ki[0-1] sont connus
 * (renvoie y[0-(n-1)] et yp[0-(n-1)])
 */

EHD_API void ehd_spline_seg(double *xi, double *yi, double *ki,
                            double *x, double *y, double *yp, int n)
{
    // ctes du segment concerne
    double bi = yi[0];
    double hi = xi[1] - xi[0];
    double ai = (yi[1] - yi[0]) / hi - hi / 6.0 * (ki[1] + 2.0 * ki[0]);

    double k2 = ki[1] - ki[0];
    double hi2 = hi * hi;

    for (int i = 0; i < n; i++)
    {
        double t = (x[i] - xi[0]) / hi;
        double t2 = t * t;
        double t3 = t2 * t;
        yp[i] = hi * ki[0] * t + hi * k2 * t2 / 2.0 + ai;
        y[i] = hi2 * ki[0] * t2 / 2.0 + hi2 * k2 * t3 / 6.0 + hi * ai * t + bi;
    }
}

/**
 * @brief Evalue une spline en x et renvoie y (y=f(x)) et y' (derivee)
 */

EHD_API void ehd_spline_y(int nn, double *xi, double *yi, double *ki,
                         double x, double *y, double *yp)
{
    // recherche du morceau
    int i;
    for (i = 0; i < nn - 1; i++)
        if (x >= xi[i] && x <= xi[i + 1])
            break;
    if (i == nn - 1)
        throw std::runtime_error("evaluation hors de la spline!");

    // evaluation sur le segment 'i'
    ehd_spline_seg(&(xi[i]), &(yi[i]), &(ki[i]), &x, y, yp, 1);
}

/**
 *  @brief Calcule les ki (derivees 2nd)
 */

EHD_API int ehd_spline_ki(TdiMat *K, int nn, double *xi, double *yi, double *ki)
{
    int iop = 0;

    double *rhs = (double *)malloc(nn * sizeof(double));

    K->setsize(nn);

    // assemblage

    K->ass(0, 0, 1.0);
    K->ass(0, 1, 0.0);

    rhs[0] = 0.0;

    for (int i = 1; i < nn - 1; i++)
    {

        double hi1 = xi[i] - xi[i - 1];
        double hi2 = xi[i + 1] - xi[i];
        double di1 = (yi[i] - yi[i - 1]) / hi1;
        double di2 = (yi[i + 1] - yi[i]) / hi2;

        K->ass(i, i - 1, hi1);
        K->ass(i, i, 2.0 * (hi1 + hi2));
        K->ass(i, i + 1, hi2);

        rhs[i] = 6.0 * (di2 - di1);
    }
    K->ass(nn - 1, nn - 2, 0.0);
    K->ass(nn - 1, nn - 1, 1.0);

    rhs[nn - 1] = 0.0;

    /*
    K->mlab_tdi("tri.m","",TDI_A,MLAB_NEW);
    mlab_vec("tri.m","rhs",rhs,nn,MLAB_OLD);
*/

    // resolution
    iop = K->solve(rhs, ki, TDI_DO_LU | TDI_DO_SUBST);
    K->print_err(stdout, iop);
    if (iop != 0)
        goto FIN;

    free(rhs);

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}

/*********************************************************************
 *                             ROUTINE DE TEST                       *
 *********************************************************************/

#ifdef STANDALONE

int main()
{
    int iop = 0;
    int nn = 10;
    double xi[nn], yi[nn], ki[nn];
    int i;
    double x, y, yp;
    FILE *fich;
    double dx = 0.01;

    double xx[10] = {0.0, 0.5, 1.5, 3.0, 4.2, 5.1, 7.0, 7.3, 8.0, 9.1};

    // init matrice tridiag

    TdiMat K("K");

    // fichier resultat

    fich = fopen("spl.m", "w");
    if (fich == NULL)
    {
        printf("erreur fichier!\n");
        iop = 990;
        goto FIN;
    }

    for (i = 0; i < nn; i++)
    {
        xi[i] = (double)i;
        xi[i] = xx[i];
        yi[i] = (double)i;
        ki[i] = 0.0; // inutile
    }

    yi[1] = 4.0;
    yi[5] = -4.0;

    // calcul des derivees secondes (ki)

    iop = ehd_spline_ki(&K, nn, xi, yi, ki);
    if (iop != 0)
        goto FIN;

    // ecriture des poles + derivees

    for (i = 0; i < nn; i++)
    {
        fprintf(fich, "xi(%d)  = %E;\n", i + 1, xi[i]);
        fprintf(fich, "yi(%d)  = %E;\n", i + 1, yi[i]);
        fprintf(fich, "ki(%d)  = %E;\n", i + 1, ki[i]);
    }

    // evaluation sur l'ensemble de la courbe

    i = 0;
    for (x = xi[0]; x <= xi[nn - 1]; x += dx)
    {
        i++;
        iop = ehd_spline_y(nn, xi, yi, ki,
                           x, &y, &yp);
        if (iop != 0)
            goto FIN;

        fprintf(fich, "x(%d)  = %E;\n", i, x);
        fprintf(fich, "y(%d)  = %E;\n", i, y);
        fprintf(fich, "yp(%d) = %E;\n", i, yp);
    }

    // purge memoire du systeme tridiag

    iop = K->reinit();
    if (iop != 0)
        goto FIN;

    // fermeture fichier

    fclose(fich);

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}

#endif
