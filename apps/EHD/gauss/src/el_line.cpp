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
 * Routines de gestion d'un segment lineaire
 */

#include "el.h"


/*
 *   FONCTIONS DE FORME & DERIVEES
 */

GAUSS_API void
el_line_ff(double F[][4], double *c)
{

    /*
     * Fonctions de forme 'F[0][0-1]' et derivees 'F[1][0-1]'
     * d'un segment lineaire (4 noeuds) aux coords
     * (ksi) = (c[0])
     *
     */

    static double demi = 0.5, un = 1.0;

    double r;
    double rm, rp;
    double drm, drp;

    r = c[0];

    rp = demi * (un + r);
    drp = demi;
    rm = demi * (un - r);
    drm = -demi;

    // valeur des 2 fcts de forme lineaires

    F[0][0] = rm;
    F[1][0] = rp;

    // derviee ./. ksi

    F[0][1] = drm;
    F[1][1] = drp;
}



/*
 *   DET JACOBIEN
 */

GAUSS_API int
el_line_detj(double jaco[3][3], int ndim, double *res)
{
    int j;
    double detj;

    // si 1D, on renvoie le bon signe (int(a,b,f(x),x)<0 si f(x)>0 et b<a)

    if (ndim == 1)
    {
        *res = jaco[0][0];
    }
    else
    {
        detj = 0.0;
        for (j = 0; j < ndim; j++)
            detj = jaco[j][0] * jaco[j][0];
        detj = sqrt(detj);

        *res = detj;
    }

    return 0;
}
