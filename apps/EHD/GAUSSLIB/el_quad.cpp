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
 * Routines de gestion d'un quad 2d bi-lineaire
 */

#include "el.h"

/* ---------------------------------------------------------------------------------- */

/*
 *   FONCTIONS DE FORME & DERIVEES
 */

void el_quad_ff(double F[][4], double *c)
{

    /*
   * Fonctions de forme 'F[0][0-3]' et derivees 'F[1-2][0-3]' 
   * d'un quad bi-lineaire (4 noeuds) aux coords
   * (ksi,eta) = (c[0],c[1])
   *
   */

    static double demi = 0.5, un = 1.0;

    double r, s;
    double rm, rp, sm, sp;
    double drm, drp, dsm, dsp;

    r = c[0];
    s = c[1];

    rp = demi * (un + r);
    drp = demi;
    rm = demi * (un - r);
    drm = -demi;

    sp = demi * (un + s);
    dsp = demi;
    sm = demi * (un - s);
    dsm = -demi;

    // valeur des 4 fcts de forme lineaires

    F[0][0] = rm * sm;
    F[1][0] = rp * sm;
    F[2][0] = rp * sp;
    F[3][0] = rm * sp;

    // derviee ./. ksi

    F[0][1] = drm * sm;
    F[1][1] = drp * sm;
    F[2][1] = drp * sp;
    F[3][1] = drm * sp;

    // derviee ./. eta

    F[0][2] = rm * dsm;
    F[1][2] = rp * dsm;
    F[2][2] = rp * dsp;
    F[3][2] = rm * dsp;
}

/* ---------------------------------------------------------------------------------- */

/*
 *   DET JACOBIEN 
 */

int el_quad_detj(double jaco[3][3], int ndim, double *res)
{
    double detj = 0.0;
    double ds[3];

    if (ndim == 2)
        detj = jaco[0][0] * jaco[1][1] - jaco[1][0] * jaco[0][1];
    else if (ndim == 3)
    {
        ds[0] = jaco[1][0] * jaco[2][1] - jaco[1][1] * jaco[2][0];
        ds[1] = jaco[2][0] * jaco[0][1] - jaco[0][0] * jaco[2][1];
        ds[2] = jaco[0][0] * jaco[1][1] - jaco[1][0] * jaco[0][1];
        detj = ds[0] * ds[0] + ds[1] * ds[1] + ds[2] * ds[2];
        detj = sqrt(detj);
    }

    *res = detj;

    return 0;
}
