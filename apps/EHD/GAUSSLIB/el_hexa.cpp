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

void el_hexa_ff(double F[][4], double *c)
{

    /*
   * Fonctions de forme 'F[0][0-7]' et derivees 'F[1-3][0-7]' 
   * d'un hexa tri-lineaire (8 noeuds) aux coords
   * (ksi,eta,zeta) = (c[0],c[1],c[2])
   *
   */

    static double demi = 0.5, un = 1.0;

    double r, s, t;
    double rm, rp, sm, sp, tm, tp;
    double drm, drp, dsm, dsp, dtm, dtp;

    r = c[0];
    s = c[1];
    t = c[2];

    rp = demi * (un + r);
    drp = demi;
    rm = demi * (un - r);
    drm = -demi;

    sp = demi * (un + s);
    dsp = demi;
    sm = demi * (un - s);
    dsm = -demi;

    tp = demi * (un + t);
    dtp = demi;
    tm = demi * (un - t);
    dtm = -demi;

    // valeur des 8 fcts de forme lineaires

    F[0][0] = rm * sm * tm;
    F[1][0] = rp * sm * tm;
    F[2][0] = rp * sp * tm;
    F[3][0] = rm * sp * tm;
    F[4][0] = rm * sm * tp;
    F[5][0] = rp * sm * tp;
    F[6][0] = rp * sp * tp;
    F[7][0] = rm * sp * tp;

    // derviee ./. ksi

    F[0][1] = drm * sm * tm;
    F[1][1] = drp * sm * tm;
    F[2][1] = drp * sp * tm;
    F[3][1] = drm * sp * tm;
    F[4][1] = drm * sm * tp;
    F[5][1] = drp * sm * tp;
    F[6][1] = drp * sp * tp;
    F[7][1] = drm * sp * tp;

    // derviee ./. eta

    F[0][2] = rm * dsm * tm;
    F[1][2] = rp * dsm * tm;
    F[2][2] = rp * dsp * tm;
    F[3][2] = rm * dsp * tm;
    F[4][2] = rm * dsm * tp;
    F[5][2] = rp * dsm * tp;
    F[6][2] = rp * dsp * tp;
    F[7][2] = rm * dsp * tp;

    // derviee ./. zeta

    F[0][3] = rm * sm * dtm;
    F[1][3] = rp * sm * dtm;
    F[2][3] = rp * sp * dtm;
    F[3][3] = rm * sp * dtm;
    F[4][3] = rm * sm * dtp;
    F[5][3] = rp * sm * dtp;
    F[6][3] = rp * sp * dtp;
    F[7][3] = rm * sp * dtp;
}

/* ---------------------------------------------------------------------------------- */

/*
 *   DET JACOBIEN 
 */

int el_hexa_detj(double jaco[3][3], double *res)
{

    double detj;

    detj = 0.0;
    detj += jaco[0][0] * jaco[1][1] * jaco[2][2];
    detj += jaco[0][1] * jaco[1][2] * jaco[2][0];
    detj += jaco[0][2] * jaco[1][0] * jaco[2][1];
    detj -= jaco[2][0] * jaco[1][1] * jaco[0][2];
    detj -= jaco[2][1] * jaco[1][2] * jaco[0][0];
    detj -= jaco[2][2] * jaco[1][0] * jaco[0][1];

    *res = detj;

    return 0;
}
