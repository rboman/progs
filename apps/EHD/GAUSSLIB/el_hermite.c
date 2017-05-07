/*
 * Routines de gestion d'un elem d'hermite 1d (cubique)
 *
 *        x1,dx1       x2,dx2
 *         o-------------o
 */

#include "el.h"

/* ---------------------------------------------------------------------------------- */

/*
 *   FONCTIONS DE FORME & DERIVEES
 */

void el_hermite_ff(double F[][4], double *c)
{

    /*
   * Fonctions de forme 'F[0][0-3]' et derivees 'F[1-2][0-3]' 
   * d'un element de type Hermite (2 noeuds) aux coords
   * (ksi) = (c[0])
   *
   */

    double t, t1, t2, t3;

    t = c[0];
    //t = (c[0]+1.0)/2.0; // translation -1,1 a 0,1
    t1 = 1.0 - t;
    t2 = t * t;
    t3 = 1.0 + t;

    // valeur des 4 fcts de forme (DHATT p.101)
    /*
  F[0][0] = (1.0+2.0*t)*t1*t1 ;
  F[1][0] = t*t1*t1 ;
  F[2][0] = (3.0-2.0*t)*t2 ;
  F[3][0] = -t1*t2 ;
  */
    F[0][0] = t1 * t1 * (2.0 + t) / 4.0;
    F[1][0] = ((1.0 - t2) * t1) / 4.0; // * L/2
    F[2][0] = (t3 * t3 * (2.0 - t)) / 4.0;
    F[3][0] = ((-1.0 + t2) * t3) / 4.0; // * L/2

    // derviee ./. ksi

    F[0][1] = -3.0 * (1.0 - t2) / 4.0;
    F[1][1] = -t1 * (1.0 + 3.0 * t) / 4.0;
    F[2][1] = -F[0][1];
    F[3][1] = -t3 * (1.0 - 3.0 * t) / 4.0;
    /*
  F[0][1] = (2.0*t1*t1 - 2.0*t1*(1.0+2.0*t))/2.0 ;
  F[1][1] = (t1*t1 - 2.0*t*t1)/2.0 ;
  F[2][1] = (2.0*(3.0-2.0*t)*t - 2.0*t2)/2.0 ;
  F[3][1] = (-2.0*t*t1 + t2)/2.0 ;
  */
}
