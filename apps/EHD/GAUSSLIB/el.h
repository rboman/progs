/*
 * Librairie "el" :
 * ----------------
 *   Cette librairie defini des fct propres aux elements
 *
 *      - une fct donnant les valeurs des fcts de forme & derivees
 *        en un point
 *
 *      - une fonction calculant le determinant du jacobien, le jacobien
 *        et la dimension du problème etant donnes 
 *
 * RoBo aout 2000
 */

#ifndef __EL_H__
#define __EL_H__

/* ---------------------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* ---------------------------------------------------------------------------------- */

#define EL_MAX_DIM 3
#define EL_MAX_NODE 8

/*
 *    Quadrangle bi-lineaire
 */

#define EL_QUAD_DIM 2 // dimension selon (xi, eta)
#define EL_QUAD_NODE 4

/*
 *    Segment lineaire
 */

#define EL_LINE_DIM 1 // dimension selon (xi, eta)
#define EL_LINE_NODE 2

/*
 *    Hexaedre tri-lineaire
 */

#define EL_HEXA_DIM 3 // dimension selon (xi, eta)
#define EL_HEXA_NODE 8

/*
 *    Elem Hermite 1d a 2 noeuds et 4 ddl
 */

#define EL_HERMITE_DIM 1
#define EL_HERMITE_NODE 4

/* ---------------------------------------------------------------------------------- */

/*
 *   PROTOS
 */

// el_quad.c

void el_quad_ff(double F[][4], double *c);
int el_quad_detj(double jaco[3][3], int ndim, double *res);

// el_line.c

void el_line_ff(double F[][4], double *c);
int el_line_detj(double jaco[3][3], int ndim, double *res);

// el_hexa.c

void el_hexa_ff(double F[][4], double *c);
int el_hexa_detj(double jaco[3][3], double *res);

// el_hermite.c

void el_hermite_ff(double F[][4], double *c);

/* ---------------------------------------------------------------------------------- */

#endif

// EOF
