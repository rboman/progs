/*
 * Librairie "Gauss" : (necessite la lib "el")
 * -------------------
 *
 * Cette librairie permet d'integrer n'importe quelle fonction
 * sur n'importe quel type d'element defini dans la lib "el".
 *      a/  ligne (1d/2d/3d)
 *      b/  quad  (2d/3d)
 *      c/  hexa  (3d)
 *
 * On peut utiliser de 1 a 10 pts de Gauss. Les donnees importantes
 * ne dependant pas de la fct integree (val des fct de forme, positions
 * et poids de Gauss) sont calcules lors du premier appel et conserves 
 * en memoire pour accelerer les integrations suivantes. 
 *
 * Il existe 2 manieres pour appeler les routines:
 *
 *      a/ appel des routines specifiques a la surface d'integration
 *         (gauss_line,gauss_quad,...)
 *
 *      b/ appel a la routine generique (gauss_generic) qui est un peu
 *         plus lente mais plus generale.
 *
 * Attention: la routine "gauss_common_init()" doit etre appelee
 *            avant toutes les autres pour initialiser la librairie.
 *
 *
 *
 * ToDo List :
 * ~ ~ ~ ~ ~ ~
 *  - Routine de libération de la mémoire.
 *  - Verification de l'appel de l'initialisation.
 *  - Etendre a plus de 10 pts de Gauss tout en optimisant la
 *    memoire utilisee.
 *  - Introduire des triangles (2d/3d) & tetraedres (3d).
 *  - Introduire d'autres courbes (cercles, splines, ...).
 *
 * RoBo aout 2000
 */

#ifndef __GAUSSLIB_H__
#define __GAUSSLIB_H__

/* ---------------------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "el.h"

/* ---------------------------------------------------------------------------------- */

#define GAUSS_MAX_NG 10

#define GAUSS_MAX_EL 3

#define GAUSS_EL_LINE 0
#define GAUSS_EL_QUAD 1
#define GAUSS_EL_HEXA 2

/* ---------------------------------------------------------------------------------- */

/*
 *   QUAD BI-LINEAIRE 2D/3D
 */

double *quad_xg[GAUSS_MAX_NG];
double *quad_pg[GAUSS_MAX_NG];
double ***quad_psi[GAUSS_MAX_NG];

/*
 *   SEGMENT LINEAIRE 1D/2D/3D
 */

double *line_xg[GAUSS_MAX_NG];
double *line_pg[GAUSS_MAX_NG];
double ***line_psi[GAUSS_MAX_NG];

/*
 *   HEXAEDRE TRI-LINEAIRE 3D
 */

double *hexa_xg[GAUSS_MAX_NG];
double *hexa_pg[GAUSS_MAX_NG];
double ***hexa_psi[GAUSS_MAX_NG];

/* 
 *   ELEM HERMITE 2 NOEUDS (CUBIQUE) // n'est pas ajoute au cas "generique"
 */

// hermite_xg = line_xg
// hermite_pg = line_pg
double ***hermite_psi[GAUSS_MAX_NG];

/*
 *   ROUTINE GENERALE
 */

double *generic_xg[GAUSS_MAX_EL][GAUSS_MAX_NG];
double *generic_pg[GAUSS_MAX_EL][GAUSS_MAX_NG];
double ***generic_psi[GAUSS_MAX_EL][GAUSS_MAX_NG];

/* ---------------------------------------------------------------------------------- */

/* protos */

// gauss_common.c

int gauss_common_init();
int gauss_common_pp(double *xg, double *wg, int ng);

// gauss_quad.c

int gauss_quad(int ng, int ndim,
               double *x1, double *x2, double *x3, double *x4,
               int (*fct)(double *, double *, void *, int, double *),
               void *par, double *res);
void gauss_quad_getx(int no, double ***psi, double **xx, int ndim, double *x);
int gauss_quad_get_psi(int ng, double ****psi, double *xg);
void gauss_quad_jaco(double **xx, double jaco[][3], int no,
                     double *xg, double ***psi, int ndim);
int gauss_quad_get_xgpg(int ng, double **xg, double **pg);

// gauss_line.c

int gauss_line(int ng, int ndim,
               double *x1, double *x2,
               int (*fct)(double *, double *, void *, int, double *),
               void *par, double *res);
void gauss_line_getx(int no, double ***psi, double **xx, int ndim, double *x);
void gauss_line_getf(int no, double ***psi, double *ff, double *x);
int gauss_line_get_psi(int ng, double ****psi, double *xg);
void gauss_line_jaco(double **xx, double jaco[][3], int no,
                     double *xg, double ***psi, int ndim);
int gauss_line_get_xgpg(int ng, double **xg, double **pg);

// gauss_hexa.c

int gauss_hexa(int ng, int ndim,
               double *x1, double *x2, double *x3, double *x4,
               double *x5, double *x6, double *x7, double *x8,
               int (*fct)(double *, double *, void *, int, double *),
               void *par, double *res);
void gauss_hexa_getx(int no, double ***psi, double **xx, int ndim, double *x);
int gauss_hexa_get_psi(int ng, double ****psi, double *xg);
void gauss_hexa_jaco(double **xx, double jaco[][3], int no,
                     double *xg, double ***psi, int ndim);
int gauss_hexa_get_xgpg(int ng, double **xg, double **pg);

// gauss_generic.c

int gauss_generic(int ng, int ndim,
                  double **xx, int type,
                  int (*fct)(double *, double *, void *, int, double *),
                  void *par, double *res);
void gauss_generic_getx(int no, double ***psi, double **xx,
                        int ndim, int nnode, double *x);
int gauss_generic_get_psi(int ng, double ****psi, double *xg,
                          int type, int dimp, int nnode, int npg);
void gauss_generic_jaco(double **xx, double jaco[][3], int no,
                        double *xg, double ***psi, int ndim,
                        int dimp, int nnode);
int gauss_generic_get_xgpg(int ng, double **xg, double **pg, int dimp, int type,
                           int npg);

// gauss_hermite.c

int gauss_hermite_get_psi(int ng, double ****psi, double *xg);

/* ---------------------------------------------------------------------------------- */

#endif
