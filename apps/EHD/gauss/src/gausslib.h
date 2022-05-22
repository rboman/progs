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

#include "gauss.h"

#include <cstdio>
#include <cstdlib>
#include <cmath>

#include "el.h"

#define GAUSS_MAX_NG 10

#define GAUSS_MAX_EL 3

#define GAUSS_EL_LINE 0
#define GAUSS_EL_QUAD 1
#define GAUSS_EL_HEXA 2

/*
 *   QUAD BI-LINEAIRE 2D/3D
 */
#ifndef SWIG
extern double *quad_xg[GAUSS_MAX_NG];
extern double *quad_pg[GAUSS_MAX_NG];
extern double ***quad_psi[GAUSS_MAX_NG];

/*
 *   SEGMENT LINEAIRE 1D/2D/3D
 */

extern double *line_xg[GAUSS_MAX_NG];
extern double *line_pg[GAUSS_MAX_NG];
extern double ***line_psi[GAUSS_MAX_NG];

/*
 *   HEXAEDRE TRI-LINEAIRE 3D
 */

extern double *hexa_xg[GAUSS_MAX_NG];
extern double *hexa_pg[GAUSS_MAX_NG];
extern double ***hexa_psi[GAUSS_MAX_NG];

/*
 *   ELEM HERMITE 2 NOEUDS (CUBIQUE) // n'est pas ajoute au cas "generique"
 */

// hermite_xg = line_xg
// hermite_pg = line_pg
extern double ***hermite_psi[GAUSS_MAX_NG];

/*
 *   ROUTINE GENERALE
 */

extern double *generic_xg[GAUSS_MAX_EL][GAUSS_MAX_NG];
extern double *generic_pg[GAUSS_MAX_EL][GAUSS_MAX_NG];
extern double ***generic_psi[GAUSS_MAX_EL][GAUSS_MAX_NG];
#endif // SWIG

/* protos */

// gauss_common.c

GAUSS_API int gauss_common_init();
GAUSS_API int gauss_common_pp(double *xg, double *wg, int ng);

// gauss_quad.c

GAUSS_API int gauss_quad(int ng, int ndim, double *x1, double *x2, double *x3,
                         double *x4,
                         int (*fct)(double *, double *, void *, int, double *),
                         void *par, double *res);
GAUSS_API void gauss_quad_getx(int no, double ***psi, double **xx, int ndim,
                               double *x);
GAUSS_API int gauss_quad_get_psi(int ng, double ****psi, double *xg);
GAUSS_API void gauss_quad_jaco(double **xx, double jaco[][3], int no,
                               double *xg, double ***psi, int ndim);
GAUSS_API int gauss_quad_get_xgpg(int ng, double **xg, double **pg);

// gauss_line.c

GAUSS_API int gauss_line(int ng, int ndim, double *x1, double *x2,
                         int (*fct)(double *, double *, void *, int, double *),
                         void *par, double *res);
GAUSS_API void gauss_line_getx(int no, double ***psi, double **xx, int ndim,
                               double *x);
GAUSS_API void gauss_line_getf(int no, double ***psi, double *ff, double *x);
GAUSS_API int gauss_line_get_psi(int ng, double ****psi, double *xg);
GAUSS_API void gauss_line_jaco(double **xx, double jaco[][3], int no,
                               double *xg, double ***psi, int ndim);
GAUSS_API int gauss_line_get_xgpg(int ng, double **xg, double **pg);

// gauss_hexa.c

GAUSS_API int gauss_hexa(int ng, int ndim, double *x1, double *x2, double *x3,
                         double *x4, double *x5, double *x6, double *x7,
                         double *x8,
                         int (*fct)(double *, double *, void *, int, double *),
                         void *par, double *res);
GAUSS_API void gauss_hexa_getx(int no, double ***psi, double **xx, int ndim,
                               double *x);
GAUSS_API int gauss_hexa_get_psi(int ng, double ****psi, double *xg);
GAUSS_API void gauss_hexa_jaco(double **xx, double jaco[][3], int no,
                               double *xg, double ***psi, int ndim);
GAUSS_API int gauss_hexa_get_xgpg(int ng, double **xg, double **pg);

// gauss_generic.c

GAUSS_API int gauss_generic(int ng, int ndim, double **xx, int type,
                            int (*fct)(double *, double *, void *, int,
                                       double *),
                            void *par, double *res);
GAUSS_API void gauss_generic_getx(int no, double ***psi, double **xx, int ndim,
                                  int nnode, double *x);
GAUSS_API int gauss_generic_get_psi(int ng, double ****psi, double *xg,
                                    int type, int dimp, int nnode, int npg);
GAUSS_API void gauss_generic_jaco(double **xx, double jaco[][3], int no,
                                  double *xg, double ***psi, int ndim, int dimp,
                                  int nnode);
GAUSS_API int gauss_generic_get_xgpg(int ng, double **xg, double **pg, int dimp,
                                     int type, int npg);

// gauss_hermite.c

GAUSS_API int gauss_hermite_get_psi(int ng, double ****psi, double *xg);

GAUSS_API void gauss_line_getf2(int no, double ***psi, double *ff, double *x,
                                double upw);

#endif
