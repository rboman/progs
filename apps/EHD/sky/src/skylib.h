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
 * SkyLib
 * ------
 *   - Gestion des matrices au format SKYLINE
 *   - Solver Symetrique / Non Symetrique
 *     (sans gestion de pivots nuls)
 *
 * RoBo 27-09-00
 */

#ifndef __SKYLIB_H__
#define __SKYLIB_H__

#include "sky.h"
#include <stdio.h>

#ifdef _UNDERSCORE_
#define sky_f_solve_usym sky_f_solve_usym_
#endif

/**************************************************************************
                                    Macros
 **************************************************************************/

/* type de matrice (sym/non-sym) */

#define SKY_MAT_UNKNOWN 0
#define SKY_MAT_USYM 1
#define SKY_MAT_SYM 2

/* codes d'erreurs renvoyes par les solveurs */

#define SKY_ERR_OK 0
#define SKY_ERR_PIV0 1
#define SKY_ERR_PIVN 2

/* options du solver (utiliser l'operateur '|') */

#define SKY_DO_LU (1 << 0)
#define SKY_DO_SUBST (1 << 1)
#define SKY_STOP_ON_PIVN (1 << 2)

/* etat de la matrice (matrice/decomp LU - LDLt) */

#define SKY_A 0
#define SKY_LU 1

/* options d'impression */

#define SKY_SILENT 0
#define SKY_VERBOSE 1

/**************************************************************************
                                    Objet(s)
 **************************************************************************/

struct SKY_API SkyMat
{
    int init;     // code d'init
    int sym;      // matrice symetrique O/N
    double *sitl; // matrice L
    double *situ; // matrice U
    int nsit;     // nbre de termes stockes
    int nsitl_a;  // taille allouee pour L
    int nsitu_a;  // taille allouee pour U
    int *locsit;  // ptr vers les termes diag
    int nsys;     // taille de la matrice
    int nsys_a;   // taille matrice allouee
    char *name;   // nom (pour affichage)
};

/**************************************************************************
                                   Prototypes
 **************************************************************************/

// Debug
SKY_API int sky_print(FILE *fich, SkyMat *A);

// Routines d'initialisation
SKY_API int sky_initmat(SkyMat *A);
SKY_API int sky_reinit(SkyMat *A);
SKY_API int sky_setname(SkyMat *A, char *name);

// Routines de (re)determination de la ligne de ciel
SKY_API int sky_pre_start(SkyMat *A, int nsys);
SKY_API int sky_pre_ass(SkyMat *A, int i, int j);
SKY_API int sky_alloc(SkyMat *A, int type);
SKY_API int sky_pre_close(SkyMat *A, int type, int opt);

// Routines de manipulation de la matrice
SKY_API int sky_ass(SkyMat *A, int i, int j, double val);
SKY_API int sky_set(SkyMat *A, int i, int j, double val);
SKY_API int sky_fill(SkyMat *A, double val);

// Multiplication
SKY_API int sky_mulv_a(SkyMat *A, double *x, double *b);
SKY_API int sky_mulv_lu(SkyMat *A, double *x, double *b);

// Solveurs
SKY_API int sky_solve_sym(SkyMat *A, double *q, double *x, int type);
SKY_API int sky_solve_usym(SkyMat *A, double *q, double *x, int type);
SKY_API int sky_solve_usym_opt(SkyMat *A, double *q, double *x, int type);
SKY_API int sky_solve(SkyMat *A, double *q, double *x, int type);
SKY_API void sky_print_err(FILE *fich, int code);

// Interfaces FORTRAN
SKY_API void sky_f_solve_usym(double *sitl, double *situ,
                      int *locsit, int *nsys, int *nsit,
                      double *q, double *x, int *kkk, int *iop);

SKY_API int sky_test();

#endif // __SKYLIB_H__
