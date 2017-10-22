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
 * TdiLib
 * ======
 *   - Gestion de Matrices TriDIagonales
 *   - Solver non symetrique (sans gestion de pivots nuls)
 *
 * RoBo 27-09-00
 */

#ifndef __TDILIB_H__
#define __TDILIB_H__

#include "sky.h"

/**************************************************************************
                                    Macros
 **************************************************************************/

/* codes d'erreurs renvoyes par le solveur */

#define TDI_ERR_OK 0
#define TDI_ERR_PIV0 1

/* options du solver (utiliser l'operateur '|') */

#define TDI_DO_LU (1 << 0)
#define TDI_DO_SUBST (1 << 1)

/* etat de la matrice (matrice/decomp LU - LDLt) */

#define TDI_A 0
#define TDI_LU 1

/**************************************************************************
                                    Objet(s)
 **************************************************************************/

struct SKY_API TdiMat
{
    int init;
    int nsys;
    int nsys_a;
    double *s[3];
    char *name;
};

/**************************************************************************
                                   Prototypes
 **************************************************************************/

// Routines d'initialisation
SKY_API int tdi_init(TdiMat *A);
SKY_API int tdi_reinit(TdiMat *A);
SKY_API int tdi_setname(TdiMat *A, char *name);
SKY_API int tdi_setsize(TdiMat *A, int nsys);

// Routines de manipulation de la matrice
SKY_API int tdi_ass(TdiMat *A, int i, int j, double val);
SKY_API int tdi_set(TdiMat *A, int i, int j, double val);
SKY_API int tdi_fill(TdiMat *A, double val);

// Solveur
SKY_API int tdi_solve(TdiMat *A, double *q, double *x, int type);
SKY_API void tdi_print_err(FILE *fich, int code);

#endif // __TDILIB_H__
