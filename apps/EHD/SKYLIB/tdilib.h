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

typedef struct TDIMAT S_TDIMAT;

struct TDIMAT
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
int tdi_init(S_TDIMAT *A);
int tdi_reinit(S_TDIMAT *A);
int tdi_setname(S_TDIMAT *A, char *name);
int tdi_setsize(S_TDIMAT *A, int nsys);

// Routines de manipulation de la matrice
int tdi_ass(S_TDIMAT *A, int i, int j, double val);
int tdi_set(S_TDIMAT *A, int i, int j, double val);
int tdi_fill(S_TDIMAT *A, double val);

// Solveur
int tdi_solve(S_TDIMAT *A, double *q, double *x, int type);
void tdi_print_err(FILE *fich, int code);

#endif // __TDILIB_H__
