/*
 * $Id$
 *
 * SkyLib
 * ------
 *   - Gestion des matrices au format SKYLINE
 *   - Solver Symetrique / Non Symetrique
 *     (sans gestion de pivots nuls)
 *
 *
 * RoBo 27-09-00
 *
 */

#ifndef __SKYLIB_H__
#define __SKYLIB_H__

#include <stdio.h>

#ifdef _UNDERSCORE_
#define sky_f_solve_usym sky_f_solve_usym_
#endif

/**************************************************************************
                                    Macros
 **************************************************************************/

/* type de matrice (sym/non-sym) */

#define SKY_MAT_UNKNOWN 0
#define SKY_MAT_USYM    1
#define SKY_MAT_SYM     2

/* codes d'erreurs renvoyes par les solveurs */ 

#define SKY_ERR_OK   0
#define SKY_ERR_PIV0 1
#define SKY_ERR_PIVN 2

/* options du solver (utiliser l'operateur '|') */

#define SKY_DO_LU        (1<<0)
#define SKY_DO_SUBST     (1<<1)
#define SKY_STOP_ON_PIVN (1<<2)

/* etat de la matrice (matrice/decomp LU - LDLt) */

#define SKY_A  0
#define SKY_LU 1

/* options d'impression */

#define SKY_SILENT  0
#define SKY_VERBOSE 1

/**************************************************************************
                                    Objet(s)
 **************************************************************************/


typedef struct SKYMAT S_SKYMAT;

struct SKYMAT {
  int init;           // code d'init
  int sym;            // matrice symetrique O/N
  double *sitl;       // matrice L
  double *situ;       // matrice U
  int nsit;           // nbre de termes stockes
  int nsitl_a;        // taille allouee pour L
  int nsitu_a;        // taille allouee pour U
  int *locsit;        // ptr vers les termes diag
  int nsys;           // taille de la matrice
  int nsys_a;         // taille matrice allouee
  char *name;         // nom (pour affichage)
};

/**************************************************************************
                                   Prototypes
 **************************************************************************/


// Debug
int sky_print(FILE *fich, S_SKYMAT *A);

// Routines d'initialisation
int sky_init(S_SKYMAT *A);
int sky_reinit(S_SKYMAT *A);
int sky_setname(S_SKYMAT *A, char *name);

// Routines de (re)determination de la ligne de ciel
int sky_pre_start(S_SKYMAT *A, int nsys);
int sky_pre_ass(S_SKYMAT *A, int i, int j);
int sky_alloc(S_SKYMAT *A, int type);
int sky_pre_close(S_SKYMAT *A, int type, int opt);

// Routines de manipulation de la matrice
int sky_ass(S_SKYMAT *A, int i, int j, double val);
int sky_set(S_SKYMAT *A, int i, int j, double val);
int sky_fill(S_SKYMAT *A, double val);

// Multiplication
int sky_mulv_a(S_SKYMAT *A, double *x, double *b);
int sky_mulv_lu(S_SKYMAT *A, double *x, double *b);

// Solveurs
int sky_solve_sym(S_SKYMAT *A, double *q, double *x, int type);
int sky_solve_usym(S_SKYMAT *A, double *q, double *x, int type);
int sky_solve_usym_opt(S_SKYMAT *A, double *q, double *x, int type);
int sky_solve(S_SKYMAT *A, double *q, double *x, int type);
void sky_print_err(FILE *fich, int code);

// Interfaces FORTRAN
void sky_f_solve_usym(double *sitl, double *situ,
		      int *locsit, int *nsys, int *nsit, 
                      double *q, double *x, int *kkk, int *iop);


#endif // __SKYLIB_H__
