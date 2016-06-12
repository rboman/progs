/*
 * $Id$
 *
 * TdiLib
 * ======
 *   - Gestion de Matrices TriDIagonales
 *   - Solver non symetrique (sans gestion de pivots nuls)
 *
 *
 * RoBo 27-09-00
 *
 * Remarques: - l'utilisation est identique a "skylib"
 *            - la matrice est ecrasee par sa decomposition LU !!
 *
 */


/**************************************************************************
                                    Headers
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "tdilib.h"
#include "mlab.h"

/**************************************************************************
                                 Macros locales
 **************************************************************************/

// Tolerance relative aux pivots nuls
#define TDI_EPS 1.0e-18

// Compile la routine "main"
//#define TDI_STANDALONE

// Infos de debug
#undef VERBOSE
#define VERBOSE 0

char *tdi_nulname = "noname";


/**************************************************************************
                        Routines d'initialisation
 **************************************************************************/

/*
 *                    Initialise une matrice TRIDIAG
 *                    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_init(S_TDIMAT *A)
{
  int iop=0;
  int i;

  if(A->init==1) goto ERR1;
  
  A->nsys   = 0;
  A->nsys_a = 0;
  for(i=0;i<3;i++)
    A->s[i] = NULL;
  A->name    = tdi_nulname;

  A->init = 1;

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice \"%s\" a deja ete initialisee !",A->name);
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *              Reinit la matrice utilisee (libere la memoire)
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_reinit(S_TDIMAT *A)
{
  int iop=0;
  int i,mem;

  if(A->init!=1) goto ERR1;

  // PURGE LA MEMOIRE

  // s[]
  mem=0;
  if(A->nsys_a>0) {
    for(i=0;i<3;i++)
      free(A->s[i]);
    mem = 3*A->nsys_a;
    A->nsys_a=0;
  }
  // nom
  if(A->name!=tdi_nulname) 
    free(A->name);

#if VERBOSE
    printf("liberation de %d doubles\n",mem);
#endif
  // INITIALISATION

  A->init=0;
  iop = tdi_init(A);
  if(iop!=0) goto FIN;

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *                Donne un nom a une matrice (pour output)
 *                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_setname(S_TDIMAT *A, char *name)
{
  int iop=0;
  size_t l;

  if(A->init!=1) goto ERR1;

  l = strlen(name);
  A->name = (char*)calloc(l+1,sizeof(char));
  if(A->name == NULL) goto ERR2;
  strcpy(A->name,name);

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *              Definit la taille de la matrice
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_setsize(S_TDIMAT *A, int nsys)
{
  int iop=0;
  int i;

  if(A->init!=1) goto ERR1;

  if(nsys>0) {
    if(A->nsys_a<nsys) {
      for(i=0;i<3;i++) {
        A->s[i] = (double*)realloc(A->s[i],nsys*sizeof(double));
        if(A->s[i]==NULL) goto ERR2;
        A->nsys_a = nsys;
      }
    }
    A->nsys = nsys;
  }
  else 
    goto ERR3;
  
  // Init

  iop = tdi_fill(A,0.0);
  if(iop!=0) goto FIN;

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;
 ERR3:
  printf("\nerreur: taille matrice <0 !");
  iop = 990;
  goto FIN;
}

/**************************************************************************
                  Routines de manipulation de la matrice
 **************************************************************************/

/*
 *              Assemble un element dans la matrice TRIDIAG
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_ass(S_TDIMAT *A, int i, int j, double val)
{
  int iop=0;

  if(abs(i-j)>1) goto ERR1;

  A->s[1+i-j][i] += val; 


 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: (i,j)=(%d,%d) hors des 3 diagonales !",i,j);
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *              Assemble un element dans la matrice TRIDIAG
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_set(S_TDIMAT *A, int i, int j, double val)
{
  int iop=0;

  if(abs(i-j)>1) goto ERR1;

  A->s[1+i-j][i] = val; 


 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: (i,j)=(%d,%d) hors des 3 diagonales !",i,j);
  iop = 990;
  goto FIN;

}

/**************************************************************************/

/*
 *              Remplit un matrice TRIDIAG avec "val"
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int tdi_fill(S_TDIMAT *A, double val)
{
  int iop=0;
  int i,j;

  if(A->init!=1) goto ERR1;

  for(i=0;i<3;i++)
    for(j=0;j<A->nsys;j++)
      A->s[i][j] = val;

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nerreur: la matrice n'est pas initialisee !");
  iop = 990;
  goto FIN;

}

/**************************************************************************
                                  Solveur
 **************************************************************************/

int tdi_solve(S_TDIMAT *A, double *q, double *x, int type)
{
  int iop=TDI_ERR_OK;
  double *s[3];
  int i,nn;
  double prec;

  // Raccourcis
  
  for(i=0;i<3;i++)
    s[i]=A->s[i];
  nn = A->nsys;

  // Decomposition LU

  if((type & TDI_DO_LU) == TDI_DO_LU) {

    prec = 0.0;
    for(i=0;i<nn;i++) {
      prec += fabs(s[1][i]);
    }
    prec = prec/nn*TDI_EPS;

    if(fabs(s[1][0])<prec) goto ERR1; 
    for(i=1;i<nn;i++) {
      s[2][i] /= s[1][i-1];
      s[1][i] -= s[2][i]*s[0][i-1];
      if(fabs(s[1][i])<prec) goto ERR1; 
    }

  }

  if((type & TDI_DO_SUBST) == TDI_DO_SUBST) {

    // Subst avant
    
    x[0] = q[0];
    for(i=1;i<nn;i++)
      x[i] = q[i] - s[2][i]*x[i-1];
    
    // Subst arriere (on suppose qu'il n'y a pas de pivots nuls) 
    
    x[nn-1] = x[nn-1]/s[1][nn-1];
    for(i=nn-2;i>=0;i--)
      x[i] = (x[i]-s[0][i]*x[i+1])/s[1][i];

  }

  /***/

 FIN:
  return iop;
  
 ERR1:
  iop = SKY_ERR_PIV0;
  goto FIN;

}

/**************************************************************************/

/*
 *             Traitement des codes renvoyes par les solveurs
 *             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

void tdi_print_err(FILE *fich, int code)
{
  char *err[3] = {"ok", "pivot nul", "code inconnu"};
  char *e;
  
  switch(code) {
  case TDI_ERR_OK:
    e = err[0];
    break;
  case TDI_ERR_PIV0:
    e = err[1];
    break;
  default:
    e = err[2];
    break;
  }
  fprintf(fich, "code solver: %s\n",e);

}

/**************************************************************************
                       Routines de test de la librairie
 **************************************************************************/

#ifdef TDI_STANDALONE

int tdi_test()
{
  int iop=0;
  S_TDIMAT K;
  int i,j,n;

  double A[5][5] = { { 1, 3, 0, 0, 0},
                     { 1, 2, 0, 0, 0},
                     { 0, 2, 3, 0, 0},
                     { 0, 0, 11, 4, 0},
                     { 0, 0, 0, 0, 7}};

  double q[5] = {1,1,1,1,1};
  double x[5],xs[5];
  n = 5;

  // initialisation

  for(i=0;i<n;i++) {
    x[i]=0;xs[i]=0;
  }

  iop = tdi_init(&K);
  if(iop!=0) goto FIN;

  iop = tdi_setname(&K,"K");
  if(iop!=0) goto FIN;

  iop = tdi_setsize(&K,n);
  if(iop!=0) goto FIN;

  // assemblage Ks

  for(i=0;i<n;i++) {
    for(j=0;j<n;j++) {
      if(A[i][j]!=0.0) {
        iop = tdi_ass(&K,i,j,A[i][j]);
        if(iop!=0) goto FIN;
      }
    }
  }

  mlab_tdi("tri.m","1",&K,TDI_A,MLAB_NEW, MLAB_VERBOSE);

  // resolution

  iop = tdi_solve(&K, q, x, TDI_DO_LU | TDI_DO_SUBST); 
  tdi_print_err(stdout,iop);

  // verification matlab

  mlab_tdi("tri.m","2",&K,TDI_LU,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("tri.m","q",q,n,MLAB_OLD, MLAB_VERBOSE);
  mlab_vec("tri.m","x",x,n,MLAB_OLD, MLAB_VERBOSE);

  // purge memoire (facultatif)

  iop = tdi_reinit(&K);
  if(iop!=0) goto FIN;

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;

}

/**************************************************************************/

int main()
{
  tdi_test();
  return 0;
}

#endif
