/*
 * Integration d'une fonction sur un quad bi-lineaire 2D/3D
 * par la methode de Gauss
 */

#include "gausslib.h"

/* ---------------------------------------------------------------------------------- */

/*
 *  Integration d'une fonction sur un quad bi-lineaire
 */

int gauss_quad(int ng, int ndim,
               double *x1, double *x2, double *x3, double *x4, 
               int (*fct)(double *,double *,void *,int,double *), 
               void *par, double *res)
{
  int iop=0;
  int i;
  double *xg,*pg;
  double detj;
  double ***psi;
  double *xx[EL_QUAD_NODE];
  double *ksi;
  double x[3];
  double valfct;
  double jaco[3][3];

  // initialisations

  xx[0] = x1;
  xx[1] = x2;
  xx[2] = x3;
  xx[3] = x4;

  if(ndim!=2 && ndim!=3) goto ERR3;

  // recupere les pos & poids de Gauss (->xg, pg)

  iop = gauss_quad_get_xgpg(ng, &xg, &pg);
  if(iop!=0) goto FIN;

  // recupere les valeurs des fct de forme & derivees (->psi)

  iop = gauss_quad_get_psi(ng, &psi, xg);
  if(iop!=0) goto FIN;

  // calcule les determinants du jacobien aux pts de Gauss

  *res = 0.0;
  for(i=0;i<ng*ng;i++) {
    
    // determinant du jacobien (->detj)
    gauss_quad_jaco(xx, jaco, i, xg, psi,ndim);
    el_quad_detj(jaco,ndim,&detj);

    //if(detj<=0.0) goto ERR2;

    // coordonnees du point traite
    ksi = &(xg[i*EL_QUAD_DIM]);
    gauss_quad_getx(i, psi, xx, ndim, x);

    // calcule la valeur de la fct
    iop = (*fct)(ksi, x, par, i,&valfct);
    if(iop!=0) goto ERR1;

    // formule de Gauss

    *res += valfct*detj*pg[i];

  }


 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;
 ERR1:
  printf("\nErreur: probleme lors de l'evaluation de la fct a integrer");
  iop = 990;
  goto FIN;
  /*
 ERR2:
  printf("\nErreur: jacobien negatif ou nul");
  iop = 990;
  goto FIN;
  */
 ERR3:
  printf("\nErreur: dimension incorrecte (ndim=%d)",ndim);
  iop = 990;
  goto FIN;

}

/* ---------------------------------------------------------------------------------- */

void gauss_quad_getx(int no, double ***psi, double **xx, int ndim, double *x)
{
  int i,j;

  for(j=0;j<ndim;j++) {
  
    x[j]=0.0;
    for(i=0; i<EL_QUAD_NODE; i++)
      x[j]+=psi[0][i][no]*xx[i][j];

  }

}

/* ---------------------------------------------------------------------------------- */

int gauss_quad_get_psi(int ng, double ****psi, double *xg)
{
  int iop=0;
  int i,j,l,m;
  int ng1;
  double F[EL_QUAD_NODE][4];
  double *c;
  
  ng1 = ng-1;

  if(ng>GAUSS_MAX_NG) goto ERR2;

  // Calcul si pas encore fait

  if(quad_psi[ng1]==NULL) {

    // allocation

    quad_psi[ng1] = (double***)calloc(1+EL_QUAD_DIM, sizeof(double**));
    if(quad_psi[ng1]==NULL) goto ERR1;

    for(i=0;i<1+EL_QUAD_DIM;i++) {
      quad_psi[ng1][i] = (double**)calloc(EL_QUAD_NODE, sizeof(double*));
      if(quad_psi[ng1][i]==NULL) goto ERR1;
      for(j=0;j<EL_QUAD_NODE;j++) {
        quad_psi[ng1][i][j] = (double*)calloc(ng*ng, sizeof(double));
        if(quad_psi[ng1][i][j]==NULL) goto ERR1;
      }
    }

    // remplissage

    for(i=0;i<ng*ng;i++) {

      c = &(xg[EL_QUAD_DIM*i]);
      el_quad_ff(F,c);

      for(l=0;l<1+EL_QUAD_DIM;l++)
        for(m=0;m<EL_QUAD_NODE;m++)
          quad_psi[ng1][l][m][i] = F[m][l];
    }
  } // endif
  
  // Retourne le pointeur

  *psi = quad_psi[ng1];

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;

 ERR1:
  printf("\nErreur: pas assez de memoire");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nErreur: le nbre de points de Gauss demande est trop grand !");
  iop = 990;
  goto FIN;
}


/* ---------------------------------------------------------------------------------- */

/*
 *   Retourne la matrice jacobienne "jaco" au pt de Gauss "no" 
 */


void gauss_quad_jaco(double **xx, double jaco[][3], int no, 
                     double *xg, double ***psi, int ndim)
{
  int i,j,k;
  double va;

  for(j=0;j<EL_QUAD_DIM;j++) {

    for(i=0;i<ndim;i++)
      jaco[i][j]=0.;

    for(i=0;i<EL_QUAD_NODE;i++) {
      va = psi[j+1][i][no];
      for(k=0;k<ndim;k++) {
        jaco[k][j] += va * xx[i][k];
      }
    }
  }

}

/* ---------------------------------------------------------------------------------- */

/*
 *   Renvoie un ptr vers les points et un ptr vers les poids de Gauss
 */

int gauss_quad_get_xgpg(int ng, double **xg, double **pg)
{
  int iop=0;
  int i,j;
  int ng1;
  int n1,n2;
  double xg1d[GAUSS_MAX_NG], pg1d[GAUSS_MAX_NG];

  ng1 = ng-1;

  if(ng>GAUSS_MAX_NG) goto ERR2;

  // Calcul si pas encore fait

  if(quad_xg[ng1] == NULL) {

    // allocation
    quad_xg[ng1] = (double*)calloc(ng*ng*EL_QUAD_DIM,sizeof(double));
    if(quad_xg[ng1]==NULL) goto ERR1;
    quad_pg[ng1] = (double*)calloc(ng*ng,sizeof(double));
    if(quad_pg[ng1]==NULL) goto ERR1;

    // calcul points & poids 1d (verif ng dans limites)
    iop = gauss_common_pp(xg1d,pg1d,ng);
    if(iop!=0) goto FIN;

    // remplissage
    n1 = 0;
    n2 = 0;
    for(i=0;i<ng;i++) {
      for(j=0;j<ng;j++) {
        quad_xg[ng1][n1++]=xg1d[i];
        quad_xg[ng1][n1++]=xg1d[j];
        quad_pg[ng1][n2++]=pg1d[i]*pg1d[j];
      }
    }

  } // endif

  // Retourne les pointeurs

  (*xg) = quad_xg[ng1];
  (*pg) = quad_pg[ng1];

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;

 ERR1:
  printf("\nErreur: pas assez de memoire");
  iop = 990;
  goto FIN;
 ERR2:
  printf("\nErreur: le nbre de points de Gauss demande est trop grand !");
  iop = 990;
  goto FIN;
}

/* ---------------------------------------------------------------------------------- */

