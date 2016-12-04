/*
 * $Id$
 *
 * Essai de merge des routines
 *           
 *       "gauss_line"
 *       "gauss_quad"
 *       "gauss_hexa"
 *
 *
 * RoBo
 *
 */

#include "gausslib.h"

/* ---------------------------------------------------------------------------------- */

int gauss_generic(int ng, int ndim,
                  double **xx, int type, 
                  int (*fct)(double *,double *,void *, int, double *), 
                  void *par, double *res)
{
  int iop=0;
  int i;
  double *xg,*pg;
  double detj;
  double ***psi;
  double *ksi;
  double x[EL_MAX_DIM];
  double valfct;
  double jaco[EL_MAX_DIM][3];

  int npg, dimp, nnode;

  // initialisations

  switch(type) {
  case GAUSS_EL_LINE:
     if(ndim!=1 && ndim!=2 && ndim!=3) goto ERR3;
     dimp    = EL_LINE_DIM;
     nnode   = EL_LINE_NODE;
     npg     = ng;
     break;
  case GAUSS_EL_QUAD:
     if(ndim!=2 && ndim!=3) goto ERR3;
     dimp    = EL_QUAD_DIM;
     nnode   = EL_QUAD_NODE;
     npg     = ng*ng;
     break;  
  case GAUSS_EL_HEXA:
     if(ndim!=3) goto ERR3;
     dimp    = EL_HEXA_DIM;
     nnode   = EL_HEXA_NODE;
     npg     = ng*ng*ng;
     break;
  default:
    goto ERR4;
  }

  // recupere les pos & poids de Gauss (->xg, pg)

  iop = gauss_generic_get_xgpg(ng, &xg, &pg, dimp, type, npg);
  if(iop!=0) goto FIN;

  // recupere les valeurs des fct de forme & derivees (->psi)

  iop = gauss_generic_get_psi(ng, &psi, xg, type, dimp, nnode, npg);
  if(iop!=0) goto FIN;

  // calcule les determinants du jacobien aux pts de Gauss

  *res = 0.0;
  for(i=0;i<npg;i++) {
    
    // determinant du jacobien (->detj)
    gauss_generic_jaco(xx, jaco, i, xg, psi,ndim,dimp,nnode);

    switch(type) {
    case GAUSS_EL_LINE:
      el_line_detj(jaco, ndim, &detj);
      break;
    case GAUSS_EL_QUAD:
      el_quad_detj(jaco,ndim,&detj);
      break;  
    case GAUSS_EL_HEXA:
      el_hexa_detj(jaco, &detj);
      break;   
    }

    //if(detj<=0.0) goto ERR2;

    // coordonnees du point traite
    ksi = &(xg[i*dimp]);
    gauss_generic_getx(i, psi, xx, ndim, nnode, x);

    // calcule la valeur de la fct
    iop = (*fct)(ksi, x, par, i, &valfct);
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
  printf("\nErreur: dimension incompatible le type de domaine (ndim=%d)",ndim);
  iop = 990;
  goto FIN;
 ERR4:
  printf("\nErreur: type de domaine non supporte");
  iop = 990;
  goto FIN;

}
/* ---------------------------------------------------------------------------------- */

void gauss_generic_getx(int no, double ***psi, double **xx, 
                        int ndim, int nnode, double *x)
{
  int i,j;

  for(j=0;j<ndim;j++) {
  
    x[j]=0.0;
    for(i=0; i<nnode; i++)
      x[j]+=psi[0][i][no]*xx[i][j];

  }

}

/* ---------------------------------------------------------------------------------- */

int gauss_generic_get_psi(int ng, double ****psi, double *xg, 
                          int type, int dimp, int nnode, int npg)
{
  int iop=0;
  int i,j,l,m;
  int ng1;
  double F[EL_MAX_NODE][4];
  double *c;
  
  ng1 = ng-1;

  if(ng>GAUSS_MAX_NG) goto ERR2;

  // Calcul si pas encore fait

  if(generic_psi[type][ng1]==NULL) {

    // allocation

    generic_psi[type][ng1] = (double***)calloc(1+dimp, sizeof(double**));
    if(generic_psi[type][ng1]==NULL) goto ERR1;

    for(i=0;i<1+dimp;i++) {
      generic_psi[type][ng1][i] = (double**)calloc(nnode, sizeof(double*));
      if(generic_psi[type][ng1][i]==NULL) goto ERR1;
      for(j=0;j<nnode;j++) {
        generic_psi[type][ng1][i][j] = (double*)calloc(npg, sizeof(double));
        if(generic_psi[type][ng1][i][j]==NULL) goto ERR1;
      }
    }

    // remplissage

    for(i=0;i<npg;i++) {

      c = &(xg[dimp*i]);

      switch(type) {
      case GAUSS_EL_LINE:
        el_line_ff(F,c);
        break;
      case GAUSS_EL_QUAD:
        el_quad_ff(F,c);
        break;  
      case GAUSS_EL_HEXA:
        el_hexa_ff(F,c);
        break;   
      }

      for(l=0;l<1+dimp;l++)
        for(m=0;m<nnode;m++)
          generic_psi[type][ng1][l][m][i] = F[m][l];
    }
  } // endif
  
  // Retourne le pointeur

  *psi = generic_psi[type][ng1];

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


void gauss_generic_jaco(double **xx, double jaco[][3], int no, 
                        double *xg, double ***psi, int ndim, 
                        int dimp, int nnode)
{
  int i,j,k;
  double va;

  for(j=0;j<dimp;j++) {

    for(i=0;i<ndim;i++)
      jaco[i][j]=0.;

    for(i=0;i<nnode;i++) {
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

int gauss_generic_get_xgpg(int ng, double **xg, double **pg, int dimp,int type,
                           int npg)
{
  int iop=0;
  int i,j,k;
  int ng1;
  int n1,n2;
  double xg1d[GAUSS_MAX_NG], pg1d[GAUSS_MAX_NG];
  int ngx=0,ngy=0,ngz=0;

  ng1 = ng-1;

  if(ng>GAUSS_MAX_NG) goto ERR2;

  // Calcul si pas encore fait

  if(generic_xg[type][ng1] == NULL) {

    // allocation
    generic_xg[type][ng1] = (double*)calloc(npg*dimp,sizeof(double));
    if(generic_xg[type][ng1]==NULL) goto ERR1;
    generic_pg[type][ng1] = (double*)calloc(npg*dimp,sizeof(double));
    if(generic_pg[type][ng1]==NULL) goto ERR1;

    // calcul points & poids 1d (verif ng dans limites)
    iop = gauss_common_pp(xg1d,pg1d,ng);
    if(iop!=0) goto FIN;

    // remplissage

    switch(type) {
    case GAUSS_EL_LINE:
      ngx = ng;
      ngy = 1;
      ngz = 1;
      break;
    case GAUSS_EL_QUAD:
      ngx = ng;
      ngy = ng;
      ngz = 1;
      break;  
    case GAUSS_EL_HEXA:
      ngx = ng;
      ngy = ng;
      ngz = ng;
      break;   
    }

    n1 = 0;
    n2 = 0;
    for(i=0;i<ngx;i++) {
      for(j=0;j<ngy;j++) {
        for(k=0;k<ngz;k++) {
          generic_xg[type][ng1][n1++]=xg1d[i];
          generic_pg[type][ng1][n2]=pg1d[i];
          if(dimp>1) {
            generic_xg[type][ng1][n1++]=xg1d[j];
            generic_pg[type][ng1][n2]*=pg1d[j];
          }
          if(dimp>2) {
            generic_xg[type][ng1][n1++]=xg1d[k];
            generic_pg[type][ng1][n2]*=pg1d[k];
          }
          n2++;
        }
      }
    }

  } // endif

  // Retourne les pointeurs

  (*xg) = generic_xg[type][ng1];
  (*pg) = generic_pg[type][ng1];

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


