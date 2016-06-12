/*
 * $Id$
 *
 * Calcule le cisaillement (tau) en un point donne
 *
 *
 */

#include "ehd.h"

int ehd_cisail(double eta0, double alpha,
               double v, double p, double dp, double h, 
               double Rq, double Rq1, double Rq2,
               int loi, double *tau)
{

  int iop=0;

  double PhiF, PhiFS, PhiFP;
  double tmp;
  double eta,etad;

  /*
   *  Calcul des "flow factors" : PhiF, PhiFS, PhiFP
   */

  iop = ehd_flow_cisail(h, Rq, Rq1, Rq2, loi,
                        &PhiF, &PhiFS, &PhiFP);
  if(iop!=0) goto FIN;

  /*
   *  Viscosite
   */

  iop = ehd_visco(eta0, alpha, p, &eta, &etad);
  if(iop!=0) goto FIN;

  /*
   *  Evaluation de "tau"
   */

  if(h!=0.0) {

    tmp  = eta * v / h;
    tmp *= PhiF + PhiFS;
    tmp += h/2.0 * PhiFP * dp;

    *tau = -tmp;

  } else {

    goto ERR1;

  }

  /* ------------------------------------------------------------ */

 FIN:
  if(iop>900)
    printf("\n\t-->"__FUNCTION__" in "__FILE__"\n");
  return iop;
 ERR1:
  printf("Epaisseur de film nulle -> division par 0 !\n");
  iop = 990;
  goto FIN;

}
