/*
 * Calcule la viscosite en fct de la pression
 * et sa derivee.
 */

#include "ehd.h"

int ehd_visco(double eta0, double alpha, double p, double *eta, double *etad)
{
  int iop=0;


  *eta = eta0 * exp(alpha*p);

  *etad = alpha * (*eta);

    //FIN:
  if(iop>900)
    printf("\n\t-->"__FILE__"\n");
  return iop;

}

