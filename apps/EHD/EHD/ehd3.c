/*
 * Test des flow factors
 */

#include "ehd.h"

int main3()
{
  int iop=0;

  int i;
  int loi = EHD_PATIR;
  const int nn = 100;
  double h_max = 10;
  double h_min = 1;
  

    double *h = (double*)malloc(nn*sizeof(double));

  double Rq=1.0, Rq1=1.0, Rq2=0.0;
  double gam_s;

    double *PhiS = (double*)malloc(nn*sizeof(double));
    double *PhiP = (double*)malloc(nn*sizeof(double));
    double *dPhiS = (double*)malloc(nn*sizeof(double));
    double *dPhiP = (double*)malloc(nn*sizeof(double));



  // Calcule l'abscisse

  for(i=0;i<nn;i++) {
    h[i] = h_min + (h_max-h_min) * (double)i/((double)(nn-1)); 
  }

  // Evaluation

  gam_s = 1.0/9.0;

  for(i=0;i<nn;i++) {
    iop = ehd_flow_factors(h[i], gam_s, 
                           Rq, Rq1, Rq2,
                           &(PhiP[i]), &(PhiS[i]),
                           &(dPhiP[i]), &(dPhiS[i]), loi);
    if(iop!=0) goto FIN;
  }

  // Resultats vers matlab

  iop = mlab_vec("pipo.m", "h"    , h     , nn    , MLAB_NEW, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "PhiP" , PhiP  , nn    , MLAB_OLD, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "PhiS" , PhiS  , nn    , MLAB_OLD, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "dPhiP", dPhiP , nn    , MLAB_OLD, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "dPhiS", dPhiS , nn    , MLAB_OLD, MLAB_VERBOSE);
  if(iop!=0) goto FIN;


    free(h);
    free(PhiS);
    free(PhiP);
    free(dPhiS);
    free(dPhiP);

  /***/

 FIN:
  if(iop>900)
    printf("\n\t-->"__FUNCTION__" in "__FILE__"\n");
  return iop;

}
