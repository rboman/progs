/*
 * $Id$
 *
 * Calule le produit Sp p
 *
 * RoBo
 *
 */

#include "ehd.h"

int ehd_spp(double Sp[4][4], double *p, double *dp, double *res)
{

  int i;

  for(i=0;i<4;i++)
    res[i] += Sp[i][0]*p[0]
      +       Sp[i][1]*dp[0]
      +       Sp[i][2]*p[1]
      +       Sp[i][3]*dp[1];


  return 0;
}
















