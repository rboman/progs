/*
 * Donnees du probleme
 */

#include "ehd.h"
#include "math.h"

int ehd_setpar(int nn, double *x, double *h, double *h_t0, double *um,
               double *eta0, double *alpha, double *u, double *dt)
{
  int iop=0;
  int i;

  double L    = 200.0;
  double e    = 0.01;
  double R    = 2.0;
  double xr   = 0.9;
  double visc = 0.1;

  double xx;
  
  
  // positions

#if 1  
  for(i=0;i<nn;i++) {
    x[i] = L*(double)i/(nn-1);
  }
#else
  for(i=0;i<nn;i++) {
    x[i] = L-L*(double)i/(nn-1);
  }
#endif

  *eta0 = visc;
  *alpha = 1.0e-4;
  *alpha = 0.0;

  // epaisseurs de film

  for(i=0;i<nn;i++) {
#if 1
    u[i]   = 1.0;
    h[i]   = 0.1e-1 - 0.5e-3*(x[i]-L);
    //xx = x[i]-xr;
    //h[i] = R+e-sqrt(R*R-xx*xx);
    um[i] = 0.0;
    //eta[i] = visc;
#else
    u[i]   = -1.0;
    h[i]   = 6.0e0 - 0.5e-3*x[i]/L;
    xx = L-x[i]-xr;
    h[i] = R+e-sqrt(R*R-xx*xx);
    um[i] = 0.0;    
    //eta[i] = visc;
#endif
  }
  
  *dt = 1.0e-1;
  for(i=0;i<nn;i++) {
    h_t0[i]=h[i]+2*e;
  }  

  return iop;
}
