/*
 * $Id$
 *
 * Test determination CL
 *
 */

#include "ehd.h"

#define TOL_NR 1.0e-8

int main1()
{
  int iop=0;
  //int i,j,ni,nj,n;
  int i,n;

  const int nbelem=1000;
  const int nbnode=nbelem+1;

  // variables
  double eta0, alpha;

    double *h = (double*)malloc(nbnode*sizeof(double));
    double *h_t0 = (double*)malloc(nbnode*sizeof(double));
    double *eta = (double*)malloc(nbnode*sizeof(double));
    double *x = (double*)malloc(nbnode*sizeof(double));
    double *u = (double*)malloc(nbnode*sizeof(double));
    double *um = (double*)malloc(nbnode*sizeof(double));

  // inc
    double *p = (double*)malloc(nbnode*sizeof(double));
    double *dp = (double*)malloc(nbnode*sizeof(double));
    double *dpdh0 = (double*)malloc(nbnode*sizeof(double));


  S_SKYMAT K;
  S_SKYMAT K2;

#if 1
  // fixations
  int nbfix = 2;                    // nbre de fix
  int nnfix[2] = {0, nbelem};       // no de noeud fixe (start=0)
  int ndfix[2] = {0, 1};            // no de ddl fixe
  double vfix[2]  = {0.0, 0.0};     // valeur des fix

  // fixations
  int nbfix2 = 1;                 // nbre de fix
  int nnfix2[1] = {0};            // no de noeud fixe (start=0)
  int ndfix2[1] = {0};            // no de ddl fixe
  double vfix2[1]  = {0.0};       // valeur des fix

  int nL = nbnode-1;
#else
  // fixations
  int nbfix = 2;                    // nbre de fix
  int nnfix[2] = {0, nbelem};       // no de noeud fixe (start=0)
  int ndfix[2] = {1, 0};            // no de ddl fixe
  double vfix[2]  = {-1000.0, 0.0};     // valeur des fix

  // fixations
  int nbfix2 = 1;                 // nbre de fix
  int nnfix2[1] = {nbelem};       // no de noeud fixe (start=0)
  int ndfix2[1] = {0};            // no de ddl fixe
  double vfix2[1]  = {0.0};       // valeur des fix

  int nL = 0;
#endif

  // newton raphson

  int rester;
  double dh0,dh0t,pL=400.0;
  double residu;

  double dt;
  
  int scheme=EHD_STATIO;
  //int scheme=EHD_EULER;

#if 0
  double h2[nbnode];
  FILE *fich;
#endif

  
  // Init du module d'integration de Gauss
  
  iop = gauss_common_init();
  if(iop!=0) goto FIN;

  // Init du module EHD

  iop = ehd_init();
  if(iop!=0) goto FIN;

  // Mise en place des donnees

  iop = ehd_setpar(nbnode,x,h,h_t0,um,&eta0,&alpha,u,&dt);
  if(iop!=0) goto FIN;

  for(i=0;i<nbnode;i++)
    eta[i] = eta0;

  // Initialisation skyline

  iop = sky_init(&K);
  if(iop!=0) goto FIN;
  iop = sky_setname(&K,"K");
  if(iop!=0) goto FIN;
  iop = sky_init(&K2);
  if(iop!=0) goto FIN;
  iop = sky_setname(&K2,"K2");
  if(iop!=0) goto FIN;

  // NEWTON-RAPHSON
  // --------------

#if 0  
  fich = fopen("pipo.m","w");
  n=0;
  for(dh0 = -0.005; dh0<0.1; dh0+=0.0005) {
    n++;
    for(i=0;i<nbnode;i++)
      h2[i] = h[i]+dh0;
    iop = ehd_get_p(nbelem, nbnode, h2, eta, x,
                    u, h_t0, dt, p, dp, 
                    &K, nbfix,
                    nnfix, ndfix, vfix, EHD_NO_IO, scheme);
    if(iop!=0) goto FIN;
    residu = p[nL]-pL;
    fprintf(fich,"dh(%d)=%E;\n",n,dh0);
    fprintf(fich,"r(%d)=%E;\n",n,residu);
  }
  fclose(fich);
  exit(1);
#endif

  dh0t = 0.0;
  rester = 1;
  n=0;
  while(rester) { 

    // Resolution h(x) -> p(x)
    
    iop = ehd_get_p(nbelem, nbnode, h, eta0, alpha, x, um,
                    u, h_t0, dt, p, dp, 
                    &K, nbfix,
                    nnfix, ndfix, vfix, EHD_NO_IO, scheme);
    if(iop!=0) goto FIN;
    
    //iop = mlab_vec("pipo2.m", "p1", p, nbnode, MLAB_NEW, MLAB_VERBOSE);

    residu = p[nL]-pL;

    printf("ite %4d:\tres = %+E\t",n,residu);
    
    if(fabs(residu)<TOL_NR) {
      rester=0;
      break;
    }

    // Correction NR:
    // Resolution h(x), p(x) -> dp/dh0
    
    iop = ehd_get_dpdh0(nbelem, nbnode, h, eta0, alpha, x,
                        u, um, dt, p, dp, &K2, nbfix2,
                        nnfix2, ndfix2, vfix2, dpdh0, 
                        EHD_NO_IO, scheme);
    if(iop!=0) goto FIN;

    /*
    iop = mlab_vec("pipo2.m", "dpdh0", dpdh0, nbnode, MLAB_OLD, MLAB_VERBOSE);
    for(i=0;i<nbnode;i++)
      h[i]+=0.00001;
    iop = ehd_get_p(nbelem, nbnode, h, eta0, alpha, x,
                    u, h_t0, dt, p, dp, 
                    &K, nbfix,
                    nnfix, ndfix, vfix, EHD_NO_IO, scheme);
    if(iop!=0) goto FIN;
    iop = mlab_vec("pipo2.m", "p2", p, nbnode, MLAB_OLD, MLAB_VERBOSE);
    exit(1);
    */

    dh0 = -1.0/dpdh0[nL]*residu;
    dh0t += dh0;

    printf("dh0 = %+E\tdh0t = %+E\n",dh0,dh0t);

    for(i=0;i<nbnode;i++)
      h[i]+=dh0;

    n++;
  }

  printf("\n");

  // output solution

  iop = ehd_get_p(nbelem, nbnode, h, eta0, alpha, x, um,
                  u, h_t0, dt, p, dp, 
                  &K, nbfix,
                  nnfix, ndfix, vfix, EHD_IO, scheme);
  if(iop!=0) goto FIN;

  /*
  for(i=0;i<nbnode;i++)
    dpdh0[i] = 12.0*eta[i]*u[i]*x[nbnode-1-i]/(h[i]*h[i]*h[i]);
  iop = mlab_vec("pipo2.m", "val", dpdh0, nbnode, MLAB_NEW, MLAB_VERBOSE);
  */




  printf("\n");

    free(h);
    free(h_t0);
    free(eta);
    free(x);
    free(u);
    free(um);

    free(p);
    free(dp);
    free(dpdh0);

 FIN:
  if(iop>900)
    printf("\n\t-->"__FUNCTION__" in "__FILE__"\n");
  return iop;
  /*
 ERR1:
  printf("\nerreur: pas assez de memoire !");
  iop = 990;
  goto FIN;
  */
}
