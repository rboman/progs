/*
 * Module EHD
 */


#ifndef __EHD_H__
#define __EHD_H__


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "gausslib.h"
#include "skylib.h"
#include "tdilib.h"
#include "mlab.h"


// MACROS

#define EHD_NO_IO 0
#define EHD_IO    1

#define EHD_STATIO 0
#define EHD_EULER  1

#define EHD_LISSE 0
#define EHD_PATIR 1
#define EHD_TRIPP 2


// PROTOS
int ehd_flux(double h, double u, double v, 
             double eta0, double alpha, 
             double p, double dp, 
             double Rq, 
             double phis, double phip,
             double dphis, double dphip,
             double *flux, double *fluxd);
int ehd_init();
int ehd_mat_p(double *x, double *h, double eta0, double alpha,
              double *p, double *dp, double *u, double *um,
              double Sp[4][4], double Se[4][4], double Fu[4], 
              double C1[4][2], double Fum[4]);

int ehd_preass(S_SKYMAT *K, int *loc2, int nbelem, int nsys, int nddl);
int ehd_setpar(int nn, double *x, double *h, double *h_t0, double *um,
               double *eta0, double *alpha, double *u, double *dt);
int ehd_mat_dp(double *x, double *h, double eta0, double alpha, double *u, double *um, 
               double *p, double *dp, double Sp[2][2], double Se[2][2], 
               double Fu[2], double C1[2][2], double Fum[2]);

int ehd_get_p(int nbelem, int nbnode, double *h, double eta0, 
              double alpha, double *x, double *um,
              double *u, double *h_t0, double dt, 
              double *p, double *dp, S_SKYMAT *K, int nbfix,
              int *nnfix, int *ndfix, double *vfix, int opt, int scheme);
int ehd_get_dpdh0(int nbelem, int nbnode, double *h, double eta0, double alpha, 
                  double *x, double *u, double *um, double dt, 
                  double *p, double *dp, S_SKYMAT *K, int nbfix,
                  int *nnfix, int *ndfix, double *vfix, double *dpdh0,
                  int opt, int scheme);
int ehd_visco(double eta0, double alpha, double p, double *eta, double *etad);
int ehd_spp(double Sp[4][4], double *p, double *dp, double *res);




// splines

int ehd_spline_seg(double *xi, double *yi, double *ki, 
                   double *x, double *y, double *yp, int n);
int ehd_spline_y(int nn, double *xi, double *yi, double *ki, 
                 double x, double *y, double *yp);
int ehd_spline_ki(S_TDIMAT *K,int nn, double *xi, double *yi, double *ki);


#endif // __EHD_H__





