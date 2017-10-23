/*
 *   Copyright 2000-2017 Romain Boman
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

/*
 * Module EHD
 */

#ifndef __EHD_H__
#define __EHD_H__

#if defined(WIN32)
#ifdef ehd_EXPORTS
#define EHD_API __declspec(dllexport)
#else
#define EHD_API __declspec(dllimport)
#endif
#else
#define EHD_API
#endif

#ifdef _MSC_VER
#if !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS 1
#endif

#pragma warning( disable : 4251)  // DLL/templates non exportes
#pragma warning( disable : 4275)  // non - DLL-interface classkey 'identifier' used as base for DLL-interface classkey 'identifier'

#endif //_MSC_VER


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "gausslib.h"
#include "SkyMat.h"
#include "TdiMat.h"
#include "mlab.h"

// MACROS

#define EHD_NO_IO 0
#define EHD_IO 1

#define EHD_STATIO 0
#define EHD_EULER 1

#define EHD_LISSE 0
#define EHD_PATIR 1
#define EHD_TRIPP 2

// PROTOS
EHD_API int ehd_flux(double h, double u, double v,
                     double eta0, double alpha,
                     double p, double dp,
                     double Rq,
                     double phis, double phip,
                     double dphis, double dphip,
                     double *flux, double *fluxd);
EHD_API int ehd_init();
EHD_API int ehd_mat_p(double *x, double *h, double eta0, double alpha,
                      double *p, double *dp, double *u, double *um,
                      double Sp[4][4], double Se[4][4], double Fu[4],
                      double C1[4][2], double Fum[4]);

EHD_API int ehd_preass(SkyMat *K, int *loc2, int nbelem, int nsys, int nddl);
EHD_API int ehd_setpar(int nn, double *x, double *h, double *h_t0, double *um,
                       double *eta0, double *alpha, double *u, double *dt);
EHD_API int ehd_mat_dp(double *x, double *h, double eta0, double alpha, double *u, double *um,
                       double *p, double *dp, double Sp[2][2], double Se[2][2],
                       double Fu[2], double C1[2][2], double Fum[2]);

EHD_API int ehd_get_p(int nbelem, int nbnode, double *h, double eta0,
                      double alpha, double *x, double *um,
                      double *u, double *h_t0, double dt,
                      double *p, double *dp, SkyMat *K, int nbfix,
                      int *nnfix, int *ndfix, double *vfix, int opt, int scheme);
EHD_API int ehd_get_dpdh0(int nbelem, int nbnode, double *h, double eta0, double alpha,
                          double *x, double *u, double *um, double dt,
                          double *p, double *dp, SkyMat *K, int nbfix,
                          int *nnfix, int *ndfix, double *vfix, double *dpdh0,
                          int opt, int scheme);
EHD_API int ehd_visco(double eta0, double alpha, double p, double *eta, double *etad);
EHD_API int ehd_spp(double Sp[4][4], double *p, double *dp, double *res);

// splines

EHD_API int ehd_spline_seg(double *xi, double *yi, double *ki,
                           double *x, double *y, double *yp, int n);
EHD_API int ehd_spline_y(int nn, double *xi, double *yi, double *ki,
                         double x, double *y, double *yp);
EHD_API int ehd_spline_ki(TdiMat *K, int nn, double *xi, double *yi, double *ki);

//

EHD_API int ehd_flow_factors(double h, double gam_s,
                             double Rq, double Rq1, double Rq2,
                             double *PhiP, double *PhiS,
                             double *dPhiP, double *dPhiS, int loi);
EHD_API int ehd_mat_h(double *x, double *h, double *u,
                      double *um, double *v, double eta0, double alpha,
                      double *p, double *dp,
                      double *PhiS, double *dPhiS,
                      double *PhiP, double *dPhiP,
                      double Su[2][2], double Sp[2][2],
                      double dSu[2][2], double dSp[2][2],
                      double C1[2][2], double C2[2][2],
                      double Sv[2][2], double dSv[2][2]);
EHD_API int ehd_cisail(double eta0, double alpha,
                       double v, double p, double dp, double h,
                       double Rq, double Rq1, double Rq2,
                       int loi, double *tau);
EHD_API int ehd_setpar2(int nn, double *x, double *h, double *h_t0,
                        double *p, double *dp,
                        double *eta0, double *alpha, double *u, double *um,
                        double *v, double *dt);
EHD_API int ehd_get_h(int nbelem, int nbnode, double *h, double eta0, double alpha,
                      double *x, double *u, double *um, double *v,
                      double *h_t0, double dt,
                      double *p, double *dp,
                      double *PhiP, double *PhiS, double *dPhiP, double *dPhiS,
                      double Rq1, double Rq2, double gam_s,
                      TdiMat *K, int nbfix,
                      int *nnfix, int *ndfix, double *vfix, int opt,
                      int loi, int scheme);
EHD_API int ehd_flow_cisail(double h,
                            double Rq, double Rq1, double Rq2, int loi,
                            double *PhiF, double *PhiFS, double *PhiFP);

#endif // __EHD_H__
