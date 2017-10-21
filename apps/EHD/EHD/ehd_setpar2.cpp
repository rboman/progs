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
 * Donnees du probleme
 */

#include "ehd.h"
#include "math.h"

int ehd_setpar2(int nn, double *x, double *h, double *h_t0,
                double *p, double *dp,
                double *eta0, double *alpha, double *u, double *um,
                double *v, double *dt)
{
    int iop = 0;
    int i;

    double L = 200.0;
    double e = 0.01;
    double R = 2.0;
    double xr = 0.9;
    double visc = 0.1;

    //double xx;

    S_TDIMAT K;
    double *dp2 = (double *)malloc(nn * sizeof(double));
    double y, yp;

// positions

#if 1
    for (i = 0; i < nn; i++)
    {
        x[i] = L * (double)i / (nn - 1);
    }
#else
    for (i = 0; i < nn; i++)
    {
        x[i] = L - L * (double)i / (nn - 1);
    }
#endif

    *eta0 = visc;
    *alpha = 1.0e-4;
    *alpha = 0.0;

    // pression / vitesses

    for (i = 0; i < nn; i++)
    {

        um[i] = 0.0;
        v[i] = 0.5;
        u[i] = 1.0;
        p[i] = 1.0e0 - 0.5e0 * (x[i] - L / 2.0) * (x[i] - L / 2.0) / L / L * 4.0;
        //p[i] = 0.0;
        //u[i] = 0.0;
    }

    // transitoire

    for (i = 0; i < nn; i++)
    {
        h_t0[i] = 1.0;
    }
    *dt = 1.0e0;

    /* -------------------------------------------------------------------- */

    // derivee de la pression (interp. spline)
    // ---------------------------------------

    // init

    iop = tdi_init(&K);
    if (iop != 0)
        goto FIN;

    iop = tdi_setname(&K, "K");
    if (iop != 0)
        goto FIN;

    // calcul des derivees secondes (dp2)

    iop = ehd_spline_ki(&K, nn, x, p, dp2);
    if (iop != 0)
        goto FIN;

    // evaluation spline (calcul des derivees dp)

    for (i = 0; i < nn; i++)
    {

        iop = ehd_spline_y(nn, x, p, dp2,
                           x[i], &y, &yp);
        if (iop != 0)
            goto FIN;

        dp[i] = yp;
    }

    // purge memoire du systeme tridiag

    iop = tdi_reinit(&K);
    if (iop != 0)
        goto FIN;

    /*
  iop = mlab_vec("pipo.m", "p", p, nn, MLAB_NEW, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "dp", dp, nn, MLAB_OLD, MLAB_VERBOSE);
  iop = mlab_vec("pipo.m", "x", x, nn, MLAB_OLD, MLAB_VERBOSE);
  if(iop!=0) goto FIN;
  */

    free(dp2);

FIN:
    if (iop > 900)
        printf("\n\t-->" __FUNCTION__
               " in " __FILE__
               "\n");
    return iop;
}
