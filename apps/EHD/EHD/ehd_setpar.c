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

int ehd_setpar(int nn, double *x, double *h, double *h_t0, double *um,
               double *eta0, double *alpha, double *u, double *dt)
{
    int iop = 0;
    int i;

    double L = 200.0;
    double e = 0.01;
    double R = 2.0;
    double xr = 0.9;
    double visc = 0.1;

    double xx;

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

    // epaisseurs de film

    for (i = 0; i < nn; i++)
    {
#if 1
        u[i] = 1.0;
        h[i] = 0.1e-1 - 0.5e-3 * (x[i] - L);
        //xx = x[i]-xr;
        //h[i] = R+e-sqrt(R*R-xx*xx);
        um[i] = 0.0;
//eta[i] = visc;
#else
        u[i] = -1.0;
        h[i] = 6.0e0 - 0.5e-3 * x[i] / L;
        xx = L - x[i] - xr;
        h[i] = R + e - sqrt(R * R - xx * xx);
        um[i] = 0.0;
//eta[i] = visc;
#endif
    }

    *dt = 1.0e-1;
    for (i = 0; i < nn; i++)
    {
        h_t0[i] = h[i] + 2 * e;
    }

    return iop;
}
