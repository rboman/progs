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
 * Calcule le flux de l'equ. de Reynolds & ses derivees
 */

#include "ehd.h"

#undef VERBOSE
#define VERBOSE 0

int ehd_flux(double h, double u, double v,
             double eta0, double alpha,
             double p, double dp,
             double Rq,
             double phis, double phip,
             double dphis, double dphip,
             double *flux, double *fluxd)
{
    int iop = 0;
    double eta, etad;

    iop = ehd_visco(eta0, alpha, p, &eta, &etad);
    if (iop != 0)
        goto FIN;

    *flux = -phip * h * h * h / 12.0 / eta * dp + h * u + v * Rq / 2.0 * phis;

    fluxd[0] = phip * h * h * h / 12.0 / eta / eta * etad * dp;                                                   // d(flux)/d(p)
    fluxd[1] = -phip * h * h * h / 12.0 / eta;                                                                    // d(flux)/d(dp)
    fluxd[2] = -(phip * h * h / 4.0 / eta * dp + dphip * h * h * h / 12.0 / eta * dp) + u + v * Rq / 2.0 * dphis; // d(flux)/d(h)

    if (VERBOSE)
    {
        printf("h = %E\n", h);
        printf("dp = %E\n", dp);
        printf("p = %E\n", p);
        printf("eta = %E\n", eta);
        printf("etad = %E\n", etad);
        printf("u = %E\n", u);
        printf("flux = %E\n", *flux);
        printf("d(flux)/d(p) = %E\n", fluxd[0]);
        printf("d(flux)/d(dp) = %E\n", fluxd[1]);
        printf("d(flux)/d(h) = %E\n", fluxd[2]);
    }
FIN:
    if (iop > 900)
        printf("\n\t-->"__FILE__
               "\n");
    return iop;
}
