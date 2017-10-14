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
 *
 * Calcule le cisaillement (tau) en un point donne
 *
 *
 */

#include "ehd.h"

int ehd_cisail(double eta0, double alpha,
               double v, double p, double dp, double h,
               double Rq, double Rq1, double Rq2,
               int loi, double *tau)
{

    int iop = 0;

    double PhiF, PhiFS, PhiFP;
    double tmp;
    double eta, etad;

    /*
   *  Calcul des "flow factors" : PhiF, PhiFS, PhiFP
   */

    iop = ehd_flow_cisail(h, Rq, Rq1, Rq2, loi,
                          &PhiF, &PhiFS, &PhiFP);
    if (iop != 0)
        goto FIN;

    /*
   *  Viscosite
   */

    iop = ehd_visco(eta0, alpha, p, &eta, &etad);
    if (iop != 0)
        goto FIN;

    /*
   *  Evaluation de "tau"
   */

    if (h != 0.0)
    {

        tmp = eta * v / h;
        tmp *= PhiF + PhiFS;
        tmp += h / 2.0 * PhiFP * dp;

        *tau = -tmp;
    }
    else
    {

        goto ERR1;
    }

/* ------------------------------------------------------------ */

FIN:
    if (iop > 900)
        printf("\n\t-->"__FUNCTION__
               " in "__FILE__
               "\n");
    return iop;
ERR1:
    printf("Epaisseur de film nulle -> division par 0 !\n");
    iop = 990;
    goto FIN;
}
