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
 * Calcul des flow factors de cisaillement
 *
 * ATTENTION : routine ecrite pour gam = 1.0
 */

#include "ehd.h"

EHD_API int ehd_flow_cisail(double h,
                    double Rq, double Rq1, double Rq2, int loi,
                    double *PhiF, double *PhiFS, double *PhiFP)
{

    int iop = 0;

    double z, z2, h2;
    double va, va2, va3;
    double tmp;
    double A3, al4, al5, al6;
    double D, s;

    switch (loi)
    {

    /*
     *  Surfaces lisses
     *  ---------------
     */

    case EHD_LISSE:

        *PhiF = 1.0;
        *PhiFS = 0.0;
        *PhiFP = 0.0;

        break;

    /*
     *  Flow Factors de Patir & Cheng / Tripp
     *  -------------------------------------
     */

    case EHD_PATIR:
    case EHD_TRIPP:

        //
        // PhiF
        //

        z = h / Rq / 3.0;

        va = 1.0 - z * z;
        va3 = va * va * va;

        if (z <= 1.0)
        {

            va2 = log(300.0 * (z + 1.0));

            tmp = 60.0e0 + z * 147.0e0;
            tmp = -405.0e0 + z * tmp;
            tmp = -160.0e0 + z * tmp;
            tmp = 345.0e0 + z * tmp;
            tmp = 132.0e0 + z * tmp;
            tmp = -55.0e0 + z * tmp;
            tmp = 35.0e0 / 32.0e0 * z * (va3 * va2 + tmp / 60.0e0);

            *PhiF = tmp;
        }
        else
        {

            if (z < 10.0)
            {

                va2 = log((z + 1.0) / (z - 1.0));
                z2 = z * z;

                tmp = 30.0e0 * z2 - 80.0e0;
                tmp = 66.0e0 + z2 * tmp;
                tmp = 35.0e0 / 32.0e0 * z * (va3 * va2 + z / 15.0e0 * tmp);

                *PhiF = tmp;
            }
            else
            {

                *PhiF = 1.0;
            }
        }

        //
        // PhiFS
        //

        h2 = h / Rq;

        if (h2 < 7.0)
        {

            A3 = 11.10;
            al4 = 2.31;
            al5 = 2.38;
            al6 = 0.11;

            *PhiFS = A3 * pow(h2, al4) * exp(-al5 * h2 + al6 * h2 * h2);
        }
        else
        {

            *PhiFS = 0.0;
        }

        //
        // PhiFP
        //

        D = 1.4;
        s = 0.66;

        *PhiFP = 1.0 - D * exp(-s * h2);

        break;

    default:
        goto ERR1;
    }

    /*
   *  Recalcul de PhiFS en fct des rugosites des deux surfaces
   *  (on suppose un meme Peklenik)
   */

    va = (Rq1 * Rq1 - Rq2 * Rq2) / (Rq * Rq);

    (*PhiFS) *= va;

/* ------------------------------------------------------------ */

FIN:
    if (iop > 900)
        printf("\n\t-->" __FUNCTION__
               " in " __FILE__
               "\n");
    return iop;
ERR1:
    printf("Modele de rugosite non implemente !\n");
    iop = 990;
    goto FIN;
}
