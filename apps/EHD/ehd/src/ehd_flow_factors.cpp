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

#include "ehd.h"

/**
 * @brief Calcule les "flow factors"
 */

EHD_API int
ehd_flow_factors(double h, double gam_s, double Rq, double Rq1, double Rq2,
                 double *PhiP, double *PhiS, double *dPhiP, double *dPhiS,
                 int loi)
{
    int iop = 0;

    switch (loi)
    {

    // Surfaces lisses
    case EHD_LISSE:
    {
        *PhiP = 1.0;
        *PhiS = 0.0;

        *dPhiP = 0.0;
        *dPhiS = 0.0;

        break;
    }

    // Flow Factors de Patir & Cheng
    case EHD_PATIR:
    {
        double z = h / Rq;
        double xlg = log(gam_s);

        // il manque une partie de la courbe (PhiS pour h<=5)
        double As = 1.0766e0 - 0.37758e0 * xlg;
        double xex = exp(-z / 4.0);
        *PhiS = As * xex;
        *dPhiS = -As / (4.0 * Rq) * xex;

        if (gam_s < 1.0)
        {

            double Cx = 0.89679e0 - 0.26591e0 * xlg;
            double Rx =
                0.43006e0 - 0.10828e0 * gam_s + 0.23821e0 * gam_s * gam_s;
            double xex = exp(-Rx * z);
            *PhiP = 1.0 - Cx * xex;
            *dPhiP = Cx * Rx / Rq * xex;
        }
        else
        {
            double Cx = -0.10667e0 + 0.1075e0 * gam_s;
            double xex = Cx * pow(z, -1.5e0);
            *PhiP = 1.0 + xex;
            *dPhiP = -1.5 * xex / z / Rq;
        }

        break;
    }

    // Flow Factors de Tripp
    case EHD_TRIPP:
    {
        double xex = 3.0 / (1.0 + gam_s * gam_s);
        double xex2 = 3.0 * (1.0 - xex);

        double z = h / Rq;
        if (z < 1.0)
            z = 1.0;

        double z2 = z * z;
        double z3 = z * z2;
        *PhiP = 1.0 + xex2 / z2;
        *PhiS = xex / z;
        *dPhiP = -2.0 * xex2 / z3 / Rq;
        *dPhiS = -xex / z2 / Rq;

        break;
    }
    default:
        std::stringstream str;
        str << __FUNCTION__ << " in " << __FILE__ << ": ";
        str << "Modele de rugosite non implemente !";
        throw std::runtime_error(str.str());
    }

    // Recalcul de PhiS en fct des rugosites des deux surfaces
    // (on suppose un meme Peklenik)

    double xex = (Rq1 * Rq1 - Rq2 * Rq2) / (Rq * Rq);

    (*PhiS) *= xex;
    (*dPhiS) *= xex;

    return iop;
}
