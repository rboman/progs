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
#include <stdio.h>

/**
 * @brief Calcule le cisaillement (tau) en un point donne
 */

EHD_API int ehd_cisail(double eta0, double alpha,
                       double v, double p, double dp, double h,
                       double Rq, double Rq1, double Rq2,
                       int loi, double *tau)
{
    int iop = 0;

    // Calcul des "flow factors" : PhiF, PhiFS, PhiFP

    double PhiF, PhiFS, PhiFP;
    ehd_flow_cisail(h, Rq, Rq1, Rq2, loi,
                          &PhiF, &PhiFS, &PhiFP);

    // Viscosite

    double eta, etad;
    ehd_visco(eta0, alpha, p, &eta, &etad);

    // Evaluation de "tau"

    if (h != 0.0)
    {
        double tmp = eta * v / h;
        tmp *= PhiF + PhiFS;
        tmp += h / 2.0 * PhiFP * dp;

        *tau = -tmp;
    }
    else
    {
        std::stringstream str; str << __FUNCTION__ << " in " << __FILE__ << ": ";
        str << "Epaisseur de film nulle -> division par 0 !";
        throw std::runtime_error(str.str());
    }

    return iop;
}
