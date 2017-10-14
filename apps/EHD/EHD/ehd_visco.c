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
 * Calcule la viscosite en fct de la pression
 * et sa derivee.
 */

#include "ehd.h"

int ehd_visco(double eta0, double alpha, double p, double *eta, double *etad)
{
    int iop = 0;

    *eta = eta0 * exp(alpha * p);

    *etad = alpha * (*eta);

    //FIN:
    if (iop > 900)
        printf("\n\t-->"__FILE__
               "\n");
    return iop;
}
