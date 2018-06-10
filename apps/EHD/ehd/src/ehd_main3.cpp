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
 * Test des flow factors
 */

#include "ehd_main3.h"

Main3::Main3(int _nn) : nn(_nn), h(_nn), PhiS(_nn), PhiP(_nn), dPhiS(_nn), dPhiP(_nn)
{
}

int Main3::execute()
{
    int iop = 0;
    int loi = EHD_PATIR;
    //const int nn = 100;
    double h_max = 10;
    double h_min = 1;

    // Calcule l'abscisse
    for (int i = 0; i < nn; i++)
        h[i] = h_min + (h_max - h_min) * (double)i / ((double)(nn - 1));

    double Rq = 1.0, Rq1 = 1.0, Rq2 = 0.0;
    double gam_s = 1.0 / 9.0;


    // Evaluation

    for (int i = 0; i < nn; i++)
    {
        iop = ehd_flow_factors(h[i], gam_s,
                               Rq, Rq1, Rq2,
                               &(PhiP[i]), &(PhiS[i]),
                               &(dPhiP[i]), &(dPhiS[i]), loi);
        if (iop != 0)
            goto FIN;
    }

    // Resultats vers matlab

    iop = mlab_vec("pipo.m", "h", &(h[0]), nn, MLAB_NEW, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "PhiP", &(PhiP[0]), nn, MLAB_OLD, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "PhiS", &(PhiS[0]), nn, MLAB_OLD, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "dPhiP", &(dPhiP[0]), nn, MLAB_OLD, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "dPhiS", &(dPhiS[0]), nn, MLAB_OLD, MLAB_VERBOSE);
    if (iop != 0)
        goto FIN;

FIN:
    if (iop > 900)
        printf("\n\t-->" __FUNCTION__ " in " __FILE__
                                      "\n");
    return iop;
}
