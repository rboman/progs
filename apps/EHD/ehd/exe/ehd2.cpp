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
 * Test de la determination de h(x)
 */

#include "ehd.h"

int main()
{
    int iop = 0;
    int i;

    int npas = 10;
    const int nbelem = 100;
    const int nbnode = nbelem + 1;

    int scheme = EHD_STATIO;
    //int scheme=EHD_EULER;
    int loi = EHD_TRIPP;

    double Rq1 = 1.0e-1;
    double Rq2 = 1.0e-1;
    double Rq;
    double gam_s = 1.0;

    // variables
    double eta0, alpha;

    double *eta = (double *)malloc(nbnode * sizeof(double));
    double *x = (double *)malloc(nbnode * sizeof(double));
    double *u = (double *)malloc(nbnode * sizeof(double));
    double *um = (double *)malloc(nbnode * sizeof(double));
    double *v = (double *)malloc(nbnode * sizeof(double));
    double *p = (double *)malloc(nbnode * sizeof(double));
    double *dp = (double *)malloc(nbnode * sizeof(double));
    double *h_t0 = (double *)malloc(nbnode * sizeof(double));
    double *PhiP = (double *)malloc(nbnode * sizeof(double));
    double *dPhiP = (double *)malloc(nbnode * sizeof(double));
    double *PhiS = (double *)malloc(nbnode * sizeof(double));
    double *dPhiS = (double *)malloc(nbnode * sizeof(double));

    double dt;
    int nt;
    double ttot;

    // inc
    double *h = (double *)malloc(nbnode * sizeof(double));
    double *tau = (double *)malloc(nbnode * sizeof(double));

    TdiMat K;

    // fixations
    int nbfix = 1;          // nbre de fix
    int nnfix[2] = {0};     // no de noeud fixe (start=0)
    int ndfix[2] = {0};     // no de ddl fixe
    double vfix[2] = {1.0}; // valeur des fix

    /* ------------------------------------------------------------------ */

    if (scheme == EHD_STATIO)
        npas = 1;
    Rq = sqrt(Rq1 * Rq1 + Rq2 * Rq2);

    // Init du module d'integration de Gauss

    iop = gauss_common_init();
    if (iop != 0)
        goto FIN;

    // Init du module EHD

    iop = ehd_init();
    if (iop != 0)
        goto FIN;

    // Mise en place des donnees

    iop = ehd_setpar2(nbnode, x, h, h_t0, p, dp, &eta0, &alpha, u, um, v, &dt);
    if (iop != 0)
        goto FIN;

    iop = mlab_vec("pipo.m", "p", p, nbnode, MLAB_NEW, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "dp", dp, nbnode, MLAB_OLD, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "x", x, nbnode, MLAB_OLD, MLAB_VERBOSE);
    if (iop != 0)
        goto FIN;

    // Initialisation de la matrice d'iteration (tri-diag)

    iop = tdi_init(&K);
    if (iop != 0)
        goto FIN;
    iop = tdi_setname(&K, "K");
    if (iop != 0)
        goto FIN;
    iop = tdi_setsize(&K, nbnode);
    if (iop != 0)
        goto FIN;

    for (nt = 0, ttot = 0.0; nt < npas; nt++)
    {

        printf("PAS %d : t = %E\n", nt + 1, ttot += dt);

        // Resolution h(x) -> p(x)

        iop = ehd_get_h(nbelem, nbnode, h, eta0, alpha, x,
                        u, um, v, h_t0, dt, p, dp,
                        PhiP, PhiS, dPhiP, dPhiS,
                        Rq1, Rq2, gam_s,
                        &K, nbfix,
                        nnfix, ndfix, vfix, EHD_NO_IO, loi, scheme);
        if (iop != 0)
            goto FIN;

        // update h_t0

        for (i = 0; i < nbnode; i++)
            h_t0[i] = h[i];

        // calcul du cisaillement

        for (i = 0; i < nbnode; i++)
        {
            iop = ehd_cisail(eta0, alpha, v[i], p[i], dp[i], h[i],
                             Rq, Rq1, Rq2, loi, &(tau[i]));
            if (iop != 0)
                goto FIN;
        }

    } // endfor(nt)

    iop = mlab_vec("pipo.m", "h", h, nbnode, MLAB_OLD, MLAB_VERBOSE);
    iop = mlab_vec("pipo.m", "tau", tau, nbnode, MLAB_OLD, MLAB_VERBOSE);
    if (iop != 0)
        goto FIN;

    free(eta);
    free(x);
    free(u);
    free(um);
    free(v);
    free(p);
    free(dp);
    free(h_t0);
    free(PhiP);
    free(dPhiP);
    free(PhiS);
    free(dPhiS);

    free(h);
    free(tau);

/***/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FUNCTION__
               " in " __FILE__
               "\n");
    return iop;
}
