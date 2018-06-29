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
 * @brief Cherche dp/dh0
 */
EHD_API int ehd_get_dpdh0(int nbelem, int nbnode, double *h, double eta0, double alpha,
                          double *x, double *u, double *um, double dt,
                          double *p, double *dp, SkyMat *K, int nbfix,
                          int *nnfix, int *ndfix, double *vfix, double *dpdh0,
                          int opt, int scheme)
{
    int iop = 0;

    // locels
    int *loc = (int *)malloc(nbnode * sizeof(int));
    int *loc2 = (int *)malloc(nbnode * sizeof(int));
    int *locs = (int *)malloc(nbnode * sizeof(int));

    // matrices
    double Sp[2][2], Fu[2], C1[2][2], Fh[2], Se[2][2], Fum[2];

    double *rhs = (double *)malloc(nbnode * sizeof(double));
    double *inc = (double *)malloc(nbnode * sizeof(double));

    // CALCUL DES LOCELS

    // init
    for (int i = 0; i < nbnode; i++)
    {
        loc[i] = i;
        loc2[i] = 0;
        locs[i] = 0;
    }

    // supprime fix et code la valeur
    for (int i = 0; i < nbfix; i++)
        loc2[nnfix[i] + ndfix[i]] = -1 - i;

    // calcule le locs et le loc2

    int nsys = 0;
    for (int i = 0; i < nbnode; i++)
        if (loc2[i] >= 0)
        {
            loc2[i] = nsys;
            locs[nsys] = i;
            nsys++;
        }

    /*
  printf("nsys=%d\n",nsys);
  printf("nddl=%d\n",nbnode);
  */
    // Init de la ligne de ciel & init matrice

    iop = ehd_preass(K, loc2, nbelem, nsys, 1);

    // init 2nd membre

    for (int i = 0; i < nbnode; i++)
        rhs[i] = 0.0;

    // init vecteur inc (facultatif)

    for (int i = 0; i < nbnode; i++)
        inc[i] = 0.0;

    // Construction du systeme

    for (int n = 0; n < nbelem; n++)
    {
        // calcul de Sp(elem) et Fu(elem)
        iop = ehd_mat_dp(&(x[n]),
                         &(h[n]),
                         eta0, alpha,
                         &(u[n]), &(um[n]), &(p[n]), &(dp[n]),
                         Sp, Se, Fu, C1, Fum);

        for (int i = 0; i < 2; i++)
        {
            Fh[i] = 0.0;
            for (int j = 0; j < 2; j++)
                Fh[i] += C1[i][j] / dt;
        }

        // assemblage
        for (int i = 0; i < 2; i++)
        {
            int ni;
            if ((ni = loc2[n + i]) < 0)
                continue;

            rhs[ni] += Fu[i];

            if (scheme == EHD_EULER)
                rhs[ni] -= Fh[i];

            rhs[ni] -= Fum[i];

            for (int j = 0; j < 2; j++)
            {
                int nj;
                if ((nj = loc2[n + j]) < 0)
                {
                    rhs[ni] += -(Sp[i][j]) * vfix[-loc2[n + j] - 1];
                    rhs[ni] += -(Se[i][j]) * vfix[-loc2[n + j] - 1];
                    //printf("apply cl (%d) on ddl %d (%E)\n",n+j,ni,vfix[-loc2[n+j]-1]);
                    continue;
                }
                sky_ass(K, ni, nj, Sp[i][j]);
                sky_ass(K, ni, nj, Se[i][j]);
            }
        }
    }

    // Gestion des CL en 0 et (nbnode-1)

    int n = 0;
    double nor = x[n + 1] - x[n];
    double sign = 1.0;
    if (nor > 0.0)
        sign = 1.0;

    if (loc2[n] >= 0)
    {
        int nj = loc2[n];
        double eta, etad;
        ehd_visco(eta0, alpha, p[n], &eta, &etad);

        double flux = u[n] - h[n] * h[n] / 4.0 / eta * dp[n];
        rhs[nj] += sign * flux;

        sky_ass(K, nj, nj, -sign * h[n] * h[n] * h[n] / 12.0 / eta / eta * etad * dp[n]);
        //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[ni]);
    }

    n = nbnode - 1;
    nor = x[n] - x[n - 1];
    sign = 1.0;
    if (nor > 0.0)
        sign = 1.0;

    if (loc2[n] >= 0)
    {
        int nj = loc2[n];
        double eta, etad;
        ehd_visco(eta0, alpha, p[n], &eta, &etad);

        double flux = u[n] - h[n] * h[n] / 4.0 / eta * dp[n];
        rhs[nj] += -sign * flux;

        flux = sign * h[n] * h[n] * h[n] / 12.0 / eta / eta * etad * dp[n];
        sky_ass(K, nj, nj, flux);
        //printf("apply flux on node %d : %E / %E\n",n,flux,rhs[ni]);
        //printf("** dp = %E (flux=%E) %E\n",dp[n],flux, etad/eta);
    }

    if (opt == EHD_IO)
    {
        iop = mlab_sky("syst2.m", "", K, SKY_A, MLAB_NEW, MLAB_SILENT);
    }

    // Solution systeme

    iop = sky_solve(K, rhs, inc, SKY_DO_LU | SKY_DO_SUBST);
    /*
    if (iop != 0)
    {
        sky_print_err(stdout, iop);
        goto FIN;
    }*/

    if (opt == EHD_IO)
    {
        iop = mlab_vec("pipo2.m", "inc", inc, nsys, MLAB_NEW, MLAB_SILENT);
        iop = mlab_vec("pipo2.m", "rhs", rhs, nsys, MLAB_OLD, MLAB_SILENT);
    }

    // extraction dpdh0

    for (int i = 0; i < nbnode; i++)
    {
        if (loc2[i] >= 0)
            dpdh0[i] = inc[loc2[i]];
        else
            dpdh0[i] = vfix[-loc2[i] - 1];
    }

    free(loc);
    free(loc2);
    free(locs);
    free(rhs);
    free(inc);

    return iop;
}
