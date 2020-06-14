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
#include "gausslib.h"

#define VERBOSE 0

/**
 * @brief Calcul des matrices EF relative a la resolution inverse de Reynolds
 *
 * (les matrices indep de h pourrait etre evaluee 1 seule fois)
 */

EHD_API int
ehd_mat_h(double *x, double *h, double *u, double *um, double *v, double eta0,
          double alpha, double *p, double *dp, double *PhiS, double *dPhiS,
          double *PhiP, double *dPhiP, double Su[2][2], double Sp[2][2],
          double dSu[2][2], double dSp[2][2], double C1[2][2], double C2[2][2],
          double Sv[2][2], double dSv[2][2])
{

    int iop = 0;

    double *xx[2];
    xx[0] = &(x[0]);
    xx[1] = &(x[1]);

    int ng = 3;

    // recupere les pos & poids de Gauss (->xg, pg)
    double *xg, *pg;
    iop = gauss_line_get_xgpg(ng, &xg, &pg);

    // recupere les valeurs des fct de forme lineaires & derivees (->psil)
    double ***psil;
    iop = gauss_line_get_psi(ng, &psil, xg);

    // initialisation
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 2; j++)
        {
            Sp[i][j] = 0.0;
            Su[i][j] = 0.0;
            dSp[i][j] = 0.0;
            dSu[i][j] = 0.0;
            C1[i][j] = 0.0;
            C2[i][j] = 0.0;
            Sv[i][j] = 0.0;
            dSv[i][j] = 0.0;
        }

    // BOUCLE D'INTEGRATION DE GAUSS

    for (int n = 0; n < ng; n++)
    {

        // valeur de u au pt de gauss -> u_g
        double u_g;
        gauss_line_getf(n, psil, u, &u_g);
        double v_g;
        gauss_line_getf(n, psil, v, &v_g);

        double s_g = 1.0;
        if (u_g < 0)
            s_g = -1.0;
        s_g *= 1.0;

        // valeur de h au pt de gauss -> h_g
        double h_g2;
        gauss_line_getf2(n, psil, h, &h_g2, s_g);
        double h_g;
        gauss_line_getf(n, psil, h, &h_g);

        // valeurs des flow factors -> phis_g, dphis_g, phip_g, dphip_g

        double phis_g;
        gauss_line_getf(n, psil, PhiS, &phis_g);
        double dphis_g;
        gauss_line_getf(n, psil, dPhiS, &dphis_g);
        double phip_g;
        gauss_line_getf(n, psil, PhiP, &phip_g);
        double dphip_g;
        gauss_line_getf(n, psil, dPhiP, &dphip_g);

        // valeur de p au pt de gauss -> p_g
        double p_g;
        gauss_line_getf(n, psil, p, &p_g);

        // viscosite eta=f(p)
        double eta_g, etad_g;
        ehd_visco(eta0, alpha, p_g, &eta_g, &etad_g);

        // jacobien de l'elem ( ! tjs >0 par "libgauss")
        double jaco[3][3];
        gauss_line_jaco(xx, jaco, n, xg, psil, 1);

        double detj;
        el_line_detj(jaco, 1, &detj);

        // valeur de dpdx au pt de gauss -> dp_g

        /*
    dpdx_g = psih[1][0][n] * p[0] /detj
      +      psih[1][1][n] * dp[0]
      +      psih[1][2][n] * p[1] /detj
      +      psih[1][3][n] * dp[1];
    */

        double dpdx_g =
            psil[1][0][n] * p[0] / detj + psil[1][1][n] * p[1] / detj;
        // printf("dpdx_g = %E\n",dpdx_g);

        double dumdx_g =
            psil[1][0][n] * um[0] / detj + psil[1][1][n] * um[1] / detj;

        // valeurs temporaires

        double tmp = pg[n] * detj;
        double tmp01 = h_g * h_g / (4.0 * eta_g);
        double tmp02 = u_g * h_g;
        double tmp03 = h_g / 3.0 * tmp01;
        double tmp04 = phip_g * tmp01 + dphip_g * tmp03;

        // BOUCLE SUR LES COMPOSANTES DE LA MATRICE ELEMENTAIRE

        for (int i = 0; i < 2; i++)
        {

            double Ni = psil[0][i][n];
            double dNi = psil[1][i][n] / detj;
            double Ni2 = psil[0][i][n] - s_g * psil[1][i][n];

            for (int j = 0; j < 2; j++)
            {

                double Nj = psil[0][j][n];
                double dNj = psil[1][j][n] / detj; // dNj/dx
                double Nj2 = psil[0][j][n] - s_g * psil[1][j][n];

                Sp[i][j] += phip_g * tmp03 * dNi * dNj * tmp;
                dSp[i][j] += tmp04 * dpdx_g * dNi * Nj * tmp;

                Su[i][j] += h_g2 * dNi * Nj * tmp;
                dSu[i][j] += u_g * dNi * Nj2 * tmp;

                C1[i][j] += Ni * Nj * tmp;
                C2[i][j] += dumdx_g * Ni * Nj * tmp;

                Sv[i][j] += 0.5 * phis_g * dNi * Nj * tmp;
                dSv[i][j] += 0.5 * dphis_g * v_g * dNi * Nj * tmp;

            } // endfor(j)

        } // endfor(i)

    } // endfor(n)

    // Infos de debug

    if (VERBOSE)
    {
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 2; j++)
                printf("Su %d,%d = %E\n", i, j, Su[i][j]);
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 2; j++)
                printf("dSu %d,%d = %E\n", i, j, dSu[i][j]);
    }

    return iop;
}
