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
 * @brief Calcul des matrices C1, Sp et du vect Fu
 */

EHD_API int ehd_mat_dp(double *x, double *h, double eta0, double alpha, double *u, double *um,
                       double *p, double *dp, double Sp[2][2], double Se[2][2],
                       double Fu[2], double C1[2][2], double Fum[2])
{

    int iop = 0;

    double *xx[2];
    xx[0] = &(x[0]);
    xx[1] = &(x[1]);

    int ng = 3;

    // recupere les pos & poids de Gauss (->xg, pg)

    double *xg, *pg;
    iop = gauss_line_get_xgpg(ng, &xg, &pg);
    if (iop != 0)
        goto FIN;

    // recupere les valeurs des fct de forme lineaires & derivees (->psil)
    double ***psil;
    iop = gauss_line_get_psi(ng, &psil, xg);
    if (iop != 0)
        goto FIN;

    // recupere les valeurs des fct de forme cubiques & derivees (->psih)
    double ***psih;
    iop = gauss_hermite_get_psi(ng, &psih, xg);
    if (iop != 0)
        goto FIN;

    // initialisation

    for (int i = 0; i < 2; i++)
    {
        Fu[i] = 0.0;
        Fum[i] = 0.0;
        for (int j = 0; j < 2; j++)
        {
            Sp[i][j] = 0.0;
            C1[i][j] = 0.0;
            Se[i][j] = 0.0;
        }
    }

    // BOUCLE D'INTEGRATION DE GAUSS

    for (int n = 0; n < ng; n++)
    {
        // valeur de h, eta, u au pt de gauss -> h_g, u_g
        double h_g;
        gauss_line_getf(n, psil, h, &h_g);
        double u_g;
        gauss_line_getf(n, psil, u, &u_g);

        // jacobien de l'elem ( ! tjs >0 par "libgauss")
        double jaco[3][3];
        gauss_line_jaco(xx, jaco, n, xg, psil, 1);
        double detj;
        el_line_detj(jaco, 1, &detj);

        // valeur dx/dx au pt de gauss

        double p_g = psih[0][0][n] * p[0] + psih[0][1][n] * dp[0] * detj + psih[0][2][n] * p[1] + psih[0][3][n] * dp[1] * detj;
        double dpdx_g = psih[1][0][n] * p[0] / detj + psih[1][1][n] * dp[0] + psih[1][2][n] * p[1] / detj + psih[1][3][n] * dp[1];
        double dum_g = (psil[1][0][n] * um[0] + psil[1][1][n] * um[1]) / detj;

        // viscosite eta=f(p)
        double eta_g, etad_g;
        ehd_visco(eta0, alpha, p_g, &eta_g, &etad_g);

        //printf("dpdx_g (%d)= %E (%E,%E,%E,%E)\n",n,dpdx_g,p[0],dp[0],p[1],dp[1]);
        // valeurs temporaires

        double tmp = pg[n] * detj;
        double tmp01 = h_g * h_g / (4.0 * eta_g);
        double tmp02 = tmp01 * h_g / 3.0;

        // BOUCLE SUR LES COMPOSANTES DE LA MATRICE ELEMENTAIRE

        for (int i = 0; i < 2; i++)
        {
            double Ni = psil[0][i][n];
            double dNi = psil[1][i][n] / detj; // dNi/dx

            Fu[i] += (u_g - tmp01 * dpdx_g) * dNi * tmp;
            Fum[i] += dum_g * Ni * tmp;

            for (int j = 0; j < 2; j++)
            {
                double Nj = psil[0][j][n];
                double dNj = psil[1][j][n] / detj; // dNj/dx

                Sp[i][j] += tmp02 * dNi * dNj * tmp;
                C1[i][j] += Ni * Nj * tmp;
                Se[i][j] += -tmp02 * etad_g / eta_g * dpdx_g * dNi * Nj * tmp;

            } // endfor(j)

        } // endfor(i)

    } // endfor(n)

    // ** TEST **
    /*
  for(i=0; i<4; i++) {
    Fu[i]/=2.0;
    for(j=0; j<4; j++)
      Sp[i][j]/=2.0;
  }
  */
    // Infos de debug

    if (VERBOSE)
    {
        for (int i = 0; i < 2; i++)
            for (int j = 0; j < 2; j++)
                printf("Sp: %d,%d = %E\n", i, j, Sp[i][j]);
        for (int i = 0; i < 2; i++)
            printf("Fu: %d = %E\n", i, Fu[i]);
    }

    /***/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__
               "\n");
    return iop;
}
