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
 * @brief Calcul des matrices C1,Sp et du vect Fu
 */

EHD_API int
ehd_mat_p(double *x, double *h, double eta0, double alpha, double *p,
          double *dp, double *u, double *um, double Sp[4][4], double Se[4][4],
          double Fu[4], double C1[4][2], double Fum[4])
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

    // recupere les valeurs des fct de forme cubiques & derivees (->psih)

    double ***psih;
    iop = gauss_hermite_get_psi(ng, &psih, xg);

    // initialisation

    for (int i = 0; i < 4; i++)
    {
        Fu[i] = 0.0;
        Fum[i] = 0.0;
        for (int j = 0; j < 4; j++)
        {
            Sp[i][j] = 0.0;
            Se[i][j] = 0.0;
        }
        for (int j = 0; j < 2; j++)
        {
            C1[i][j] = 0.0;
        }
    }

    // BOUCLE D'INTEGRATION DE GAUSS

    for (int n = 0; n < ng; n++)
    {
        // valeur de h, u au pt de gauss -> h_g, u_g
        double h_g;
        gauss_line_getf(n, psil, h, &h_g);
        double u_g;
        gauss_line_getf(n, psil, u, &u_g);

        // jacobien de l'elem ( ! tjs >0 par "libgauss")
        double jaco[3][3];
        gauss_line_jaco(xx, jaco, n, xg, psil, 1);
        double detj;
        el_line_detj(jaco, 1, &detj);

        // evaluation de p et dp/dx au pt de Gauss
        double p_g = psih[0][0][n] * p[0] + psih[0][1][n] * dp[0] * detj +
                     psih[0][2][n] * p[1] + psih[0][3][n] * dp[1] * detj;
        double dpdx_g = psih[1][0][n] * p[0] / detj + psih[1][1][n] * dp[0] +
                        psih[1][2][n] * p[1] / detj + psih[1][3][n] * dp[1];
        double dum_g = (psil[1][0][n] * um[0] + psil[1][1][n] * um[1]) / detj;

        // viscosite eta=f(p)
        double eta_g, etad_g;
        ehd_visco(eta0, alpha, p_g, &eta_g, &etad_g);

        // correction des fformes 2 et 4
        double cor[4] = {1.0, 1.0, 1.0, 1.0};
        cor[1] = detj;
        cor[3] = detj;

        // valeurs temporaires
        double tmp = pg[n] * detj;
        double tmp01 = h_g * h_g * h_g / (12.0 * eta_g);
        double tmp02 = u_g * h_g;

        // BOUCLE SUR LES COMPOSANTES DE LA MATRICE ELEMENTAIRE

        for (int i = 0; i < 4; i++)
        {
            double Ni = psih[0][i][n] * cor[i];
            double dNi = psih[1][i][n] * cor[i] / detj; // dNi/dx

            Fu[i] += tmp02 * dNi * tmp;
            Fum[i] += h_g * dum_g * Ni * tmp;

            for (int j = 0; j < 4; j++)
            {
                double Nj = psih[0][j][n] * cor[j];
                double dNj = psih[1][j][n] * cor[j] / detj; // dNj/dx

                Sp[i][j] += tmp01 * dNi * dNj * tmp;
                Se[i][j] += -tmp01 * etad_g / eta_g * dpdx_g * dNi * Nj * tmp;
            }

            for (int j = 0; j < 2; j++)
            {
                double Nj = psil[0][j][n];
                C1[i][j] += Ni * Nj * tmp;
            }
        }
    } // endfor(n)

    // Infos de debug

    if (VERBOSE)
    {
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                printf("%d,%d = %E\n", i, j, Sp[i][j]);
    }

    return iop;
}
