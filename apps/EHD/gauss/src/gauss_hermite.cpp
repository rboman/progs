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
 * Integration d'une fonction sur un segment cubique 2D/3D
 * par la methode de Gauss
 *
 * VERSION TRES INCOMPLETE:
 *   - Je n'ai besoin que d'integrer des fct cubiques sur des lignes.
 *     Je n'ai donc besoin que de la fct "get_psi"
 *   - Cet ensemble de routine devrait deboucher a une integration
 *     de fct sur des "splines" cubiques.
 */

#include "gausslib.h"


GAUSS_API int
gauss_hermite_get_psi(int ng, double ****psi, double *xg)
{
    int iop = 0;
    int i, j, l, m;
    int ng1;
    double F[EL_HERMITE_NODE][4];
    double *c;

    ng1 = ng - 1;

    if (ng > GAUSS_MAX_NG)
        goto ERR2;

    // Calcul si pas encore fait

    if (hermite_psi[ng1] == NULL)
    {

        // allocation

        hermite_psi[ng1] =
            (double ***)calloc(1 + EL_HERMITE_DIM, sizeof(double **));
        if (hermite_psi[ng1] == NULL)
            goto ERR1;

        for (i = 0; i < 1 + EL_HERMITE_DIM; i++)
        {
            hermite_psi[ng1][i] =
                (double **)calloc(EL_HERMITE_NODE, sizeof(double *));
            if (hermite_psi[ng1][i] == NULL)
                goto ERR1;
            for (j = 0; j < EL_HERMITE_NODE; j++)
            {
                hermite_psi[ng1][i][j] = (double *)calloc(ng, sizeof(double));
                if (hermite_psi[ng1][i][j] == NULL)
                    goto ERR1;
            }
        }

        // remplissage

        for (i = 0; i < ng; i++)
        {

            c = &(xg[EL_HERMITE_DIM * i]);
            el_hermite_ff(F, c);

            for (l = 0; l < 1 + EL_HERMITE_DIM; l++)
                for (m = 0; m < EL_HERMITE_NODE; m++)
                    hermite_psi[ng1][l][m][i] = F[m][l];
        }
    } // endif

    // Retourne le pointeur

    *psi = hermite_psi[ng1];

    /***/

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__ "\n");
    return iop;

ERR1:
    printf("\nErreur: pas assez de memoire");
    iop = 990;
    goto FIN;
ERR2:
    printf("\nErreur: le nbre de points de Gauss demande est trop grand !");
    iop = 990;
    goto FIN;
}

