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
 * Gauss : routines communes a tous les types de domaines d'integration
 */

#include "gausslib.h"

/*
 *   QUAD BI-LINEAIRE 2D/3D
 */

double *quad_xg[GAUSS_MAX_NG];
double *quad_pg[GAUSS_MAX_NG];
double ***quad_psi[GAUSS_MAX_NG];

/*
 *   SEGMENT LINEAIRE 1D/2D/3D
 */

double *line_xg[GAUSS_MAX_NG];
double *line_pg[GAUSS_MAX_NG];
double ***line_psi[GAUSS_MAX_NG];

/*
 *   HEXAEDRE TRI-LINEAIRE 3D
 */

double *hexa_xg[GAUSS_MAX_NG];
double *hexa_pg[GAUSS_MAX_NG];
double ***hexa_psi[GAUSS_MAX_NG];

/*
 *   ELEM HERMITE 2 NOEUDS (CUBIQUE) // n'est pas ajoute au cas "generique"
 */

// hermite_xg = line_xg
// hermite_pg = line_pg
double ***hermite_psi[GAUSS_MAX_NG];

/*
 *   ROUTINE GENERALE
 */

double *generic_xg[GAUSS_MAX_EL][GAUSS_MAX_NG];
double *generic_pg[GAUSS_MAX_EL][GAUSS_MAX_NG];
double ***generic_psi[GAUSS_MAX_EL][GAUSS_MAX_NG];

/* -------------------------------------------------------------------------- */

GAUSS_API int
gauss_common_init()
{
    int i, j;

    // INITIALISATIONS

    for (i = 0; i < GAUSS_MAX_NG; i++)
    {
        line_xg[i] = NULL;
        line_pg[i] = NULL;
        line_psi[i] = NULL;

        quad_xg[i] = NULL;
        quad_pg[i] = NULL;
        quad_psi[i] = NULL;

        hexa_xg[i] = NULL;
        hexa_pg[i] = NULL;
        hexa_psi[i] = NULL;

        hermite_psi[i] = NULL;

        for (j = 0; j < GAUSS_MAX_EL; j++)
        {
            generic_xg[j][i] = NULL;
            generic_pg[j][i] = NULL;
            generic_psi[j][i] = NULL;
        }
    }

    printf("Initialisation de la librairie \"Gauss\".\n");

    return 0;
}

/* -------------------------------------------------------------------------- */

GAUSS_API int
gauss_common_pp(double *xg, double *wg, int ng)
{
    /* adaptee de "ppgo8.f", cette routine renvoie les positions
     * et poids de gauss si on lui donne le nbre de points voulu
     * 'ng'.
     *
     * Attention: xg[] et wg[] doivent etre correctement
     *            dimensionnes.
     *
     */
    int iop = 0;
    int i, j;
    double X[55] = {
        5.0000000000000000e-01, 2.1132486540518712e-01, 7.8867513459481288e-01,
        1.1270166537925831e-01, 5.0000000000000000e-01, 8.8729833462074169e-01,
        6.9431844202973714e-02, 3.3000947820757187e-01, 6.6999052179242813e-01,
        9.3056815579702629e-01, 4.6910077030668004e-02, 2.3076534494715845e-01,
        5.0000000000000000e-01, 7.6923465505284155e-01, 9.5308992296933200e-01,
        3.3765242898423986e-02, 1.6939530676686775e-01, 3.8069040695840155e-01,
        6.1930959304159845e-01, 8.3060469323313225e-01, 9.6623475710157601e-01,
        2.5446043828620738e-02, 1.2923440720030278e-01, 2.9707742431130142e-01,
        5.0000000000000000e-01, 7.0292257568869858e-01, 8.7076559279969722e-01,
        9.7455395617137926e-01, 1.9855071751231884e-02, 1.0166676129318664e-01,
        2.3723379504183550e-01, 4.0828267875217510e-01, 5.9171732124782490e-01,
        7.6276620495816450e-01, 8.9833323870681336e-01, 9.8014492824876812e-01,
        1.5919880246186955e-02, 8.1984446336682101e-02, 1.9331428364970479e-01,
        3.3787328829809554e-01, 5.0000000000000000e-01, 6.6212671170190446e-01,
        8.0668571635029521e-01, 9.1801555366331790e-01, 9.8408011975381304e-01,
        1.3046735741414140e-02, 6.7468316655507746e-02, 1.6029521585048780e-01,
        2.8330230293537639e-01, 4.2556283050918439e-01, 5.7443716949081561e-01,
        7.1669769706462361e-01, 8.3970478414951220e-01, 9.3253168334449225e-01,
        9.8695326425858586e-01};

    double W[55] = {
        1.0000000000000000e+00, 5.0000000000000000e-01, 5.0000000000000000e-01,
        2.7777777777777778e-01, 4.4444444444444443e-01, 2.7777777777777778e-01,
        1.7392742256872692e-01, 3.2607257743127308e-01, 3.2607257743127308e-01,
        1.7392742256872692e-01, 1.1846344252809454e-01, 2.3931433524968324e-01,
        2.8444444444444444e-01, 2.3931433524968324e-01, 1.1846344252809454e-01,
        8.5662246189585178e-02, 1.8038078652406930e-01, 2.3395696728634552e-01,
        2.3395696728634552e-01, 1.8038078652406930e-01, 8.5662246189585178e-02,
        6.4742483084434851e-02, 1.3985269574463834e-01, 1.9091502525255948e-01,
        2.0897959183673469e-01, 1.9091502525255948e-01, 1.3985269574463834e-01,
        6.4742483084434851e-02, 5.0614268145188130e-02, 1.1119051722668724e-01,
        1.5685332293894363e-01, 1.8134189168918100e-01, 1.8134189168918100e-01,
        1.5685332293894363e-01, 1.1119051722668724e-01, 5.0614268145188130e-02,
        4.0637194180787206e-02, 9.0324080347428698e-02, 1.3030534820146773e-01,
        1.5617353852000142e-01, 1.6511967750062988e-01, 1.5617353852000142e-01,
        1.3030534820146773e-01, 9.0324080347428698e-02, 4.0637194180787206e-02,
        3.3335672154344069e-02, 7.4725674575290293e-02, 1.0954318125799102e-01,
        1.3463335965499817e-01, 1.4776211235737644e-01, 1.4776211235737644e-01,
        1.3463335965499817e-01, 1.0954318125799102e-01, 7.4725674575290293e-02,
        3.3335672154344069e-02};

    if (ng > 10 || ng < 1)
        goto ERR1;

    j = (ng - 1) * ng / 2 - 1;
    for (i = 0; i < ng; i++)
    {
        j += 1;
        xg[i] = X[j] + X[j] - 1.0;
        wg[i] = W[j] + W[j];
    }

FIN:
    if (iop > 900)
        printf("\n\t-->" __FILE__ "\n");
    return iop;

ERR1:
    printf(
        "\nErreur: nombre de pts de Gauss demande (%d) hors des limites (1-10)",
        ng);
    iop = 990;
    goto FIN;
}
