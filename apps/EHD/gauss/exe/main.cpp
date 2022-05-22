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
 * Routine de test de la lib d'integration de Gauss
 */

#include "gausslib.h"
#include <math.h>

/*
 *   FONCTIONS "TEST"
 */

int
func(double *ksi, double *x, void *par, int i, double *val)
{
    double **xx;

    // recup des donnes
    xx = (double **)par;

    *val = sin(x[0]);
    return 0;
}

int
func2(double *ksi, double *x, void *par, int i, double *val)
{
    //  double **xx;

    *val = 1.0;
    return 0;
}

int
func3(double *ksi, double *x, void *par, int i, double *val)
{
    //  double **xx;

    *val = x[0] * x[1] * x[2];
    return 0;
}

/* -------------------------------------------------------------------------- */

/*
 *    TEST QUAD (3D)
 */

int
test_quad()
{
    int iop = 0, i;
    double x1[3], x2[3], x3[3], x4[3];
    double res = 0.0;
    double *xx[4];

    void *pipo = nullptr;

    xx[0] = x1;
    xx[1] = x2;
    xx[2] = x3;
    xx[3] = x4;

#if 0
  x1[2] = 0.0;
  x1[0] = 0.0;
  x1[1] = 0.0;

  x2[2] = 4.0*atan(1.0);
  x2[0] = 0.0;
  x2[1] = 0.0;

  x3[2] = 4.0*atan(1.0);
  x3[0] = 4.0*atan(1.0);
  x3[1] = 0.0;

  x4[2] = 0.0;
  x4[0] = 4.0*atan(1.0);
  x4[1] = 0.0;
#endif

    x1[2] = 0.0;
    x1[0] = 0.0;
    x1[1] = 0.0;

    x2[2] = 1.0;
    x2[0] = 0.0;
    x2[1] = 1.0;

    x3[2] = 1.0;
    x3[0] = 1.0;
    x3[1] = 1.0;

    x4[2] = 0.0;
    x4[0] = 1.0;
    x4[1] = 0.0;

    gauss_common_init();

    for (i = 1; i < GAUSS_MAX_NG + 1; i++)
    {
        // i=1;
        iop = gauss_quad(i, 3, x1, x2, x3, x4, &func2, (void *)xx, &res);
        printf("integrale sur quad par gauss_quad    (%d pt Gauss) = %E\n", i,
               res);
        iop = gauss_generic(i, 3, xx, GAUSS_EL_QUAD, &func2, (void *)xx, &res);
        printf("integrale sur quad par gauss_generic (%d pt Gauss) = %E\n", i,
               res);
    }

    /****/

    return 0;
}
/* -------------------------------------------------------------------------- */

/*
 *    TEST HEXA
 */

int
test_hexa()
{
    int iop = 0, i;
    double x1[3], x2[3], x3[3], x4[3], x5[3], x6[3], x7[3], x8[3];
    double res = 0.0;
    double *xx[4];

    void *pipo = nullptr;

    xx[0] = x1;
    xx[1] = x2;
    xx[2] = x3;
    xx[3] = x4;

    x1[0] = 0.0;
    x1[1] = 0.0;
    x1[2] = 0.0;

    x2[0] = 1.0;
    x2[1] = 0.0;
    x2[2] = 0.0;

    x3[0] = 1.0;
    x3[1] = 1.0;
    x3[2] = 0.0;

    x4[0] = 0.0;
    x4[1] = 1.0;
    x4[2] = 0.0;

    x5[0] = 0.0;
    x5[1] = 0.0;
    x5[2] = 1.0;

    x6[0] = 1.0;
    x6[1] = 0.0;
    x6[2] = 1.0;

    x7[0] = 1.0;
    x7[1] = 1.0;
    x7[2] = 1.0;

    x8[0] = 0.0;
    x8[1] = 1.0;
    x8[2] = 1.0;

    gauss_common_init();

    for (i = 1; i < GAUSS_MAX_NG + 1; i++)
    {
        // i=1;
        iop = gauss_hexa(i, 3, x1, x2, x3, x4, x5, x6, x7, x8, &func3,
                         (void *)xx, &res);
        printf("integrale sur hexa (%d pt Gauss) = %E\n", i, res);
    }

    /****/

    return 0;
}

/* -------------------------------------------------------------------------- */

/*
 *    TEST LINE (3D)
 */

int
test_line()
{
    int iop = 0, i;
    double x1[3], x2[3];
    double res = 0.0;
    double *xx[4];

    void *pipo = nullptr;

    xx[0] = x1;
    xx[1] = x2;

    x1[0] = 0.0;
    x1[1] = 0.0;
    x1[2] = 0.0;

    x2[2] = 4.0 * atan(1.0);
    x2[1] = 0.0;
    x2[0] = 0.0;

    gauss_common_init();

    for (i = 1; i < GAUSS_MAX_NG + 1; i++)
    {
        // i=1;
        iop = gauss_line(i, 3, x1, x2, &func2, (void *)xx, &res);
        printf("integrale sur ligne (%d pt Gauss) = %E\n", i, res);
    }

    /****/

    return 0;
}

/* -------------------------------------------------------------------------- */

/*
 *    ROUTINE D'APPEL
 */

int
main()
{

    printf("\n*** TEST INTEGRATION SUR QUAD ***\n");
    test_quad();
    printf("\n*** TEST INTEGRATION SUR LINE ***\n");
    test_line();
    printf("\n*** TEST INTEGRATION SUR HEXA ***\n");
    test_hexa();

    return 0;
}
