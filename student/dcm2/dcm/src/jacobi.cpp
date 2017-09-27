//   Copyright 1994 Igor Klapka
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

#include "jacobi.h"
#include <cmath>

#define ROTATE(a, i, j, k, l)        \
    g = a[i][j];                     \
    h = a[k][l];                     \
    a[i][j] = g - s * (h + g * tau); \
    a[k][l] = h + s * (g - h * tau);

typedef double TYPE;

//    a Matrice de départ dont on cherche les Valeurs Propres
//      (! la diagonale sera modifiée après passage)
//    n Taille de la matrice a
//    d valeurs propres (non ordonnées)
//    v modes propres normés correspondants
// nrot nombre de rotations dans la méthode

void jacobi(TYPE **a, int n, TYPE d[], TYPE **v, int &nrot)
{
    int j, iq, ip, i;
    TYPE tresh, theta, tau, t, sm, s, h, g, c, *b, *z;

    b = new TYPE[n + 1];
    z = new TYPE[n + 1];
    for (ip = 1; ip <= n; ip++)
    {
        for (iq = 1; iq <= n; iq++)
            v[ip][iq] = 0.0;
        v[ip][ip] = 1.0;
    }
    for (ip = 1; ip <= n; ip++)
    {
        b[ip] = d[ip] = a[ip][ip];
        z[ip] = 0.0;
    }
    nrot = 0;
    for (i = 1; 1 <= 50; i++)
    {
        sm = 0.0;
        for (ip = 1; ip <= n - 1; ip++)
            for (iq = ip + 1; iq <= n; iq++)
                sm += fabs(a[ip][iq]);
        if (sm == 0.0)
        {
            delete b;
            delete z;
            return;
        }
        if (i < 4)
            tresh = 0.2 * sm / (n * n);
        else
            tresh = 0.;
        for (ip = 1; ip <= n - 1; ip++)
        {
            for (iq = ip + 1; iq <= n; iq++)
            {
                g = 100.0 * fabs(a[ip][iq]);
                if ((i > 4) && ((TYPE)(fabs(d[ip]) + g) == (TYPE)fabs(d[ip])) && ((TYPE)(fabs(d[iq]) + g) == (TYPE)fabs(d[iq])))
                    a[ip][iq] = 0.0;
                else if (fabs(a[ip][iq]) > tresh)
                {
                    h = d[iq] - d[ip];
                    if ((TYPE)(fabs(h) + g) == (TYPE)fabs(h))
                        t = (a[ip][iq]) / h;
                    else
                    {
                        theta = 0.5 * h / (a[ip][iq]);
                        t = 1.0 / (fabs(theta) + sqrt(1.0 + theta * theta));
                        if (theta < 0.0)
                            t = -t;
                    }
                    c = 1.0 / sqrt(1. + t * t);
                    s = t * c;
                    tau = s / (1.0 + c);
                    h = t * a[ip][iq];
                    z[ip] -= h;
                    z[iq] += h;
                    d[ip] -= h;
                    d[iq] += h;
                    a[ip][iq] = 0.0;
                    for (j = 1; j <= ip - 1; j++)
                    {
                        ROTATE(a, j, ip, j, iq)
                    }
                    for (j = ip + 1; j <= iq - 1; j++)
                    {
                        ROTATE(a, ip, j, j, iq)
                    }
                    for (j = iq + 1; j <= n; j++)
                    {
                        ROTATE(a, ip, j, iq, j)
                    }
                    for (j = 1; j <= n; j++)
                    {
                        ROTATE(v, j, ip, j, iq)
                    }
                    ++nrot;
                }
            }
        }
        for (ip = 1; ip <= n; ip++)
        {
            b[ip] += z[ip];
            d[ip] = b[ip];
            z[ip] = 0.0;
        }
    }
}
