//   Copyright 1996-2017 Romain Boman
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

#include "elmfr.h"
#include "matfun.h"

//--------------------------------------------------------------------
// Crée un vecteur é la MATLAB
// ( x=a:b:c  -> fillvector(x,a,b,(c-a)/b+1); )
//--------------------------------------------------------------------

void fillvector(double *vect, double v1, double step, int nel)
{
    vect[0] = v1;
    for (int i = 1; i < nel; i++)
        vect[i] = vect[i - 1] + step;
}

//--------------------------------------------------------------------
// Routine de multiplication d'une matrice carrée par un vecteur
// ( c=A*b avec A(dim,dim), b(dim), c(dim) )
//--------------------------------------------------------------------

void mmv(int dim, double **A, double *b, double *c)
{
    for (int i = 0; i < dim; i++)
    {
        double temp = 0.0;
        for (int j = 0; j < dim; j++)
            temp = temp + A[i][j] * b[j];
        c[i] = temp;
    }
}

//--------------------------------------------------------------------
//  Solveur de A*x=b : Gauss sans pivotage
//--------------------------------------------------------------------

void gauss(int dim, double **A, double *x, double *b)
{
    for (int t = 0; t < dim - 1; t++)
        for (int i = t + 1; i < dim; i++)
        {
            double m = A[i][t] / A[t][t];
            for (int j = t + 1; j < dim; j++)
                A[i][j] = A[i][j] - m * A[t][j];
            b[i] = b[i] - m * b[t];
        }

    for (int i = dim - 1; (i + 1) > 0; i--)
    {
        double m = 0.0;
        for (int t = i + 1; t < dim; t++)
            m = m + A[i][t] * x[t];
        x[i] = (b[i] - m) / A[i][i];
    }
}

//--------------------------------------------------------------------
// Affichage d'un vecteur à l'écran.
//--------------------------------------------------------------------

void vectaff(int dim, double *v)
{
    for (int i = 0; i < dim; i++)
        std::cout << "\n" << v[i];
}

//--------------------------------------------------------------------
// Copie un bloc dans la matrice A, de taille 'sizebloc'
// de la position i1,j1 à la position i2,j2.
//--------------------------------------------------------------------

void copy_block(double **A, int i2, int j2, int i1, int j1, int sizebloc)
{
    for (int i = 0; i < sizebloc; i++)
        for (int j = 0; j < sizebloc; j++)
            A[i2 + i][j2 + j] = A[i1 + i][j1 + j];
}