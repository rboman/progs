/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : El�ments aux fronti�res               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derni�re modif.: 30.11.96               *
 *                                                                   *
 *********************************************************************
 *  Programme : MATFUN.CPP  (routines de manipulation matricielles)  *
 *********************************************************************/

#include "extern.h"

//--------------------------------------------------------------------
// Cr�e un vecteur � la MATLAB
// ( x=a:b:c  -> vector(x,a,b,(c-a)/b+1); )
//--------------------------------------------------------------------

void vector(float *vect, float v1, float step, int nel)
{
      int i;
      vect[0] = v1;
      for (i = 1; i < nel; i++)
            vect[i] = vect[i - 1] + step;
}

//--------------------------------------------------------------------
// Routine de multiplication d'une matrice carr�e par un vecteur
// ( c=A*b avec A(dim,dim), b(dim), c(dim) )
//--------------------------------------------------------------------

void mmv(int dim, float **A, float *b, float *c)
{
      int i, j;
      float temp;
      for (i = 0; i < dim; i++)
      {
            temp = 0.0;
            for (j = 0; j < dim; j++)
                  temp = temp + A[i][j] * b[j];
            c[i] = temp;
      }
}

//--------------------------------------------------------------------
//  Solveur de A*x=b : Gauss sans pivotage
//--------------------------------------------------------------------

void gauss(int dim, float **A, float *x, float *b)
{
      int i, j, t;
      float m;

      for (t = 0; t < dim - 1; t++)
            for (i = t + 1; i < dim; i++)
            {
                  m = A[i][t] / A[t][t];
                  for (j = t + 1; j < dim; j++)
                        A[i][j] = A[i][j] - m * A[t][j];
                  b[i] = b[i] - m * b[t];
            }

      for (i = dim - 1; (i + 1) > 0; i--)
      {
            m = 0.0;
            for (t = i + 1; t < dim; t++)
                  m = m + A[i][t] * x[t];
            x[i] = (b[i] - m) / A[i][i];
      }
}

//--------------------------------------------------------------------
// Affichage d'un vecteur � l'�cran.
//--------------------------------------------------------------------

void vectaff(int dim, float *v)
{
      int i;
      for (i = 0; i < dim; i++)
            cout << "\n"
                 << v[i];
      cout << "\n<Pause>\n";
      getch();
}

//--------------------------------------------------------------------
// Copie un bloc dans la matrice A, de taille 'sizebloc'
// de la position i1,j1 � la position i2,j2.
//--------------------------------------------------------------------

void copy_block(float **A, int i2, int j2, int i1, int j1, int sizebloc)
{
      int i, j;
      for (i = 0; i < sizebloc; i++)
            for (j = 0; j < sizebloc; j++)
                  A[i2 + i][j2 + j] = A[i1 + i][j1 + j];
}