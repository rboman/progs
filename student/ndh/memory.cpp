/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : Eléments aux frontiéres               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derniére modif.: 30.11.96               *
 *                                                                   *
 *********************************************************************
 *  Programme : MEMORY.CPP  (routines de gestion des tableaux dyn.)  *
 *********************************************************************/

#include "ndh.h"

//--------------------------------------------------------------------
// Routines de gestion des tableaux dynamiques.
// (création, destruction,...)
//--------------------------------------------------------------------

void create_aux()
{
      int i;
      T = new float *[density];
      for (i = 0; i < density; i++)
            T[i] = new float[range];
}

void create_GH()
{
      int i;
      H = new float *[N];
      for (i = 0; i < N; i++)
            H[i] = new float[N];
      G = new float *[N];
      for (i = 0; i < N; i++)
            G[i] = new float[N];
}

void create_vectors()
{
      int i;
      void create_aux();
      alpha = new float[N + 1];
      xf = new float[N + 1];
      yf = new float[N + 1];
      xel = new float[N];
      yel = new float[N];
      xint = new float[istep + 1];
      yint = new float[istep + 1];
      u = new float[N];
      q = new float[N];
      fct = new float[istep + 1];
      fct2 = new float[istep + 1];
      G1 = new float[N];
      H1 = new float[N];
      create_aux();
}

void destroy_aux()
{
      int i;
      for (i = 0; i < d_old; i++)
            delete T[i];
      delete T;
      d_old = density;
}

void destroy_GH()
{
      int i;
      for (i = 0; i < N; i++)
            delete H[i];
      delete H;
      for (i = 0; i < N; i++)
            delete G[i];
      delete G;
}

void destroy_vectors()
{
      int i;
      void destroy_aux();
      delete alpha, xf, yf, xel, yel, xint, yint, u, q, fct, fct2;
      delete G1, H1;
      destroy_aux();
}
