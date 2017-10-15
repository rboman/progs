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
#include "memory.h"

//--------------------------------------------------------------------
// Routines de gestion des tableaux dynamiques.
// (création, destruction,...)
//--------------------------------------------------------------------

void create_aux()
{
      T = new double *[density];
      for (int i = 0; i < density; i++)
            T[i] = new double[range];
}

void create_GH()
{
      H = new double *[N];
      for (int i = 0; i < N; i++)
            H[i] = new double[N];
      G = new double *[N];
      for (int i = 0; i < N; i++)
            G[i] = new double[N];
}

void create_vectors()
{
      alpha = new double[N + 1];
      xf = new double[N + 1];
      yf = new double[N + 1];
      xel = new double[N];
      yel = new double[N];
      xint = new double[istep + 1];
      yint = new double[istep + 1];
      u = new double[N];
      q = new double[N];
      fct = new double[istep + 1];
      fct2 = new double[istep + 1];
      G1 = new double[N];
      H1 = new double[N];
      create_aux();
}

void destroy_aux()
{
      for (int i = 0; i < d_old; i++)
            delete T[i];
      delete T;
      d_old = density;
}

void destroy_GH()
{
      for (int i = 0; i < N; i++)
            delete H[i];
      delete H;
      for (int i = 0; i < N; i++)
            delete G[i];
      delete G;
}

void destroy_vectors()
{
      delete alpha, xf, yf, xel, yel, xint, yint, u, q, fct, fct2;
      delete G1, H1;
      destroy_aux();
}
