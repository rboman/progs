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
      //void create_aux();
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
      //void destroy_aux();
      delete alpha, xf, yf, xel, yel, xint, yint, u, q, fct, fct2;
      delete G1, H1;
      destroy_aux();
}
