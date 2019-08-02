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

#ifndef MATFUN_H
#define MATFUN_H

#include "ndh.h"

// routines de manipulation matricielles

namespace ndh
{

void fillvector(double *vect, double v1, double step, int nel);
void mmv(int dim, double **A, double *b, double *c);
void gauss(int dim, double **A, double *x, double *b);
void vectaff(int dim, double *v);
void copy_block(double **A, int i2, int j2, int i1, int j1, int sizebloc);

} // namespace ndh

#endif // MATFUN_H
