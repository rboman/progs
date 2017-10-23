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

#ifndef __TDIMAT_H__
#define __TDIMAT_H__

#include "sky.h"

#include <string>
#include <vector>

/* codes d'erreurs renvoyes par le solveur */

#define TDI_ERR_OK 0
#define TDI_ERR_PIV0 1

/* options du solver (utiliser l'operateur '|') */

#define TDI_DO_LU (1 << 0)
#define TDI_DO_SUBST (1 << 1)

/* etat de la matrice (matrice/decomp LU - LDLt) */

#define TDI_A 0
#define TDI_LU 1

/**
 * @brief Tridiagonal Matrix / Solver
 * 
 *   - Gestion de Matrices TriDIagonales
 *   - Solver non symetrique (sans gestion de pivots nuls)
 */

class SKY_API TdiMat
{
    int nsys;
    int nsys_a;
    double *s[3];
    std::string name;
public:
    TdiMat(std::string const &_name="noname");
    ~TdiMat();

    // Routines d'initialisation
    void reinit();
    void setsize(int nsys);

    // Routines de manipulation de la matrice
    void ass(int i, int j, double val);
    void set(int i, int j, double val);
    void fill(double val);

    // Solveur
    int solve(double *q, double *x, int type);
    void print_err(FILE *fich, int code);

    static void test();  

    void mlab(char *filename, char *id_txt, int type, int nfile, int opt);

private:

};

#endif // __TDIMAT_H__
