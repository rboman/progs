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
 * TdiLib
 * ======
 *   - Gestion de Matrices TriDIagonales
 *   - Solver non symetrique (sans gestion de pivots nuls)
 *
 * RoBo 27-09-00
 *
 * Remarques: - l'utilisation est identique a "skylib"
 *            - la matrice est ecrasee par sa decomposition LU !!
 */

/**************************************************************************
                                    Headers
 **************************************************************************/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <cmath>

#include "TdiMat.h"
#include "mlab.h"

/**************************************************************************
                                 Macros locales
 **************************************************************************/

// Tolerance relative aux pivots nuls
#define TDI_EPS 1.0e-18

// Infos de debug
#undef VERBOSE
#define VERBOSE 0

TdiMat::TdiMat(std::string const &_name) : name(_name)
{
    // throw std::runtime_error("STOP"); // test exception

    this->nsys = 0;
    this->nsys_a = 0;
    for (int i = 0; i < 3; i++)
        this->s[i] = nullptr;
}

TdiMat::~TdiMat()
{
    if (this->nsys_a > 0)
    {
        for (int i = 0; i < 3; i++)
            free(this->s[i]);
    }
}

/**************************************************************************/

/*
 *              Reinit la matrice utilisee (libere la memoire)
 *              ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

void
TdiMat::reinit()
{
    // PURGE LA MEMOIRE

    // s[]
    if (this->nsys_a > 0)
    {
        for (int i = 0; i < 3; i++)
            free(this->s[i]);
        this->nsys_a = 0;
    }

    // INITIALISATION

    this->nsys = 0;
    this->nsys_a = 0;
    for (int i = 0; i < 3; i++)
        this->s[i] = nullptr;
}

/**
 * @brief Definit la taille de la matrice
 */

void
TdiMat::setsize(int nsys)
{
    if (nsys <= 0)
        throw std::runtime_error("taille matrice <=0 !");

    if (this->nsys_a < nsys)
        for (int i = 0; i < 3; i++)
        {
            this->s[i] = (double *)realloc(this->s[i], nsys * sizeof(double));
            this->nsys_a = nsys;
        }

    this->nsys = nsys;

    // Init
    this->fill(0.0);
}

void
TdiMat::ass(int i, int j, double val)
{
    if (abs(i - j) > 1)
    {
        std::stringstream str;
        str << "(i,j)=(" << i << "," << j << ") hors des 3 diagonales!";
        throw std::runtime_error(str.str());
    }
    this->s[1 + i - j][i] += val;
}

/**
 * @brief Assemble un element dans la matrice TRIDIAG
 */

void
TdiMat::set(int i, int j, double val)
{
    if (abs(i - j) > 1)
    {
        std::stringstream str;
        str << "(i,j)=(" << i << "," << j << ") hors des 3 diagonales!";
        throw std::runtime_error(str.str());
    }

    this->s[1 + i - j][i] = val;
}

/**
 * @brief Remplit un matrice TRIDIAG avec "val"
 */

void
TdiMat::fill(double val)
{
    for (int i = 0; i < 3; i++)
        for (int j = 0; j < this->nsys; j++)
            this->s[i][j] = val;
}

/**
 * @brief Solveur
 */

int
TdiMat::solve(double *q, double *x, int type)
{
    // Decomposition LU

    if ((type & TDI_DO_LU) == TDI_DO_LU)
    {
        double prec = 0.0;
        for (int i = 0; i < this->nsys; i++)
            prec += fabs(s[1][i]);

        prec = prec / this->nsys * TDI_EPS;

        if (fabs(s[1][0]) < prec)
            return SKY_ERR_PIV0;
        for (int i = 1; i < this->nsys; i++)
        {
            s[2][i] /= s[1][i - 1];
            s[1][i] -= s[2][i] * s[0][i - 1];
            if (fabs(s[1][i]) < prec)
                return SKY_ERR_PIV0;
        }
    }

    if ((type & TDI_DO_SUBST) == TDI_DO_SUBST)
    {
        // Subst avant
        x[0] = q[0];
        for (int i = 1; i < this->nsys; i++)
            x[i] = q[i] - s[2][i] * x[i - 1];

        // Subst arriere (on suppose qu'il n'y a pas de pivots nuls)
        x[this->nsys - 1] = x[this->nsys - 1] / s[1][this->nsys - 1];
        for (int i = this->nsys - 2; i >= 0; i--)
            x[i] = (x[i] - s[0][i] * x[i + 1]) / s[1][i];
    }

    return TDI_ERR_OK;
}

/**
 * @brief Traitement des codes renvoyes par les solveurs
 */

void
TdiMat::print_err(FILE *fich, int code)
{
    char const *err[3] = {"ok", "pivot nul", "code inconnu"};
    char const *e;

    switch (code)
    {
    case TDI_ERR_OK:
        e = err[0];
        break;
    case TDI_ERR_PIV0:
        e = err[1];
        break;
    default:
        e = err[2];
        break;
    }
    fprintf(fich, "code solver: %s\n", e);
}

/**
 * @brief Routines de test de la classe
 */

void
TdiMat::test()
{
    double A[5][5] = {{1, 3, 0, 0, 0},
                      {1, 2, 0, 0, 0},
                      {0, 2, 3, 0, 0},
                      {0, 0, 11, 4, 0},
                      {0, 0, 0, 0, 7}};

    double q[5] = {1, 1, 1, 1, 1};
    double x[5], xs[5];
    int n = 5;

    // initialisation

    for (int i = 0; i < n; i++)
    {
        x[i] = 0;
        xs[i] = 0;
    }

    TdiMat K("K");
    K.setsize(n);

    // assemblage Ks
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            if (A[i][j] != 0.0)
                K.ass(i, j, A[i][j]);

    K.mlab("tri.m", "1", TDI_A, MLAB_NEW, MLAB_VERBOSE);

    // resolution

    int iop = K.solve(q, x, TDI_DO_LU | TDI_DO_SUBST);
    K.print_err(stdout, iop);

    // verification matlab

    K.mlab("tri.m", "2", TDI_LU, MLAB_OLD, MLAB_VERBOSE);
    mlab_vec("tri.m", "q", q, n, MLAB_OLD, MLAB_VERBOSE);
    mlab_vec("tri.m", "x", x, n, MLAB_OLD, MLAB_VERBOSE);

    // purge memoire (facultatif)
    K.reinit();
}

/**
 * @brief Ecriture d'une matrice TRIDIAG dans un fichier MATLAB existant ou non
 */

void
TdiMat::mlab(char const *filename, char const *id_txt, int type, int nfile,
             int opt)
{
    char const *pre[3] = {"", "L", "U"};

    char const *pre1 = (type == TDI_LU) ? pre[1] : pre[0];
    char const *pre2 = (type == TDI_LU) ? pre[2] : pre[0];

    FILE *fich;
    if (nfile == MLAB_OLD)
        fich = fopen(filename, "a");
    else
        fich = fopen(filename, "w");
    if (fich == nullptr)
        throw std::runtime_error("impossible d'ouvrir le fichier " + name);

    for (int i = 0; i < this->nsys; i++)
    {
        int i1 = i + 1;
        if (type == TDI_LU)
            fprintf(fich, "%s%s%s(%d,%d) = 1.0;\n", name.c_str(), id_txt, pre1,
                    i1, i1);
        fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name.c_str(), id_txt, pre2,
                i1, i1, this->s[1][i]);
        if (i != 0)
            fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name.c_str(), id_txt,
                    pre1, i1, i1 - 1, this->s[2][i]);
        if (i != this->nsys - 1)
            fprintf(fich, "%s%s%s(%d,%d) = %20.15E;\n", name.c_str(), id_txt,
                    pre2, i1, i1 + 1, this->s[0][i]);
    }

    fclose(fich);

    if (VERBOSE || (opt == MLAB_VERBOSE))
        printf("matrice \"%s\" sauvee dans le fichier matlab \"%s\" (%s)\n",
               name.c_str(), filename, (nfile == MLAB_OLD) ? "append" : "new");
}

SKY_API std::ostream &
operator<<(std::ostream &out, TdiMat const &obj)
{
    out << "TdiMat '" << obj.name << "'\n";
    return out;
}
