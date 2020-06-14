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

#ifndef BEMSOLVER_H
#define BEMSOLVER_H

#include "ndh.h"
#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
#include <time.h>
#include <vector>

namespace ndh
{

enum Meth
{
    FULL = 1,
    SYMMETRIC = 2
};

enum Prb
{
    CIRCLE = 1,
    SQUARE = 2,
    OTHER = 3
};

class NDH_API BemSolver
{
    int N;       // Nombre d'éléments frontières sur le contour.
    int istep;   // Nombre de pas d'intégration sur un élément.
    int density; // Densité de visualisation de la solution
                 // (nombre de mailles sur un rayon).
    int d_old;   // Ancienne valeur de la densité (utile pour
                 // détruire correctement le tableau des T).
    int range;   // Nbre de ray. sur lesquels la sol. est calculée.
    int ideg;    // Type d'intégration de Newton-Cotes
                 // (1=trapéze, 2=Simpson,...).
public:
    Meth type;    // Méthode de calcul (1=full, 2=symétrique).
    Prb probleme; // Type de problème (1=cercle, 2=carré, 3=qcq.).

private:
    int maillag;    // 1=Dessine le maillage.
    int whitebg;    // 1=Fond blanc pour l'impression.
    bool cartesien; // 1=maillage rectangulaire (density x density)
                    // (uniquement pour le carré).
    int calcul;     // 1=calculs effectués.

    clock_t time1, time2; // temps de début et de fin de calcul.

    double xo, yo;       // (x,y) de l'origine des axes absolus.
    double zoom;         // Zoom de visualisation.
    double *alpha;       // Vecteur temporaire [N].
    double *xf, *yf;     // (x,y) des extrémités des éléments [N+1].
    double *xel, *yel;   // (x,y) des connecteurs [N].
    double *xint, *yint; // (x,y) des points d'intégration [istep+1].
    double *fct, *fct2;  // Valeurs des fonctions é intégrer [istep+1].
    double *G1, *H1;     // Vect. auxilaires pour le calcul des T [N].
    double *u;           // Tempétatures sur les éléments [N].
    double *q;           // Flux de chaleur sur les éléments [N].
    double **G, **H;     // Matrices G et H [N,N].
    double **T;          // Tableau des T calculées [density,range].
    double beta;         // Paramétre du probléme.
    double k;            // Conductivité thermique.
    double R;            // Rayon du cercle.
    double a;            // Longueur du cété du carré.
    double pi;           // 3.141592.
    double Tmin, Tmax;   // Valeurs min et max des T calculées.

    // Coefficients de l'intégration de Newton-Cotes:
    double icoeff[6][7] = {
        {1, 1, 0, 0, 0, 0, 0},       {1, 4, 1, 0, 0, 0, 0},
        {1, 3, 3, 1, 0, 0, 0},       {7, 32, 12, 32, 7, 0, 0},
        {19, 75, 50, 50, 75, 19, 0}, {41, 216, 27, 272, 27, 216, 41}};
    double idiv[6] = {2, 6, 8, 90, 288, 840};

public:
    BemSolver();

    void exec_full();
    void exec_sym();

    std::vector<double> getSolution();

    // protos
    void eval_Texact();
    void input_data();

    void load_data(std::string const &filename);
    void save_Mfile(std::string const &filename);

    void find_minmax();

    // pas appelé
    void generate(); // void tester()

private:
    void full_calcul();

    void eval_GH(double *g, double *h, int i, int j, double x, double y);
    void eval_u();

    void create_vectors();
    void define_geometry();

    void create_aux();
    void create_GH();

    void destroy_aux();
    void destroy_GH();
    void destroy_vectors();
};

#ifndef SWIG
NDH_API void clrscr();
#endif

} // namespace ndh

#endif // BEMSOLVER_H
