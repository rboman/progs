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

#ifndef ELMFR_H
#define ELMFR_H

#include <cmath>
#include <iostream>
#include <fstream>
#include <string>
#include <time.h>

#include "graph.h"
#include "iofun.h"
#include "matfun.h"
#include "memory.h"
#include "param.h"

extern int N;                   // Nombre d'éléments frontières sur le contour.
extern int istep;               // Nombre de pas d'intégration sur un élément.
extern int density;             // Densité de visualisation de la solution
                                // (nombre de mailles sur un rayon).
extern int d_old;               // Ancienne valeur de la densité (utile pour
                                // détruire correctement le tableau des T).
extern int range;               // Nbre de ray. sur lesquels la sol. est calculée.
extern int ideg;                // Type d'intégration de Newton-Cotes
                                // (1=trapéze, 2=Simpson,...).
extern int type;                // Méthode de calcul (1=full, 2=symétrique).
extern int maillag;             // 1=Dessine le maillage.
extern int probleme;            // Type de probléme (1=cercle, 2=carré, 3=qcq.).
extern int whitebg;             // 1=Fond blanc pour l'impression.
extern int cartesien;           // 1=maillage rectangulaire (density x density)
                                // (uniquement pour le carré).
extern int calcul;              // 1=calculs effectués.

extern clock_t time1, time2;    // temps de début et de fin de calcul.

extern double xo, yo;           // (x,y) de l'origine des axes absolus.
extern double zoom;             // Zoom de visualisation.
extern double *alpha;           // Vecteur temporaire [N].
extern double *xf, *yf;         // (x,y) des extrémités des éléments [N+1].
extern double *xel, *yel;       // (x,y) des connecteurs [N].
extern double *xint, *yint;     // (x,y) des points d'intégration [istep+1].
extern double *fct, *fct2;      // Valeurs des fonctions é intégrer [istep+1].
extern double *G1, *H1;         // Vect. auxilaires pour le calcul des T [N].
extern double *u;               // Tempétatures sur les éléments [N].
extern double *q;               // Flux de chaleur sur les éléments [N].
extern double **G, **H;         // Matrices G et H [N,N].
extern double **T;              // Tableau des T calculées [density,range].
extern double beta;             // Paramétre du probléme.
extern double k;                // Conductivité thermique.
extern double R;                // Rayon du cercle.
extern double a;                // Longueur du cété du carré.
extern double pi;               // 3.141592.
extern double Tmin, Tmax;       // Valeurs min et max des T calculées.

// Coefficients de l'intégration de Newton-Cotes:
extern double icoeff[6][7];
extern double idiv[6];


// protos 

void define_geometry();
void eval_GH(double *g, double *h, int i, int j, double x, double y);
void eval_u();
void full_calcul();
void eval_Texact();
void generate();   //void tester()

void clrscr();


#endif //ELMFR_H

