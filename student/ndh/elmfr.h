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

//#include <conio.h>
//#include <stdio.h>
#include <cmath>
#include <iostream>
#include <fstream>
#include <time.h>

#include "graph.h"
#include "iofun.h"
#include "matfun.h"
#include "memory.h"
#include "param.h"

extern int N;        // Nombre d'éléments frontiéres sur le contour.
extern int istep;    // Nombre de pas d'intégration sur un élément.
extern int density;  // Densité de visualisation de la solution
                   // (nombre de mailles sur un rayon).
extern int d_old;         // Ancienne valeur de la densité (utile pour
                   // détruire correctement le tableau des T).
extern int range;         // Nbre de ray. sur lesquels la sol. est calculée.
extern int ideg;      // Type d'intégration de Newton-Cotes
                   // (1=trapéze, 2=Simpson,...).
extern int type;      // Méthode de calcul (1=full, 2=symétrique).
extern int maillag;   // 1=Dessine le maillage.
extern int probleme;  // Type de probléme (1=cercle, 2=carré, 3=qcq.).
extern int whitebg;   // 1=Fond blanc pour l'impression.
extern int cartesien; // 1=maillage rectangulaire (density x density)
                   // (uniquement pour le carré).
extern int calcul;    // 1=calculs effectués.

extern clock_t time1, time2; // temps de début et de fin de calcul.

extern float xo, yo; // (x,y) de l'origine des axes absolus.
extern float zoom; // Zoom de visualisation.
extern float *alpha;             // Vecteur temporaire [N].
extern float *xf, *yf;           // (x,y) des extrémités des éléments [N+1].
extern float *xel, *yel;         // (x,y) des connecteurs [N].
extern float *xint, *yint;       // (x,y) des points d'intégration [istep+1].
extern float *fct, *fct2;        // Valeurs des fonctions é intégrer [istep+1].
extern float *G1, *H1;           // Vect. auxilaires pour le calcul des T [N].
extern float *u;                 // Tempétatures sur les éléments [N].
extern float *q;                 // Flux de chaleur sur les éléments [N].
extern float **G, **H;           // Matrices G et H [N,N].
extern float **T;                // Tableau des T calculées [density,range].
extern float beta;          // Paramétre du probléme.
extern float k;            // Conductivité thermique.
extern float R;            // Rayon du cercle.
extern float a;            // Longueur du cété du carré.
extern float pi;                 // 3.141592.
extern float Tmin, Tmax;         // Valeurs min et max des T calculées.

// Coefficients de l'intégration de Newton-Cotes:
extern float icoeff[6][7];
extern float idiv[6];

#endif //ELMFR_H

