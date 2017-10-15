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

// VARIABLES GLOBALES! ------------------------

int N = 40;        // Nombre d'éléments frontiéres sur le contour.
int istep = 20;    // Nombre de pas d'intégration sur un élément.
int density = 15;  // Densité de visualisation de la solution
                   // (nombre de mailles sur un rayon).
int d_old;         // Ancienne valeur de la densité (utile pour
                   // détruire correctement le tableau des T).
int range;         // Nbre de ray. sur lesquels la sol. est calculée.
int ideg = 1;      // Type d'intégration de Newton-Cotes
                   // (1=trapéze, 2=Simpson,...).
int type = 1;      // Méthode de calcul (1=full, 2=symétrique).
int maillag = 1;   // 1=Dessine le maillage.
int probleme = 1;  // Type de probléme (1=cercle, 2=carré, 3=qcq.).
int whitebg = 1;   // 1=Fond blanc pour l'impression.
int cartesien = 0; // 1=maillage rectangulaire (density x density)
                   // (uniquement pour le carré).
int calcul = 0;    // 1=calculs effectués.

clock_t time1 = 0, time2 = 0; // temps de début et de fin de calcul.

double xo = 220, yo = 240; // (x,y) de l'origine des axes absolus.
double zoom = 200.0 / 1.2; // Zoom de visualisation.
double *alpha;             // Vecteur temporaire [N].
double *xf, *yf;           // (x,y) des extrémités des éléments [N+1].
double *xel, *yel;         // (x,y) des connecteurs [N].
double *xint, *yint;       // (x,y) des points d'intégration [istep+1].
double *fct, *fct2;        // Valeurs des fonctions é intégrer [istep+1].
double *G1, *H1;           // Vect. auxilaires pour le calcul des T [N].
double *u;                 // Tempétatures sur les éléments [N].
double *q;                 // Flux de chaleur sur les éléments [N].
double **G, **H;           // Matrices G et H [N,N].
double **T;                // Tableau des T calculées [density,range].
double beta = 80;          // Paramétre du probléme.
double k = 400;            // Conductivité thermique.
double R = 1.2;            // Rayon du cercle.
double a = 1.2;            // Longueur du cété du carré.
double pi = 4 * atan(1.0);                 // 3.141592.
double Tmin, Tmax;         // Valeurs min et max des T calculées.

// Coefficients de l'intégration de Newton-Cotes:
double icoeff[6][7] = {{1, 1, 0, 0, 0, 0, 0},
                      {1, 4, 1, 0, 0, 0, 0},
                      {1, 3, 3, 1, 0, 0, 0},
                      {7, 32, 12, 32, 7, 0, 0},
                      {19, 75, 50, 50, 75, 19},
                      {41, 216, 27, 272, 27, 216, 41}};
double idiv[6] = {2, 6, 8, 90, 288, 840};

// ---------------------------------------------------

#include <stdlib.h>
void clrscr()
{
#ifdef WIN32
    //system("cls");
#endif
}

//--------------------------------------------------------------------
// Routine de définition de la géométrie :
//  Remplit les vecteurs xf,yf et xel,yel.
//  . si probleme=1 -> création d'un cercle.
//  . si probleme=2 -> création d'un carré.
//--------------------------------------------------------------------

void define_geometry()
{
    if (probleme == 1) // cercle
    {
        fillvector(alpha, 0.0, (2 * pi) / N, N + 1);
        for (int i = 0; i < N + 1; i++)
        {
            xf[i] = R * cos(alpha[i]);
            yf[i] = R * sin(alpha[i]);
        }
    }
    else if (probleme == 2) // carré
    {
        int j = N / 4;
        N = 4 * j;
        fillvector(alpha, -a, (2 * a) / j, j + 1);
        for (int i = 0; i <= j; i++)
        {
            xf[i] = a;
            yf[i] = alpha[i];
            xf[i + j] = alpha[j - i];
            yf[i + j] = a;
            xf[i + 2 * j] = -a;
            yf[i + 2 * j] = alpha[j - i];
            xf[i + 3 * j] = alpha[i];
            yf[i + 3 * j] = -a;
        }
    }
    for (int i = 0; i < N; i++)
    {
        xel[i] = (xf[i] + xf[i + 1]) / 2;
        yel[i] = (yf[i] + yf[i + 1]) / 2;
    }
}

//--------------------------------------------------------------------
// Routine d'évaluation d'un élém. des matrices G et H.
//   .reéoit -les indices i et j de l'élém. à calculer.
//           -les coord. x,y de l'origine des axes.
//--------------------------------------------------------------------

void eval_GH(double *g, double *h, int i, int j, double x, double y)
{
    if (j == i)
    { 
        // terme diagonal -> on applique les formules spéciales.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        double dx = xf[i + 1] - xf[i];
        double dy = yf[i + 1] - yf[i];
        double dL = sqrt(dx * dx + dy * dy);
        *g = dL / (2 * pi) * (log(2 / dL) + 1);
        *h = 0.5;
    }
    else
    { 
        // cas général d'un terme non diagonal.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // calcul de la normale (normée) à l'élément:
        double nx = yf[j + 1] - yf[j];
        double ny = xf[j] - xf[j + 1];
        double temp = sqrt(nx * nx + ny * ny);
        nx = nx / temp;
        ny = ny / temp;

        // calcul des coord. des points d'intégration (xint,yint):
        fillvector(xint, xf[j], (xf[j + 1] - xf[j]) / istep, istep + 1);
        fillvector(yint, yf[j], (yf[j + 1] - yf[j]) / istep, istep + 1);

        // évaluation des deux fonctions à intégrer sur l'élément
        // et stockage des valeurs dans fct et fct2:
        for (int t = 0; t < istep + 1; t++)
        {
            double temp = sqrt((xint[t] - x) * (xint[t] - x) + (yint[t] - y) * (yint[t] - y));
            fct[t] = (log(1.0 / temp) / (2 * pi));
            fct2[t] = (-nx * (xint[t] - x) - ny * (yint[t] - y)) / (2 * pi * temp * temp);
        }

        // initialisation des éléments é calculer:
        *g = 0.0;
        *h = 0.0;

        // calcul de la longueur d'un pas d'intégration:
        double dx = xint[1] - xint[0];
        double dy = yint[1] - yint[0];
        double dL = sqrt(dx * dx + dy * dy);

        // intégration de Newton-Cotes:
        for (int t = 0; t < istep - ideg + 1; t += ideg)
            for (int tt = 0; tt <= ideg; tt++)
            {
                *g = *g + fct[t + tt] * icoeff[ideg - 1][tt] / idiv[ideg - 1];
                *h = *h + fct2[t + tt] * icoeff[ideg - 1][tt] / idiv[ideg - 1];
            }
        *g = *g * dL * ideg;
        *h = *h * dL * ideg;
    }
}

//--------------------------------------------------------------------
// Routine d'évaluation des températures sur chaque élément.
//--------------------------------------------------------------------

void eval_u()
{
    for (int i = 0; i < N; i++)
        u[i] = -beta / (2 * k) * (xel[i] * xel[i] + yel[i] * yel[i]);
}

//--------------------------------------------------------------------
// Routine de calcul des tempétatures (remplissage du tableau T).
// (Cette routine résoud le probléme posé)
//   .reéoit le 'type' de calculs é effectuer:
//         - type=1 : calculs sans tenir compte de la symétrie.
//         - type=2 : calculs optimisés compte tenu de la symétrie.
//--------------------------------------------------------------------

void full_calcul()
{
    int i, j, i1, j1, t;
    double temp, r, xb, yb;

    clrscr();
    titre();
    if (((probleme < 1) || (probleme > 2)) && (type == 2))
    { 
        // Cas du probléme qcq. avec calculs optimisés.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        std::cout << "\nPas de solution rapide pour un probléme QCQ !\n<ESPACE>";
        //getch();
    }
    else
    {
        time1 = clock(); // on commence é compter le temps CPU.
        calcul = 1;      // le calcul va étre effectué.
        destroy_aux();   // libération de la mémoire.
        std::cout << "\n\nCréation des matrices H et G...";
        create_GH();
        std::cout << "Ok\nCalcul des matrices H et G...";
        if (type == 1)
        {
            // Cas du probléme non optimisé:
            // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for (i = 0; i < N; i++)
                for (j = 0; j < N; j++)
                    eval_GH(&(G[i][j]), &(H[i][j]), i, j, xel[i], yel[i]);
        }
        else
        { 
            // Cas du probléme optimisé:
            // ~~~~~~~~~~~~~~~~~~~~~~~~~
            if (probleme == 1) // *** CERCLE ***
            {                  // Une seule ligne de H utile:            ******
                for (j = 0; j < N; j++)
                    eval_GH(&(G[0][j]), &(H[0][j]), 0, j, xel[0], yel[0]);
                // Utilisation de la sym. pour construire G:
                for (i = 1; i < N; i++)
                {
                    for (j = 0; j < i; j++)
                        G[i][j] = G[0][N - i + j];
                    for (j = i; j < N; j++)
                        G[i][j] = G[0][j - i];
                }
            }
            if (probleme == 2) // *** CARRE ***
            {
                t = N / 4; //     *****
                for (i = 0; i < t; i++)
                    for (j = 0; j < N; j++)
                        eval_GH(&(G[i][j]), &(H[i][j]), i, j, xel[i], yel[i]);
                for (i = 1; i < 4; i++)
                {
                    for (j = 0; j < i; j++)
                    {
                        copy_block(G, i * t, j * t, 0, (4 - i + j) * t, t);
                        copy_block(H, i * t, j * t, 0, (4 - i + j) * t, t);
                    }
                    for (j = i; j < 4; j++)
                    {
                        copy_block(G, i * t, j * t, 0, (j - i) * t, t);
                        copy_block(H, i * t, j * t, 0, (j - i) * t, t);
                    }
                }
            }
        }
        // Evaluation des T sur la frontiére et résolution du
        // systéme par Gauss:
        std::cout << "Ok\nRésolution de G q = H u...";
        eval_u();
        if ((type == 2) && (probleme == 1)) // cas du cercle optimisé
        {
            temp = 0.0;
            for (j = 0; j < N; j++)
                temp = temp + H[0][j] * u[j];
            for (j = 0; j < N; j++)
                alpha[j] = temp;
        }
        else // cas général
            mmv(N, H, u, alpha);
        gauss(N, G, q, alpha);

        // Libération de la mémoire occupée par les matrices G et H:
        std::cout << "Ok\nDestruction des matrices H et G...";
        destroy_GH();

        std::cout << "Ok\nCalcul des T intérieures...";
        if ((probleme == 1) && (type == 2))
            range = 1;
        else if ((probleme == 2) && (type == 2))
            range = density;
        else
            range = N;
        if ((probleme == 2) && (type == 2))
            cartesien = 1;
        else
            cartesien = 0;
        create_aux();

        // Calcul des points xb,yb oé va étre évaluée la T.
        for (i1 = 0; i1 < density; i1++)
            for (j1 = 0; j1 < range; j1++)
            {
                if ((probleme == 1) && (type == 2))
                {
                    xb = R / (density)*i1;
                    yb = 0.0;
                }
                else if ((probleme == 2) && (type == 2))
                {
                    xb = a / density * i1;
                    yb = a / range * j1;
                }
                else
                {
                    xb = xel[j1] / density * i1 + (xel[j1] / density) / 2.0;
                    yb = yel[j1] / density * i1 + (yel[j1] / density) / 2.0;
                }
                for (j = 0; j < N; j++)
                    eval_GH(&(G1[j]), &(H1[j]), j - 1, j, xb, yb);
                // Calcul de la solution du probléme de Poisson
                temp = 0.0;
                for (j = 0; j < N; j++)
                    temp = temp + G1[j] * q[j] - H1[j] * u[j];
                // Calcul de la solution du probléme posé
                r = sqrt(xb * xb + yb * yb);
                T[i1][j1] = temp + beta / (2 * k) * r * r;
            }

        time2 = clock(); // les calculs sont terminés !

        // Affichage de la solution
        std::cout << "Ok\nSolution :";
        for (i = 0; i < density; i++)
            std::cout << "\n"  << T[i][0];

        // Visualisation graphique:
        std::cout << "\n       <SPACE> pour solution graphique";
        //getch();
        visu();
    }
}

//--------------------------------------------------------------------
// Routine de calcul des tempétatures exactes (dans le tableau T).
//--------------------------------------------------------------------

void eval_Texact()
{
    //void titre(), visu(), find_minmax();
    int i1, j1, i;
    double temp, xb, yb, r;

    clrscr();
    titre();
    if ((probleme < 1) || (probleme > 2))
        std::cout << "\n\nPas de solution exacte disponible !";
    else
    {
        time1 = clock(); // début des calculs.
        calcul = 1;
        for (i1 = 0; i1 < density; i1++)
            for (j1 = 0; j1 < range; j1++)
            {
                if ((probleme == 1) && (range == 1))
                {
                    xb = R / (density)*i1;
                    yb = 0.0;
                }
                else if ((probleme == 2) && (cartesien == 1))
                {
                    xb = a / density * i1;
                    yb = a / range * j1;
                }
                else
                {
                    xb = xel[j1] / density * i1 + (xel[j1] / density) / 2.0;
                    yb = yel[j1] / density * i1 + (yel[j1] / density) / 2.0;
                }
                r = sqrt(xb * xb + yb * yb);
                if (probleme == 1)
                    T[i1][j1] = beta / (2 * k) * (r * r - R * R);
                if (probleme == 2)
                {
                    temp = 0.0;
                    for (i = 1; i < 100; i += 2)
                        temp = temp + (1.0 / (i * i * i)) * pow(-1.0, (i - 1) / 2.0) * (1 - cosh((i * pi * yb) / (2.0 * a)) / cosh((i * pi) / 2.0)) * cos((i * pi * xb) / (2.0 * a));
                    T[i1][j1] = -32 * beta * a * a / (pi * pi * pi * k) * temp;
                }
            }
        time2 = clock();
        find_minmax();
        std::cout << "\nCalcul effectué\n<ESPACE> pour voir la solution...";
        //getch();
        visu(); // Visualisation graphique des résultats.
    }
}

//--------------------------------------------------------------------
//  Procédure main() : boucle principale
//--------------------------------------------------------------------

//--------------------------------------------------------------------
// Routine de génération d'une géométrie donnée et sauvegarde
// dans un fichier *.DAT
//--------------------------------------------------------------------

//void tester()
void generate()
{
    int i;
    char nom_fich[50];
    /*
    void fillvector(double *, double, double, int);
    void create_vectors(), visu();
    */
    R = 1;
    N = 50;
    zoom = 200.0 / R;
    pi = 4 * atan(1);
    range = N;
    probleme = 3;
    create_vectors();

    // Génération:
    fillvector(alpha, 0.0, (2 * pi) / N, N + 1);
    for (i = 0; i < N + 1; i++)
    {
        xf[i] = (R - 0.2 + 0.2 * cos(3 * alpha[i])) * cos(alpha[i]);
        yf[i] = (R - 0.2 + 0.2 * cos(3 * alpha[i]) * 0.01 * (-alpha[i] * alpha[i] * alpha[i] + 2 * pi)) * sin(alpha[i]);
    }
    for (i = 0; i < N; i++)
    {
        xel[i] = (xf[i] + xf[i + 1]) / 2;
        yel[i] = (yf[i] + yf[i + 1]) / 2;
    }
    // Sortie vers fichier.DAT
    std::cout << "\nNom du fichier (.DAT) :";
    //gets(nom_fich);
    std::ofstream fich(nom_fich, std::ios::out);
    fich << N;
    fich << "\n"
         << zoom;
    for (i = 0; i <= N; i++)
    {
        fich << "\n"
             << xf[i];
        fich << "\n"
             << yf[i];
    }
    fich.close();
    calcul = 0;
    visu();
}



