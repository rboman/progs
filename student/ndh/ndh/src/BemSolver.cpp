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

#include "BemSolver.h"
using namespace ndh;

// VARIABLES GLOBALES! ------------------------
BemSolver::BemSolver()
{
    N = 40;            // Nombre d'�l�ments fronti�res sur le contour.
    istep = 20;        // Nombre de pas d'int�gration sur un �l�ment.
    density = 15;      // Densit� de visualisation de la solution
                       // (nombre de mailles sur un rayon).
    ideg = 1;          // Type d'int�gration de Newton-Cotes
                       // (1=trap�ze, 2=Simpson,...).
    type = FULL;       // M�thode de calcul (1=full, 2=sym�trique).
    maillag = 1;       // 1=Dessine le maillage.
    probleme = CIRCLE; // Type de probl�me (1=cercle, 2=carr�, 3=qcq.).
    whitebg = 1;       // 1=Fond blanc pour l'impression.
    cartesien = false; // 1=maillage rectangulaire (density x density)
                       // (uniquement pour le carr�).
    calcul = 0;        // 1=calculs effectu�s.

    // int pr�c�demment pas init...
    d_old = 0; // Ancienne valeur de la densit� (utile pour
               // d�truire correctement le tableau des T).
    range = 0; // Nbre de ray. sur lesquels la sol. est calcul�e.
    d_old = density;
    range = N;

    // clock_t
    time1 = 0;
    time2 = 0; // temps de d�but et de fin de calcul.

    // doubles
    xo = 220;
    yo = 240;           // (x,y) de l'origine des axes absolus.
    zoom = 200.0 / 1.2; // Zoom de visualisation.

    alpha = nullptr; // Vecteur temporaire [N].
    xf = nullptr;
    yf = nullptr; // (x,y) des extr�mit�s des �l�ments [N+1].
    xel = nullptr;
    yel = nullptr; // (x,y) des connecteurs [N].
    xint = nullptr;
    yint = nullptr; // (x,y) des points d'int�gration [istep+1].
    fct = nullptr;
    fct2 = nullptr; // Valeurs des fonctions � int�grer [istep+1].
    G1 = nullptr;
    H1 = nullptr; // Vect. auxilaires pour le calcul des T [N].
    u = nullptr;  // Temp�tatures sur les �l�ments [N].
    q = nullptr;  // Flux de chaleur sur les �l�ments [N].
    G = nullptr;
    H = nullptr; // Matrices G et H [N,N].
    T = nullptr; // Tableau des T calcul�es [density,range].

    beta = 80;          // Param�tre du probl�me.
    k = 400;            // Conductivit� thermique.
    R = 1.2;            // Rayon du cercle.
    a = 1.2;            // Longueur du c�t� du carr�.
    pi = 4 * atan(1.0); // 3.141592.

    // doubles precedemment pas init...
    Tmin = 0.0;
    Tmax = 0.0; // Valeurs min et max des T calcul�es.

    // Coefficients de l'int�gration de Newton-Cotes:

    create_vectors();
    define_geometry();
}

// ---------------------------------------------------

#include <stdlib.h>
NDH_API void ndh::clrscr()
{
#ifdef WIN32
    //system("cls");
#endif
}

//--------------------------------------------------------------------
// Routine de d�finition de la g�om�trie :
//  Remplit les vecteurs xf,yf et xel,yel.
//  . si probleme=CIRCLE -> cr�ation d'un cercle.
//  . si probleme=SQUARE -> cr�ation d'un carr�.
//--------------------------------------------------------------------

void BemSolver::define_geometry()
{
    if (probleme == CIRCLE) // cercle
    {
        fillvector(alpha, 0.0, (2 * pi) / N, N + 1);
        for (int i = 0; i < N + 1; i++)
        {
            xf[i] = R * cos(alpha[i]);
            yf[i] = R * sin(alpha[i]);
        }
    }
    else if (probleme == SQUARE) // carr�
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
// Routine d'�valuation d'un �l�m. des matrices G et H.
//   .re�oit -les indices i et j de l'�l�m. � calculer.
//           -les coord. x,y de l'origine des axes.
//--------------------------------------------------------------------

void BemSolver::eval_GH(double *g, double *h, int i, int j, double x, double y)
{
    if (j == i)
    {
        // terme diagonal -> on applique les formules sp�ciales.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        double dx = xf[i + 1] - xf[i];
        double dy = yf[i + 1] - yf[i];
        double dL = sqrt(dx * dx + dy * dy);
        *g = dL / (2 * pi) * (log(2 / dL) + 1);
        *h = 0.5;
    }
    else
    {
        // cas g�n�ral d'un terme non diagonal.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // calcul de la normale (norm�e) � l'�l�ment:
        double nx = yf[j + 1] - yf[j];
        double ny = xf[j] - xf[j + 1];
        double temp = sqrt(nx * nx + ny * ny);
        nx = nx / temp;
        ny = ny / temp;

        // calcul des coord. des points d'int�gration (xint,yint):
        fillvector(xint, xf[j], (xf[j + 1] - xf[j]) / istep, istep + 1);
        fillvector(yint, yf[j], (yf[j + 1] - yf[j]) / istep, istep + 1);

        // �valuation des deux fonctions � int�grer sur l'�l�ment
        // et stockage des valeurs dans fct et fct2:
        for (int t = 0; t < istep + 1; t++)
        {
            double temp = sqrt((xint[t] - x) * (xint[t] - x) + (yint[t] - y) * (yint[t] - y));
            fct[t] = (log(1.0 / temp) / (2 * pi));
            fct2[t] = (-nx * (xint[t] - x) - ny * (yint[t] - y)) / (2 * pi * temp * temp);
        }

        // initialisation des �l�ments � calculer:
        *g = 0.0;
        *h = 0.0;

        // calcul de la longueur d'un pas d'int�gration:
        double dx = xint[1] - xint[0];
        double dy = yint[1] - yint[0];
        double dL = sqrt(dx * dx + dy * dy);

        // int�gration de Newton-Cotes:
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
// Routine d'�valuation des temp�ratures sur chaque �l�ment.
//--------------------------------------------------------------------

void BemSolver::eval_u()
{
    for (int i = 0; i < N; i++)
        u[i] = -beta / (2 * k) * (xel[i] * xel[i] + yel[i] * yel[i]);
}

void BemSolver::exec_full()
{
    type = FULL;
    full_calcul();
}

void BemSolver::exec_sym()
{
    type = SYMMETRIC;
    full_calcul();
}

//--------------------------------------------------------------------
// Routine de calcul des temp�tatures (remplissage du tableau T).
// (Cette routine r�soud le probl�me pos�)
//   .re�oit le 'type' de calculs � effectuer:
//         - type=FULL : calculs sans tenir compte de la sym�trie.
//         - type=SYMMETRIC : calculs optimis�s compte tenu de la sym�trie.
//--------------------------------------------------------------------

void BemSolver::full_calcul()
{
    int i, j, i1, j1, t;
    double temp, r, xb, yb;

    //clrscr();
    //titre();
    if ((probleme == OTHER) && (type == SYMMETRIC))
    {
        // Cas du probl�me qcq. avec calculs optimis�s.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        std::cout << "\nPas de solution rapide pour un probl�me QCQ !\n<ESPACE>";
        //getch();
    }
    else
    {
        time1 = clock(); // on commence � compter le temps CPU.
        calcul = 1;      // le calcul va �tre effectu�.
        destroy_aux();   // lib�ration de la m�moire.

        std::cout << "\n\nCr�ation des matrices H et G...";
        create_GH();

        std::cout << "Ok\nCalcul des matrices H et G...";
        if (type == FULL)
        {
            // Cas du probl�me non optimis�:
            // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for (i = 0; i < N; i++)
                for (j = 0; j < N; j++)
                    eval_GH(&(G[i][j]), &(H[i][j]), i, j, xel[i], yel[i]);
        }
        else
        {
            // Cas du probl�me optimis�:
            // ~~~~~~~~~~~~~~~~~~~~~~~~~
            if (probleme == CIRCLE) // *** CERCLE ***
            {                       // Une seule ligne de H utile:            ******
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
            if (probleme == SQUARE) // *** CARRE ***
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
        // Evaluation des T sur la fronti�re et r�solution du
        // syst�me par Gauss:
        std::cout << "Ok\nR�solution de G q = H u...";
        eval_u();
        if ((type == SYMMETRIC) && (probleme == CIRCLE)) // cas du cercle optimis�
        {
            temp = 0.0;
            for (j = 0; j < N; j++)
                temp = temp + H[0][j] * u[j];
            for (j = 0; j < N; j++)
                alpha[j] = temp;
        }
        else // cas g�n�ral
            mmv(N, H, u, alpha);
        gauss(N, G, q, alpha);

        // Lib�ration de la m�moire occup�e par les matrices G et H:
        std::cout << "Ok\nDestruction des matrices H et G...";
        destroy_GH();

        std::cout << "Ok\nCalcul des T int�rieures...";
        if ((probleme == CIRCLE) && (type == SYMMETRIC))
            range = 1;
        else if ((probleme == SQUARE) && (type == SYMMETRIC))
            range = density;
        else
            range = N;

        if ((probleme == SQUARE) && (type == SYMMETRIC))
            cartesien = true;
        else
            cartesien = false;
        create_aux();

        // Calcul des points xb,yb o� va �tre �valu�e la T.
        for (i1 = 0; i1 < density; i1++)
            for (j1 = 0; j1 < range; j1++)
            {
                if ((probleme == CIRCLE) && (type == SYMMETRIC))
                {
                    xb = R / (density)*i1;
                    yb = 0.0;
                }
                else if ((probleme == SQUARE) && (type == SYMMETRIC))
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
                // Calcul de la solution du probl�me de Poisson
                temp = 0.0;
                for (j = 0; j < N; j++)
                    temp = temp + G1[j] * q[j] - H1[j] * u[j];
                // Calcul de la solution du probl�me pos�
                r = sqrt(xb * xb + yb * yb);
                T[i1][j1] = temp + beta / (2 * k) * r * r;
            }

        time2 = clock(); // les calculs sont termin�s !

        // Affichage de la solution
        std::cout << "Ok\nSolution :";
        for (i = 0; i < density; i++)
            std::cout << "\n"
                      << T[i][0];

        // Visualisation graphique:
        //std::cout << "\n       <SPACE> pour solution graphique";
        //getch();
        //visu();
    }
}

std::vector<double> BemSolver::getSolution()
{
    std::vector<double> vec(density);
    for (int i = 0; i < density; i++)
        vec[i] = T[i][0];
    return vec;
}

//--------------------------------------------------------------------
// Routine de calcul des temp�tatures exactes (dans le tableau T).
//--------------------------------------------------------------------

void BemSolver::eval_Texact()
{
    int i1, j1, i;
    double temp, xb, yb, r;

    clrscr();
    titre();
    if (probleme == OTHER)
        std::cout << "\n\nPas de solution exacte disponible !";
    else
    {
        time1 = clock(); // d�but des calculs.
        calcul = 1;
        for (i1 = 0; i1 < density; i1++)
            for (j1 = 0; j1 < range; j1++)
            {
                if ((probleme == CIRCLE) && (range == 1))
                {
                    xb = R / (density)*i1;
                    yb = 0.0;
                }
                else if ((probleme == SQUARE) && (cartesien == true))
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
                if (probleme == CIRCLE)
                    T[i1][j1] = beta / (2 * k) * (r * r - R * R);
                if (probleme == SQUARE)
                {
                    temp = 0.0;
                    for (i = 1; i < 100; i += 2)
                        temp = temp + (1.0 / (i * i * i)) * pow(-1.0, (i - 1) / 2.0) * (1 - cosh((i * pi * yb) / (2.0 * a)) / cosh((i * pi) / 2.0)) * cos((i * pi * xb) / (2.0 * a));
                    T[i1][j1] = -32 * beta * a * a / (pi * pi * pi * k) * temp;
                }
            }
        time2 = clock();
        find_minmax();
        //std::cout << "\nCalcul effectu�\n<ESPACE> pour voir la solution...";
        //getch();
        //visu(); // Visualisation graphique des r�sultats.
    }
}
//--------------------------------------------------------------------
// Routine de g�n�ration d'une g�om�trie donn�e et sauvegarde
// dans un fichier *.DAT
//--------------------------------------------------------------------

//void tester()
void BemSolver::generate()
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
    probleme = OTHER;
    create_vectors();

    // G�n�ration:
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

//--------------------------------------------------------------------
// Routines de gestion des tableaux dynamiques.
// (cr�ation, destruction,...)
//--------------------------------------------------------------------

void BemSolver::create_aux()
{
    T = new double *[density];
    for (int i = 0; i < density; i++)
        T[i] = new double[range];
}

void BemSolver::create_GH()
{
    H = new double *[N];
    for (int i = 0; i < N; i++)
        H[i] = new double[N];
    G = new double *[N];
    for (int i = 0; i < N; i++)
        G[i] = new double[N];
}

void BemSolver::create_vectors()
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

void BemSolver::destroy_aux()
{
    for (int i = 0; i < d_old; i++)
        delete T[i];
    delete T;
    d_old = density;
}

void BemSolver::destroy_GH()
{
    for (int i = 0; i < N; i++)
        delete H[i];
    delete H;
    for (int i = 0; i < N; i++)
        delete G[i];
    delete G;
}

void BemSolver::destroy_vectors()
{
    delete alpha, xf, yf, xel, yel, xint, yint, u, q, fct, fct2;
    delete G1, H1;
    destroy_aux();
}

// ------------

//--------------------------------------------------------------------
// Routine de modification des param�tres
//--------------------------------------------------------------------

void BemSolver::input_data()
{
    //char entree[20];

    clrscr();
    titre();
    int prob;
    param2("Probl�me (1=cercle,2=carr�,3=autre)", &prob);
    probleme = static_cast<Prb>(prob); // tester!
    param("Beta", &beta);
    param("k", &k);
    if (probleme == CIRCLE)
        param("Rayon", &R);
    else
        param("Cot�", &a);
    param2("Nbre d'�l�ments aux fronti�res", &N);
    if (probleme == SQUARE) // Le nbre d'�l�m. doit �tre un multiple de 4.
    {
        N = 4 * (N / 4);
    } // si le probl�me est le carr�.
    if (N < 2)
        N = 20;
    param2("Nbre de pas d'int�gration par �l�ment", &istep);
    if (istep < 2)
        istep = 5;
    if (probleme == CIRCLE)
        zoom = 200.0 / R;
    else
        zoom = 200.0 / a;
    param2("Type d'int�gration (1=trap�ze,2=Simpson,...,6=Weddle)", &ideg);
    if ((ideg < 1) || (ideg > 6))
        ideg = 1;
    istep = (istep / ideg) * ideg; // le nbre d'intervalles d'int�gr.
    if (istep == 0)
        istep = ideg; // doit �tre un mult. de 'ideg'.
    param2("Densit� de visualisation", &density);
    param2("Maillage (1=on 2=off)", &maillag);
    param2("White Bg (1=on 2=off)", &whitebg);

    // Re-dimensionement des tableaux:
    destroy_vectors();
    create_vectors();
    define_geometry();
}

//--------------------------------------------------------------------
// R�cup�ration d'un fichier de donn�e (chargement)
// (attention : pas de v�rification de l'existence du fichier!)
//--------------------------------------------------------------------

void BemSolver::load_data(std::string const &filename)
{
    range = N;
    probleme = OTHER;

    std::ifstream fich(filename.c_str(), std::ios::in);

    fich >> N; // Lecture du nombre d'�l�ments.
    fich >> zoom;
    destroy_vectors(); // Dimensionnement des tableaux
    create_vectors();  // en cons�quence.
    for (int i = 0; i <= N; i++)
    {
        fich >> xf[i];
        fich >> yf[i];
    }
    for (int i = 0; i < N; i++)
    {
        xel[i] = (xf[i] + xf[i + 1]) / 2;
        yel[i] = (yf[i] + yf[i + 1]) / 2;
    }
    fich.close();
    calcul = 0;
}

//--------------------------------------------------------------------
// Sauvegarde des r�sultats dans un fichier MATLAB (*.M)
//--------------------------------------------------------------------

void BemSolver::save_Mfile(std::string const &filename)
{
    //char nom_fich[50];
    double xb, yb;
    int i1, j1;

    //clrscr();
    //titre();
    //std::cout << "\nNom du fichier (.M) :";
    //gets(nom_fich);

    std::ofstream fich(filename.c_str(), std::ios::out);

    fich << "probleme =" << probleme << ';';
    fich << "\ndensity =" << density << ';';
    fich << "\nrange =" << range << ';';
    fich << "\nistep =" << istep << ';';
    fich << "\nN =" << N << ';';
    fich << "\nideg =" << ideg << ';';
    fich << "\ncpu =" << (double)(time2 - time1) / CLOCKS_PER_SEC << ';';
    fich << "\nTmin =" << Tmin << ';';
    fich << "\nTmax =" << Tmax << ';';
    for (i1 = 0; i1 < density; i1++)
        for (j1 = 0; j1 < range; j1++)
        {
            if ((probleme == CIRCLE) && (range == 1))
            {
                xb = R / (density)*i1;
                yb = 0.0;
            }
            else if (cartesien == true)
            {
                xb = a / density * i1;
                yb = a / range * j1;
            }
            else
            {
                xb = xel[j1] / density * i1 + (xel[j1] / density) / 2.0;
                yb = yel[j1] / density * i1 + (yel[j1] / density) / 2.0;
            }
            fich << "\nxb(" << i1 + 1 << "," << j1 + 1 << ")=" << xb << ";";
            fich << "\nyb(" << i1 + 1 << "," << j1 + 1 << ")=" << yb << ";";
            fich << "\nT(" << i1 + 1 << "," << j1 + 1 << ")=" << T[i1][j1] << ";";
        }
    if ((probleme == CIRCLE) && (range == 1)) // Commandes de visualisation
        fich << "\nplot(xb,T); grid;";
    else
        fich << "\nmesh(xb,yb,T); grid;";
    fich.close();
}

void BemSolver::find_minmax()
{
    Tmin = 1e10;
    Tmax = 1e-10;
    for (int i = 0; i < density; i++)
        for (int j = 0; j < range; j++)
        {
            if (T[i][j] > Tmax)
                Tmax = T[i][j];
            if (T[i][j] < Tmin)
                Tmin = T[i][j];
        }
}
