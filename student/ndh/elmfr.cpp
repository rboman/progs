/*********************************************************************
 *                                                                   *
 *	      Travail N.D.H. : Eléments aux frontiéres               *
 *            ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   	     *
 *	      Version C++    derniére modif.: 10.12.96               *
 *                                                                   *
 *********************************************************************
 *  Programme principal : ELMFR.CPP                                  *
 *  Compilation : utiliser le modéle 'large'.                        *
 *********************************************************************/

#include "elmfr.h"

//--------------------------------------------------------------------
// Routine de définition de la géométrie :
//  Remplit les vecteurs xf,yf et xel,yel.
//  . si probleme=1 -> création d'un cercle.
//  . si probleme=2 -> création d'un carré.
//--------------------------------------------------------------------

void define_geometry()
{
    int i, j;
    void fillvector(float *, float, float, int);

    if (probleme == 1)
    {
        fillvector(alpha, 0.0, (2 * pi) / N, N + 1);
        for (i = 0; i < N + 1; i++)
        {
            xf[i] = R * cos(alpha[i]);
            yf[i] = R * sin(alpha[i]);
        }
    }
    else if (probleme == 2)
    {
        j = N / 4;
        N = 4 * j;
        fillvector(alpha, -a, (2 * a) / j, j + 1);
        for (i = 0; i <= j; i++)
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
    for (i = 0; i < N; i++)
    {
        xel[i] = (xf[i] + xf[i + 1]) / 2;
        yel[i] = (yf[i] + yf[i + 1]) / 2;
    }
}

//--------------------------------------------------------------------
// Routine d'évaluation d'un élém. des matrices G et H.
//   .reéoit -les indices i et j de l'élém. é calculer.
//           -les coord. x,y de l'origine des axes.
//--------------------------------------------------------------------

void eval_GH(float *g, float *h, int i, int j, float x, float y)
{
    int t, tt;
    float dx, dy, dL, temp, r, nx, ny;
    void fillvector(float *, float, float, int);

    if (j == i)
    { // terme diagonal -> on applique les formules spéciales.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        dx = xf[i + 1] - xf[i];
        dy = yf[i + 1] - yf[i];
        dL = sqrt(dx * dx + dy * dy);
        *g = dL / (2 * pi) * (log(2 / dL) + 1);
        *h = 0.5;
    }
    else
    { // cas général d'un terme non diagonal.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // calcul de la normale (normée) é l'élément:
        nx = yf[j + 1] - yf[j];
        ny = xf[j] - xf[j + 1];
        temp = sqrt(nx * nx + ny * ny);
        nx = nx / temp;
        ny = ny / temp;

        // calcul des coord. des points d'intégration (xint,yint):
        fillvector(xint, xf[j], (xf[j + 1] - xf[j]) / istep, istep + 1);
        fillvector(yint, yf[j], (yf[j + 1] - yf[j]) / istep, istep + 1);

        // évaluation des deux fonctions é intégrer sur l'élément
        // et stockage des valeurs dans fct et fct2:
        for (t = 0; t < istep + 1; t++)
        {
            temp = sqrt((xint[t] - x) * (xint[t] - x) + (yint[t] - y) * (yint[t] - y));
            fct[t] = (log(1.0 / temp) / (2 * pi));
            fct2[t] = (-nx * (xint[t] - x) - ny * (yint[t] - y)) / (2 * pi * temp * temp);
        }

        // initialisation des éléments é calculer:
        *g = 0.0;
        *h = 0.0;

        // calcul de la longueur d'un pas d'intégration:
        dx = xint[1] - xint[0];
        dy = yint[1] - yint[0];
        dL = sqrt(dx * dx + dy * dy);

        // intégration de Newton-Cotes:
        for (t = 0; t < istep - ideg + 1; t += ideg)
            for (tt = 0; tt <= ideg; tt++)
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
    float temp, r, xb, yb;
    void titre(), destroy_aux(), create_aux(), create_GH(), eval_u();
    void destroy_GH(), visu(), fillvector(float *, float, float, int);
    void gauss(int, float **, float *, float *);
    void mmv(int, float **, float *, float *);
    void eval_GH(float *, float *, int, int, float, float);
    void copy_block(float **, int, int, int, int, int);

    clrscr();
    titre();
    if (((probleme < 1) || (probleme > 2)) && (type == 2))
    { // Cas du probléme qcq. avec calculs optimisés.
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        cout << "\nPas de solution rapide pour un probléme QCQ !\n<ESPACE>";
        getch();
    }
    else
    {
        time1 = clock(); // on commence é compter le temps CPU.
        calcul = 1;      // le calcul va étre effectué.
        destroy_aux();   // libération de la mémoire.
        cout << "\n\nCréation des matrices H et G...";
        create_GH();
        cout << "Ok\nCalcul des matrices H et G...";
        if (type == 1)
            // Cas du probléme non optimisé:
            // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            for (i = 0; i < N; i++)
                for (j = 0; j < N; j++)
                    eval_GH(&(G[i][j]), &(H[i][j]), i, j, xel[i], yel[i]);
        else
        { // Cas du probléme optimisé:
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
        cout << "Ok\nRésolution de G q = H u...";
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
        cout << "Ok\nDestruction des matrices H et G...";
        destroy_GH();

        cout << "Ok\nCalcul des T intérieures...";
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
        cout << "Ok\nSolution :";
        for (i = 0; i < density; i++)
            cout << "\n"
                 << T[i][0];

        // Visualisation graphique:
        cout << "\n       <SPACE> pour solution graphique";
        getch();
        visu();
    }
}

//--------------------------------------------------------------------
// Routine de calcul des tempétatures exactes (dans le tableau T).
//--------------------------------------------------------------------

void eval_Texact()
{
    void titre(), visu(), find_minmax();
    int i1, j1, i;
    float temp, xb, yb, r;

    clrscr();
    titre();
    if ((probleme < 1) || (probleme > 2))
        cout << "\n\nPas de solution exacte disponible !";
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
        cout << "\nCalcul effectué\n<ESPACE> pour voir la solution...";
        getch();
        visu(); // Visualisation graphique des résultats.
    }
}

//--------------------------------------------------------------------
//  Procédure main() : boucle principale
//--------------------------------------------------------------------

void main()
{
    // Initialisation des variables
    // ----------------------------
    char entree[20];
    int i, j, t, i1, j1, prm, exit = 0, choix;

    float nx, ny, temp, dL, dx, dy, r;
    void tester(), create_vectors(), define_geometry(), titre();
    void full_calcul(), input_data(), load_data(), visu();
    void eval_Texact(), save_Mfile();

    // Initialisation
    // --------------
    pi = 4 * atan(1);
    d_old = density;
    range = N;

    create_vectors();
    define_geometry();

    // Menu
    // ----

    while (exit == 0)
    {
        clrscr();
        titre();
        cout << "\n\nProbléme courant :";
        if (probleme == 1)
            cout << " CERCLE de rayon a";
        else if (probleme == 2)
            cout << " CARRE de cété a";
        else
            cout << "QUELCONQUE";
        cout << "\n\n\t [1]  Lancer le calcul complet.";
        cout << "\n\t [2]  Lancer le calcul rapide.";
        cout << "\n\t [3]  Paramétres.";
        cout << "\n\t [4]  Charger fichier données.";
        cout << "\n\t [5]  Visualisation graphique.";
        cout << "\n\t [6]  Evaluation de la solution analytique.";
        cout << "\n\t [7]  Sauvegarde vers MATLAB";
        cout << "\n\t [0]  Quitter.";
        cout << "\n\nChoix \?";
        cout << "\n\n\n\nFLOPS     : non disponible";
        cout << "\nTemps CPU : " << (double)(time2 - time1) / CLK_TCK << " sec.";
        choix = getch();
        switch (choix)
        {
        case '1':
        {
            type = 1;
            full_calcul();
        }
        break;
        case '2':
        {
            type = 2;
            full_calcul();
        }
        break;
        case '3':
            input_data();
            break;
        case '4':
            load_data();
            break;
        case '5':
            visu();
            break;
        case '6':
            eval_Texact();
            break;
        case '7':
            save_Mfile();
            break;
        case '0':
            exit = 1;
            break;
        }
    }
    clrscr();
}

//--------------------------------------------------------------------
// Routine de génération d'une géométrie donnée et sauvegarde
// dans un fichier *.DAT
//--------------------------------------------------------------------



//void tester()
void generate()
{
    int i;
    char nom_fich[50];
    void fillvector(float *, float, float, int);
    void create_vectors(), visu();

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
    cout << "\nNom du fichier (.DAT) :";
    gets(nom_fich);
    ofstream fich(nom_fich, ios::out);
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



