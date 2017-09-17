//   Copyright 2017 Romain Boman
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

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                             DCM: Travail n�2
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                                                                        R.B.
// Vendredi 10.02.95

#include "dcm1.h"

void calcule()
{
    //-------------------------------------------------------------------------
    //            Calcul de la masse et de l'inertie de l'aile
    //-------------------------------------------------------------------------
    Unite[0] = 1;
    h[0] = c0;
    h[1] = (c1 - c0) / enverg;

    m = densite * (2.0 * ep * (11.0 * h - 2.0 * ep * Unite));
    I = h * h * h * (31.0 / 6.0) * ep - 11.0 * h * h * pow(ep, 2) + (26.0 / 3.0) * h * pow(ep, 3);
    I[0] = I[0] - 4.0 / 3.0 * pow(ep, 4);

    MSX[0].masse = Mfuselage;
    MSX[0].x = 0.0;
    MSX[1].masse = Mmoteurs;
    MSX[1].x = enverg / 2.0;
    MSX[2].masse = Mmoteurs;
    MSX[2].x = -enverg / 2.0;
    MSX[3].masse = NULL;
    MSX[3].x = 0.0;
    //clrscr();
    std::cout << "--------\n";
    std::cout << "Masse   : " << m << '\n';
    std::cout << "Inertie : " << I << '\n';
    std::cout << "--------\n";

    //-------------------------------------------------------------------------
    //                   Cr�ation d'une base de polynome
    //-------------------------------------------------------------------------
    p[0] = 1 / sqrt(m.integrale(0.0, enverg) // 1� poly = cte.
                    + (!m).integrale(-enverg, 0.0) + Mfuselage + 2 * Mmoteurs);
    std::cout << "Polynome n�0:" << p << '\n';

    Base_Poly Base(MSX, I, m, MYoung, enverg, p);

    for (i = 2; i < Nmodes + 1; i++) // Ajoute 5 poly.
        KM = Base.ajoute_suivant();
    nopoly = Nmodes;

    ValPro2 = new double[Nmodes];
    ValPro2--; // Tableaux auxil. contenant
    ModPro2 = new double *[Nmodes];
    ModPro2--;                       // les val. & vect. pr.
    for (i = 1; i < Nmodes + 1; i++) // pour (n-1) poly...
    {
        ValPro2[i] = 0.0; // ...et initialisation � 0.
        ModPro2[i] = new double[Nmodes];
        ModPro2[i]--;
        for (j = 1; j <= nopoly; j++)
            ModPro2[i][j] = 0.0;
    }

    //-------------------------------------------------------------------------
    //    Ajoute un polynome et compare les val. et vect. propres avec
    //             ceux et celles de l'it�ration pr�c�dente.
    //-------------------------------------------------------------------------
    rate = 1;
    while (rate == 1)
    {
        KM = Base.ajoute_suivant();
        nopoly++;

        COPY_K = new double *[nopoly]; // Copie la matrice K
        for (i = 0; i < nopoly; i++)   //    dans COPY_K
        {
            COPY_K[i] = new double[nopoly];
            for (j = 0; j < nopoly; j++)
                COPY_K[i][j] = KM[i][j];
        }
        COPY_K--;                     // indice minimal = 1
        for (j = 1; j <= nopoly; j++) //        (pour Jacobi())
            COPY_K[j]--;

        ValPro = new double[nopoly];
        ValPro--;
        ModPro = new double *[nopoly];
        ModPro--;
        for (j = 1; j <= nopoly; j++)
        {
            ModPro[j] = new double[nopoly];
            ModPro[j]--;
        }
        jacobi(COPY_K, nopoly, ValPro, ModPro, nrot);

        //---Trie les VP (Bubble sort)-----------
        j = nopoly;
        while (j != 1)
        {
            i = 0;
            k = 0;
            while (i != j - 1)
            {
                if (ValPro[i + 1] > ValPro[i + 2])
                {
                    dswap(&ValPro[i + 1], &ValPro[i + 2]);
                    swap = Base[i];
                    Base[i] = Base[i + 1];
                    Base[i + 1] = swap;
                    for (compt = 0; compt < nopoly; compt++)
                        dswap(&ModPro[i + 1][compt], &ModPro[i + 2][compt]);
                    k = i;
                }
                i++;
            }
            j = k + 1;
        }

        //---Teste les valeurs obtenues----------
        rate = 0;
        for (i = 1; i < Nmodes + 1; i++)
        {
            if (fabs(ValPro[i]) > 1E-10)
                if (fabs(1.0 - sqrt(ValPro2[i]) / sqrt(ValPro[i])) > PREC)
                    rate = 1;
            ValPro2[i] = ValPro[i];
        }
        for (i = 1; i < Nmodes + 1; i++)
            for (j = 1; j < Nmodes + 1; j++)
            {
                if (fabs(ModPro[i][j]) > 1E-10)
                    if (fabs(1.0 - ModPro2[i][j] / ModPro[i][j]) > PREC2)
                        rate = 1;
                ModPro2[i][j] = ModPro[i][j];
            }

        //---Destruction des tabl. auxil.--------
        if (rate == 1) // Vire les VP & VP dans le
        {              // cas o� la pr�cision n'est
            ValPro++;
            delete ValPro; // pas atteinte
            for (j = 1; j <= nopoly; j++)
            {
                ModPro[j]++;
                delete ModPro[j];
            }
            ModPro++;
            delete ModPro;
        }

        for (j = 1; j <= nopoly; j++) // Vire COPY_K
            COPY_K[j]++;
        COPY_K++;
        for (i = 0; i < nopoly; i++)
            delete COPY_K[i];
        delete COPY_K;
    }
    std::cout << "\n"
              << nopoly << " poly. n�cessaires.\n";
    //getch();

    //---"Nettoie" les modes propres------------
    for (j = 1; j <= nopoly; j++)
        for (k = 1; k <= nopoly; k++)
            if (fabs(ModPro[j][k]) < 1E-10)
                ModPro[j][k] = 0.0;

    //-------------------------------------------------------------------------
    //                       affichage des r�sultats
    //-------------------------------------------------------------------------
    std::cout << "\nValeurs Propres:\n";
    for (j = 1; j <= nopoly; j++)
        std::cout << j << ": " << sqrt(ValPro[j]) << '\n';
    //getch();
    std::cout << "\nVecteurs Propres:\n";
    for (j = 1; j <= nopoly; j++)
    {
        std::cout << j << ": (";
        for (int k = 1; k <= nopoly; k++)
            std::cout << ModPro[j][k] << ",";
        std::cout << ")\n";
    }
    // Base.affiche_K(nopoly); getch();

    //-------------------------------------------------------------------------
    //           Transfert des val. et vect. propres vers Matlab
    //-------------------------------------------------------------------------
    std::cout << "\n<SPACE> pour creation des fichiers *.M\n\n";
    //getch();
    C_to_Matlab_1(ValPro, ModPro, Nmodes);

    //-------------------------------------------------------------------------
    //               Calcul des modes propres Yi(x) -> MP[i]
    //          Cr�ation d'un prog *.M pour afficher les r�sultats
    //-------------------------------------------------------------------------
    MP = new Polynome[nopoly]; // Cr�e un tableau de poly.
    for (i = 0; i < nopoly; i++)
    {
        MP[i][0] = 0.0;
        for (j = 0; j < nopoly; j++)
            MP[i] = MP[i] + ModPro[i + 1][j + 1] * Base[j];
    }

    //---Calcul de la matrice pour MATLAB--------
    XX = new double[np + 1];      // Matrice abcisse.
    MODES = new double *[nopoly]; // Matrice des Yi(x)
    for (i = 0; i < nopoly; i++)
    {
        MODES[i] = new double[np + 1];
        for (j = 0; j < np + 1; j++) // Initialisation � 0.
        {
            XX[j] = 0.0;
            MODES[i][j] = 0.0;
        }
    }

    for (compt = 0, t = (-enverg); compt <= np; t += 2 * enverg / np, compt++)
    {
        XX[compt] = t;
        for (i = 0; i < nopoly; i++)
            MODES[i][compt] += MP[i](t);
    }

    //-------------------------------------------------------------------------
    //                      Calcul des masses r�duites
    //              pour une �ventuelle normalisation des modes
    //-------------------------------------------------------------------------
    mu = new double[nopoly];
    for (i = 0; i < nopoly; i++)
    {
        mu[i] = (m * MP[i] * MP[i]).integrale(0.0, enverg) + ((!m) * MP[i] * MP[i]).integrale(-enverg, 0.0) + MP[i](0) * MP[i](0) * Mfuselage + MP[i](-enverg / 2) * MP[i](-enverg / 2) * Mmoteurs + MP[i](enverg / 2) * MP[i](enverg / 2) * Mmoteurs;
    }

    //---Ecriture dans un fichier *.M-----------
    C_to_Matlab_2();

    //-------------------------------------------------------------------------
    //                    Calcul du moment fl�chissant et
    //             de l'effort tranchant � l'emplanture de l'aile
    //-------------------------------------------------------------------------

    Moment = new double[Nperiod * np2 + 1];
    Tranchant = new double[Nperiod * np2 + 1];
    double alp0 = -2 * F0 * T / pi * MP[0](0.0);

    //---M et T pdt l'appl. de la force---------
    for (t = 0, compt = 0; compt < np2 + 1; t += T / np2, compt++)
    {
        M = M - M;
        M[0] = 0.0;
        M = MP[0] * (MP[0](0.0) * F0 * T * T / (pi * pi) * sin(pi * t / T) + (alp0 + MP[0](0.0) * F0 * T / pi) * t);
        for (i = 3; i < Nmodes; i += 2)
        {
            om = sqrt(ValPro[i]);
            M = M + MP[i - 1] * MP[i - 1](0.0) * (F0 / om * (T / pi * sin(om * t) - om * T * T / (pi * pi) * sin(pi * t / T)) / (1 - om * om * T * T / (pi * pi)));
        }
        M = (M.derive()).derive();
        M = M * MYoung * I;
        Moment[compt] = M(0.0);
        DM = M.derive();
        Tranchant[compt] = DM(0.0);
    }

    //---Calcul de M & T apr�s l'appl. de F-----
    double *alpha;
    alpha = new double[6];
    double *alphap;
    alphap = new double[6];

    alpha[0] = (alp0 + F0 * MP[0](0.0) * T / pi) * T;
    alphap[0] = 0.0;
    for (i = 3; i < Nmodes; i += 2)
    {
        om = sqrt(ValPro[i]);
        alpha[i] = F0 * MP[i - 1](0.0) / om * (T / (pi)*sin(om * T)) / (1 - ((om * om * T * T) / (pi * pi)));
        alphap[i] = F0 * MP[i - 1](0.0) / om * (T / (pi)*om * cos(om * T) + om * T / (pi)) / (1 - ((om * om * T * T) / (pi * pi)));
    }

    for (t = T + T / np2; compt < np2 * Nperiod + 1; t += T / np2, compt++)
    {
        M = M - M;
        M[0] = 0.0;
        M = MP[0] * (alphap[0] * (t - T) + alpha[0]);
        for (i = 3; i < Nmodes; i += 2)
        {
            om = sqrt(ValPro[i]);
            M = M + MP[i - 1] * (alpha[i] * cos(om * (t - T)) + alphap[i] / om * sin(om * (t - T)));
        }
        M = (M.derive()).derive();
        M = M * MYoung * I;
        Moment[compt] = M(0.0);
        DM = M.derive();
        Tranchant[compt] = DM(0.0);
    }

    //---Prog. MATLAB -> Graphe M et T----------
    C_to_Matlab_3();

    std::cout << "\n Programmes Matlab cr��s:";
    std::cout << "\n   -VPVP        -> VP en m�moire";
    std::cout << "\n   -GRAPH       -> trace les modes propres";
    std::cout << "\n   -MT          -> trace M & T";
    std::cout << "\n\n-SPACE-";
    //getch();
}

void dswap(double *a, double *b)
{
    double tmp;
    tmp = *a;
    *a = *b;
    *b = tmp;
}

void titre()
{
    //clrscr();
    std::cout << '\n';
    std::cout << "\t\t    +--------------------------------------+\n";
    std::cout << "\t\t    |          DCM : Travail #2            |\n";
    std::cout << "\t\t    |                                      |\n";
    std::cout << "\t\t    |                          RB fev.'95  |\n";
    std::cout << "\t\t    +--------------------------------------+\n";
}

void main()
{
    char choix;
    int exit = 0;
    while (exit == 0)
    {
        titre();
        std::cout << "\n\n\n     1:-----> Calcul\n";
        std::cout << "\n\     2:-----> Donnees\n";
        std::cout << "\n\     3:-----> Retour au DOS\n\n";

        std::cin >> choix;
        //choix = getch();
        switch (choix)
        {
        case '1':
            calcule();
            break;
        case '2':
            input_data();
            break;
        case '3':
            exit = 1;
            break;
        }
    }
    //clrscr();
}
