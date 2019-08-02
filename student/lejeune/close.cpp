#include <math.h>
#include <iostream>
#include <fstream>

double Q0 = 0.1, // débit initial
    H0 = 50,     // hauteur manométrique initiale
    Hs = 70,     // hauteur manométrique à débit nul
    D = 0.25,    // diamètre de la conduite
    L = 400,     // longueur de la conduite
    a1 = 0,      // cte pompe
    a = 1200,    // célérité de l'onde
    f = 0.02,    // coefficient de frottement
    tau = 0.6;   // paramètre d'ouverture de vanne

int divs = 5,  // nbre de tronçons de la conduite
    tmax = 90; // nbre de pas de temps

double a2 = (H0 - Hs - a1 * Q0) / (pow(Q0, 2)), // cte pompe
    Section = M_PI * pow(D, 2) / 4,
       g = 9.81,
       B = a / (g * Section),                            // voir rappel
    R = f * (L / divs) / (2 * g * D * pow(Section, 2)),  //  théorique
    C1 = f * pow(Q0, 2) / (2 * g * D * pow(Section, 2)), // pertes de charge
    Cv = pow(Q0 * tau, 2) / (2 * (H0 - C1 * L));         // cte vanne

double **Q, **H;
double CM, CP, rho;

std::ofstream sortie("CLOSE.M", std::ios::out); // ouverture fichier MatLab

void main()
{
    Q = new double *[divs + 1]; // déclaration des tableaux
    H = new double *[divs + 1]; // de débit et de charge
    for (int i = 0; i <= divs; i++)
    {
        Q[i] = new double[tmax];
        H[i] = new double[tmax];
    }
    for (int i = 0; i <= divs; i++) // conditions initiales
    {
        Q[i][0] = Q0;
        H[i][0] = H0 - C1 * i * L / divs;
    }
    for (int k = 1; k < tmax; k++)
    {
        // condition limite à la pompe

        CM = H[1][k - 1] - B * Q[1][k - 1] + R * Q[1][k - 1] * abs(Q[1][k - 1]);

        // choix du bon débit (fct. du signe de a2)

        rho = sqrt(pow(B - a1, 2) + 4 * a2 * (CM - Hs));
        if (a2 > 0)
            Q[0][k] = (B - a1 + rho) / (2 * a2);
        else
            Q[0][k] = (B - a1 - rho) / (2 * a2);

        H[0][k] = Hs + Q[0][k] * (a1 + a2 * Q[0][k]);

        // calcul des points intermédiaires

        for (int i = 1; i < divs; i++)
        {
            CP = H[i - 1][k - 1] + B * Q[i - 1][k - 1] + R * Q[i - 1][k - 1] * abs(Q[i - 1][k - 1]);
            CM = H[i + 1][k - 1] - B * Q[i + 1][k - 1] - R * Q[i + 1][k - 1] * abs(Q[i + 1][k - 1]);
            Q[i][k] = (CP - CM) / (2 * B);
            H[i][k] = (CP + CM) / 2;
        }

        // condition limite à la vanne

        CP = H[divs - 1][k - 1] + B * Q[divs - 1][k - 1] - R * Q[divs - 1][k - 1] * abs(Q[divs - 1][k - 1]);
        Q[divs][k] = -B * Cv + sqrt(pow(B * Cv, 2) + 2 * Cv * CP);
        H[divs][k] = CP - B * Q[divs][k];
    }

    for (int j = 0; j < tmax; j++) // Transfert vers MatLab
    {
        for (int i = 0; i <= divs; i++)
        {
            sortie << "H(" << i + 1 << "," << j + 1 << ")=" << H[i][j] << ";";
            sortie << "Q(" << i + 1 << "," << j + 1 << ")=" << Q[i][j] << ";\n";
        }
        sortie << "t(" << j + 1 << ")=" << j * (L / (divs * a)) << ";\n";
    }
    for (int i = 0; i <= divs; i++)
        sortie << "x(" << i + 1 << ")=" << i * L / divs << ";\n";
    sortie << "figure(1); plot(t,H(" << divs + 1 << ",:),'k'); grid\n";
    sortie << "  title('Charge a la vanne'), xlabel('t [s]'),";
    sortie << "  ylabel('Hv [m]')\n";
    sortie << "figure(2); plot(t,Q(1,:),'k'); grid\n";
    sortie << "  title('Debit a la pompe'), xlabel('t [s]'),";
    sortie << "  ylabel('Qp [m^3/s]')\n";
    sortie << "figure(3); mesh(t,x,Q); grid\n";
    sortie << "  title('Debit'), xlabel('t [s]'), ylabel(' x [m]'),";
    sortie << "  zlabel('Q [m^3/s]')\n";
    sortie << "figure(4); mesh(t,x,H); grid\n";
    sortie << "  title('Charge'), xlabel('t [s]'), ylabel('x [m]'),";
    sortie << "  zlabel('H [m]'),view(10,40)\n";

    sortie.close();
}