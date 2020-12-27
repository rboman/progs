
#include "student.h"
#include <gmm/gmm.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <math.h>
#include "gnuplot.h"
#include "bar.h"
#include "light.h"

void
student(Bar const &bar)
{
    Light light;
    // parametres physiques
    double kappa = bar.k;
    double rho = bar.rho;
    double cv = bar.cv;
    double Q = light.Q;
    double f = light.f;
    double E = bar.E;
    double alpha = bar.alpha;
    double T0 = bar.T0;
    double L = bar.L;

    // parametres numériques
    int m = 101;          // nombre de noeuds (doit être impair!)
    int k = 0;            // numero du pas de temps
    int nstep = 500;      // nombre de pas de temps
    double deltat = 1e-6; // pas de temps
    double gamma = 0.501; // 0.5;            // newmark
    double beta = 0.255;  // 0.25;           // newmark

    // matrices utiles
    std::cout << "init matrices/vectors...\n";
    gmm::row_matrix<gmm::wsvector<double>> A2(
        2 * m, 2 * m); // Suivant le schéma de Newmark
    gmm::row_matrix<gmm::wsvector<double>> A1(
        2 * m, 2 * m); // Suivant le schéma de Newmark
    gmm::row_matrix<gmm::wsvector<double>> A0(
        2 * m, 2 * m); // Suivant le schéma de Newmark
    gmm::row_matrix<gmm::wsvector<double>> Aprime(2 * m, 2 * m);
    std::vector<double> Fnplus1(2 * m), Fn(2 * m), Fnmoins1(2 * m);
    std::vector<double> Znplus1(2 * m), Zn(2 * m), Znmoins1(2 * m);
    std::vector<double> bprime(2 * m);

    gmm::clear(A2);
    gmm::clear(A1);
    gmm::clear(A0);
    gmm::clear(Aprime);

    // Initialisation des 3 matrices A0, A1, A2 et des 5 vecteurs (on initialise
    // pas Znplus1):
    for (int i = 0; i < m; i++)
    {
        if (i == 0)
        {
            A2(i + m, i + m) = rho * 2 * L / (6 * (m - 1));
            A2(i + m, i + 1 + m) = rho * 1 * L / (6 * (m - 1));
            A1(i, i) = rho * cv * 2 * L / (6 * (m - 1));
            A1(i, i + 1) = rho * cv * 1 * L / (6 * (m - 1));
            A1(i, i + m) = -E * alpha * T0 * (-0.5);
            A1(i, i + 1 + m) = -E * alpha * T0 * (-0.5);
            A0(i, i) = kappa * 1 * (m - 1) / L;
            A0(i, i + 1) = kappa * (-1 * (m - 1) / L);
            A0(i + m, i) = -E * alpha * (-0.5);
            A0(i + m, i + 1) = -E * alpha * (-0.5);
            A0(i + m, i + m) = E * 1 * (m - 1) / L;
            A0(i + m, i + 1 + m) = E * (-1 * (m - 1) / L);
            Fnplus1[i] = 0;
            Fnplus1[i + m] = 0;
            Fn[i] = 0;
            Fn[i + m] = 0;
            Fnmoins1[i] = 0;
            Fnmoins1[i + m] = 0;
            Zn[i] = T0;
            Zn[i + m] = 0;
            Znmoins1[i] = T0;
            Znmoins1[i + m] = 0;
        }
        else if (i == m - 1)
        {
            A2(i + m, i + m) = rho * 2 * L / (6 * (m - 1));
            A2(i + m, i - 1 + m) = rho * 1 * L / (6 * (m - 1));
            A1(i, i) = rho * cv * 2 * L / (6 * (m - 1));
            A1(i, i - 1) = rho * cv * 1 * L / (6 * (m - 1));
            A1(i, i + m) = -E * alpha * T0 * (0.5);
            A1(i, i - 1 + m) = -E * alpha * T0 * (0.5);
            A0(i, i) = kappa * 1 * (m - 1) / L;
            A0(i, i - 1) = kappa * (-1 * (m - 1) / L);
            A0(i + m, i) = -E * alpha * (0.5);
            A0(i + m, i - 1) = -E * alpha * (0.5);
            A0(i + m, i + m) = E * 1 * (m - 1) / L;
            A0(i + m, i - 1 + m) = E * (-1 * (m - 1) / L);
            Fnplus1[i] = 0;
            Fnplus1[i + m] = 0;
            Fn[i] = 0;
            Fn[i + m] = 0;
            Fnmoins1[i] = 0;
            Fnmoins1[i + m] = 0;
            Zn[i] = T0;
            Zn[i + m] = 0;
            Znmoins1[i] = T0;
            Znmoins1[i + m] = 0;
        }
        else
        {
            A2(i + m, i + m) = rho * 4 * L / (6 * (m - 1));
            A2(i + m, i - 1 + m) = rho * 1 * L / (6 * (m - 1));
            A2(i + m, i + 1 + m) = rho * 1 * L / (6 * (m - 1));
            A1(i, i) = rho * cv * 4 * L / (6 * (m - 1));
            A1(i, i - 1) = rho * cv * 1 * L / (6 * (m - 1));
            A1(i, i + 1) = rho * cv * 1 * L / (6 * (m - 1));
            A1(i, i - 1 + m) = -E * alpha * T0 * (0.5);
            A1(i, i + 1 + m) = -E * alpha * T0 * (-0.5);
            A0(i, i) = kappa * 2 * (m - 1) / L;
            A0(i, i - 1) = kappa * (-1 * (m - 1) / L);
            A0(i, i + 1) = kappa * (-1 * (m - 1) / L);
            A0(i + m, i - 1) = -E * alpha * (0.5);
            A0(i + m, i + 1) = -E * alpha * (-0.5);
            A0(i + m, i + m) = E * 2 * (m - 1) / L;
            A0(i + m, i - 1 + m) = E * (-1 * (m - 1) / L);
            A0(i + m, i + 1 + m) = E * (-1 * (m - 1) / L);
            Fnplus1[i + m] = 0;
            Fn[i + m] = 0;
            Fnmoins1[i] = 0;
            Fnmoins1[i + m] = 0;
            Zn[i] = T0;
            Zn[i + m] = 0;
            Znmoins1[i] = T0;
            Znmoins1[i + m] = 0;
            if (i == (m - 1) / 2)
            {
                Fn[i] = Q * (1 + cos(2 * M_PI * f * k * deltat - M_PI));
                Fnplus1[i] =
                    Q * (1 + cos(2 * M_PI * f * (k + 1) * deltat - M_PI));
            }
            else
            {
                Fn[i] = 0;
                Fnplus1[i] = 0;
            }
        }
    }

    // Création de la matrice Aprime et du vecteur bprime selon le schéma de
    // Newmark. On résoudra Aprime*Znplus1 = bprime Conditions aux limites
    Aprime(0, 0) = 1;
    Aprime(m - 1, m - 1) = 1;
    Aprime(m, m) = 1;
    Aprime(2 * m - 1, 2 * m - 1) = 1;

    bprime[0] = T0;
    bprime[m - 1] = T0;
    bprime[m] = 0;
    bprime[2 * m - 1] = 0;

    for (int i = 1; i < m - 1; i++)
    {
        for (int j = 0; j < 2 * m; j++)
        {
            Aprime(i, j) = A2(i, j) + gamma * deltat * A1(i, j) +
                           beta * deltat * deltat * A0(i, j); // ne change plus
            Aprime(i + m, j) =
                A2(i + m, j) + gamma * deltat * A1(i + m, j) +
                beta * deltat * deltat * A0(i + m, j); // ne change plus
        }
    }

    // Ecriture des conditions initiales dans le fichier
    std::cout << "opening result file...\n";
    std::ofstream myfile("Temp_couplage.txt");
    for (int i = 0; i < m; i++)
    {
        myfile << Zn[i] << ";";
    }
    myfile << "\n";
    for (int i = 0; i < m; i++)
    {
        myfile << Zn[i + m] << ";";
    }
    myfile << "\n";

    // Calcul de la matrice de préconditionnement et définition du critère
    // d'erreur
    std::cout << "computation of the preconditionner...\n";
    gmm::ilutp_precond<gmm::row_matrix<gmm::wsvector<double>>> P(Aprime, m, 0.);

    ///////////////////////////////
    // Boucle sur les pas de temps//
    ///////////////////////////////
    Gnuplot plot;
    plot("set title 'Output windows'");
    // plot("set linetype 1 lw 2 lc rgb 'blue' pointtype 1");

    std::cout << "time integration...\n";
    for (k = 1; k < nstep; k++)
    {
        // std::cout << "k = " << k << ": t = " << k*deltat << "\n";

        ////////////////////////////
        // Résolution de l'équation//
        ////////////////////////////
        // Calcul de bprime
        for (int i = 1; i < m - 1; i++)
        {
            bprime[i] =
                (2 * A2(i, i) - (1 - 2 * gamma) * deltat * A1(i, i) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat * A0(i, i)) *
                    Zn[i] +
                (-A2(i, i) - (gamma - 1) * deltat * A1(i, i) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i)) *
                    Znmoins1[i];
            bprime[i] +=
                (2 * A2(i, i - 1) - (1 - 2 * gamma) * deltat * A1(i, i - 1) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat * A0(i, i - 1)) *
                    Zn[i - 1] +
                (-A2(i, i - 1) - (gamma - 1) * deltat * A1(i, i - 1) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i - 1)) *
                    Znmoins1[i - 1];
            bprime[i] +=
                (2 * A2(i, i + 1) - (1 - 2 * gamma) * deltat * A1(i, i + 1) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat * A0(i, i + 1)) *
                    Zn[i + 1] +
                (-A2(i, i + 1) - (gamma - 1) * deltat * A1(i, i + 1) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i + 1)) *
                    Znmoins1[i + 1];
            bprime[i] +=
                (2 * A2(i, i + m) - (1 - 2 * gamma) * deltat * A1(i, i + m) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat * A0(i, i + m)) *
                    Zn[i + m] +
                (-A2(i, i + m) - (gamma - 1) * deltat * A1(i, i + m) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i + m)) *
                    Znmoins1[i + m];
            bprime[i] +=
                (2 * A2(i, i - 1 + m) -
                 (1 - 2 * gamma) * deltat * A1(i, i - 1 + m) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat *
                     A0(i, i - 1 + m)) *
                    Zn[i - 1 + m] +
                (-A2(i, i - 1 + m) - (gamma - 1) * deltat * A1(i, i - 1 + m) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i - 1 + m)) *
                    Znmoins1[i - 1 + m];
            bprime[i] +=
                (2 * A2(i, i + 1 + m) -
                 (1 - 2 * gamma) * deltat * A1(i, i + 1 + m) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat *
                     A0(i, i + 1 + m)) *
                    Zn[i + 1 + m] +
                (-A2(i, i + 1 + m) - (gamma - 1) * deltat * A1(i, i + 1 + m) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i, i + 1 + m)) *
                    Znmoins1[i + 1 + m];
            bprime[i] += deltat * deltat *
                         (beta * Fnplus1[i] + (0.5 + gamma - 2 * beta) * Fn[i] +
                          (0.5 - gamma + beta) * Fnmoins1[i]);

            bprime[i + m] =
                (2 * A2(i + m, i) - (1 - 2 * gamma) * deltat * A1(i + m, i) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat * A0(i + m, i)) *
                    Zn[i] +
                (-A2(i + m, i) - (gamma - 1) * deltat * A1(i + m, i) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i + m, i)) *
                    Znmoins1[i];
            bprime[i + m] +=
                (2 * A2(i + m, i - 1) -
                 (1 - 2 * gamma) * deltat * A1(i + m, i - 1) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat *
                     A0(i + m, i - 1)) *
                    Zn[i - 1] +
                (-A2(i + m, i - 1) - (gamma - 1) * deltat * A1(i + m, i - 1) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i + m, i - 1)) *
                    Znmoins1[i - 1];
            bprime[i + m] +=
                (2 * A2(i + m, i + 1) -
                 (1 - 2 * gamma) * deltat * A1(i + m, i + 1) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat *
                     A0(i + m, i + 1)) *
                    Zn[i + 1] +
                (-A2(i + m, i + 1) - (gamma - 1) * deltat * A1(i + m, i + 1) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i + m, i + 1)) *
                    Znmoins1[i + 1];
            bprime[i + m] +=
                (2 * A2(i + m, i + m) -
                 (1 - 2 * gamma) * deltat * A1(i + m, i + m) -
                 (0.5 + gamma - 2 * beta) * deltat * deltat *
                     A0(i + m, i + m)) *
                    Zn[i + m] +
                (-A2(i + m, i + m) - (gamma - 1) * deltat * A1(i + m, i + m) -
                 (0.5 - gamma + beta) * deltat * deltat * A0(i + m, i + m)) *
                    Znmoins1[i + m];
            bprime[i + m] += (2 * A2(i + m, i - 1 + m) -
                              (1 - 2 * gamma) * deltat * A1(i + m, i - 1 + m) -
                              (0.5 + gamma - 2 * beta) * deltat * deltat *
                                  A0(i + m, i - 1 + m)) *
                                 Zn[i - 1 + m] +
                             (-A2(i + m, i - 1 + m) -
                              (gamma - 1) * deltat * A1(i + m, i - 1 + m) -
                              (0.5 - gamma + beta) * deltat * deltat *
                                  A0(i + m, i - 1 + m)) *
                                 Znmoins1[i - 1 + m];
            bprime[i + m] += (2 * A2(i + m, i + 1 + m) -
                              (1 - 2 * gamma) * deltat * A1(i + m, i + 1 + m) -
                              (0.5 + gamma - 2 * beta) * deltat * deltat *
                                  A0(i + m, i + 1 + m)) *
                                 Zn[i + 1 + m] +
                             (-A2(i + m, i + 1 + m) -
                              (gamma - 1) * deltat * A1(i + m, i + 1 + m) -
                              (0.5 - gamma + beta) * deltat * deltat *
                                  A0(i + m, i + 1 + m)) *
                                 Znmoins1[i + 1 + m];
            bprime[i + m] +=
                deltat * deltat *
                (beta * Fnplus1[i + m] + (0.5 + gamma - 2 * beta) * Fn[i + m] +
                 (0.5 - gamma + beta) * Fnmoins1[i + m]);
        }
        // Résolution de l'équation
        gmm::iteration iter(1.e-6);
        // iter.set_noisy(1);
        gmm::gmres(Aprime, Znplus1, bprime, P, 100, iter);

        // Ecriture de la solution dans un fichier
        for (int i = 0; i < m; i++)
        {
            myfile << Znplus1[i] << ";";
        }
        myfile << "\n";
        for (int i = 0; i < m; i++)
        {
            myfile << Znplus1[i + m] << ";";
        }
        myfile << "\n";

        // Actualisation des vecteurs F et des 2 vecteurs Z (Znmoins1 et Zn)
        // pour le pas de temps suivant.
        Fnmoins1[(m - 1) / 2] = Fn[(m - 1) / 2];
        Fn[(m - 1) / 2] = Fnplus1[(m - 1) / 2];
        Fnplus1[(m - 1) / 2] =
            Q * (1 + cos(2 * M_PI * f * (k + 1) * deltat - M_PI));
        for (int i = 1; i < m - 1; i++)
        {
            Znmoins1[i] = Zn[i];
            Znmoins1[i + m] = Zn[i + m];
            Zn[i] = Znplus1[i];
            Zn[i + m] = Znplus1[i + m];
        }

        // plot("plot sin(x)") ;
        // plot("plot '-' with linespoints");
        plot("plot '-' with lines");
        for (int i = 0; i < m; i++)
        {
            std::stringstream str;
            str << -L / 2. + i * L / (m - 1) << " " << Zn[i];
            plot(str.str());
        }
        plot("e");
        plot("set yrange [270:290]");
    }

    std::cout << "closing file...\n";
    myfile.close();
}
