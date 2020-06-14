#include "problem.h"
#include <iostream>
#include <fstream>
#include "plotwin.h"
#include "couplage.h"
#include "bar.h"
#include "light.h"
#include "mesh.h"
#include "newmark.h"
#include "resfiles.h"

Problem::Problem() {}

void
Problem::solve()
{
    // parametres physiques
    double kappa = bar.k;
    double rho = bar.rho;
    double cv = bar.cv;
    double E = bar.E;
    double alpha = bar.alpha;
    double T0 = bar.T0;
    double L = bar.L;

    bar.save("xbar.m");

    light.save("xlight.m");

    int m = msh.m;

    int nt = nmark.nt;
    double dt = nmark.dt;
    double gamma = nmark.gamma;
    double beta = nmark.beta;

    msh.save("xmsh.m");
    nmark.save("xnmark.m");

    // matrices utiles
    std::cout << "init matrices/vectors...\n";
    gmm::row_matrix<gmm::wsvector<double>> A2(2 * m, 2 * m);
    gmm::row_matrix<gmm::wsvector<double>> A1(2 * m, 2 * m);
    gmm::row_matrix<gmm::wsvector<double>> A0(2 * m, 2 * m);

    gmm::clear(A2);
    gmm::clear(A1);
    gmm::clear(A0);

    double dx = L / (m - 1);
    std::vector<double> x(m);
    for (int i = 0; i < m; ++i)
        x[i] = -L / 2. + i * dx;

    // Initialisation des 3 matrices A0, A1, A2:
    for (int i = 0; i < m; i++)
    {
        if (i == 0)
        {
            A2(i + m, i + m) = 2 * rho * dx / 6;
            A2(i + m, i + 1 + m) = rho * dx / 6;

            A1(i, i) = 2 * rho * cv * dx / 6;
            A1(i, i + 1) = rho * cv * dx / 6;

            A1(i, i + m) = E * alpha * T0 / 2;
            A1(i, i + 1 + m) = E * alpha * T0 / 2;

            A0(i, i) = kappa / dx;
            A0(i, i + 1) = -kappa / dx;

            A0(i + m, i) = E * alpha / 2;
            A0(i + m, i + 1) = E * alpha / 2;

            A0(i + m, i + m) = E / dx;
            A0(i + m, i + 1 + m) = -E / dx;
        }
        else if (i == m - 1)
        {
            A2(i + m, i + m - 1) = rho * dx / 6;
            A2(i + m, i + m) = 2 * rho * dx / 6;

            A1(i, i - 1) = rho * cv * dx / 6;
            A1(i, i) = 2 * rho * cv * dx / 6;

            A1(i, i + m - 1) = -E * alpha * T0 / 2;
            A1(i, i + m) = -E * alpha * T0 / 2;

            A0(i, i - 1) = -kappa / dx;
            A0(i, i) = kappa / dx;

            A0(i + m, i - 1) = -E * alpha / 2;
            A0(i + m, i) = -E * alpha / 2;

            A0(i + m, i + m - 1) = -E / dx;
            A0(i + m, i + m) = E / dx;
        }
        else
        {
            A2(i + m, i - 1 + m) = rho * dx / 6;
            A2(i + m, i + m) = 4 * rho * dx / 6;
            A2(i + m, i + 1 + m) = rho * dx / 6;

            A1(i, i - 1) = rho * cv * dx / 6;
            A1(i, i) = 4 * rho * cv * dx / 6;
            A1(i, i + 1) = rho * cv * dx / 6;

            A1(i, i - 1 + m) = -E * alpha * T0 / 2;
            A1(i, i + 1 + m) = E * alpha * T0 / 2;

            A0(i, i - 1) = -kappa / dx;
            A0(i, i) = 2 * kappa / dx;
            A0(i, i + 1) = -kappa / dx;

            A0(i + m, i - 1) = -E * alpha / 2;
            A0(i + m, i + 1) = E * alpha / 2;

            A0(i + m, i - 1 + m) = -E / dx;
            A0(i + m, i + m) = 2 * E / dx;
            A0(i + m, i + 1 + m) = -E / dx;
        }
    }

    // =================
    std::vector<double> Fnplus1(2 * m), Fn(2 * m), Fnmoins1(2 * m);
    std::vector<double> Znplus1(2 * m), Zn(2 * m), Znmoins1(2 * m);
    gmm::clear(Fnplus1);
    gmm::clear(Fn);
    gmm::clear(Fnmoins1);
    gmm::clear(Znplus1);
    gmm::clear(Zn);
    gmm::clear(Znmoins1);

    // Initialisation des 5 vecteurs (on initialise pas Znplus1):

    for (int i = 0; i < m; i++)
    {
        Zn[i] = T0;
        Znmoins1[i] = T0;
    }
    Fn[(m - 1) / 2] = light.eval(0.0);
    Fnplus1[(m - 1) / 2] = light.eval(dt);

    // Création de la matrice Aprime et du vecteur bprime selon le schéma de
    // nmark. On résoudra Aprime*Znplus1 = bprime

    // Aprime
    gmm::row_matrix<gmm::wsvector<double>> Aprime(2 * m, 2 * m);
    gmm::add(A2, gmm::scaled(A1, gamma * dt), Aprime);
    gmm::add(gmm::scaled(A0, beta * dt * dt), Aprime);
    applyBC(Aprime, 0);
    applyBC(Aprime, m - 1);
    applyBC(Aprime, m);
    applyBC(Aprime, 2 * m - 1);

    // Ap1
    gmm::row_matrix<gmm::wsvector<double>> Ap1(2 * m, 2 * m);
    gmm::add(gmm::scaled(A2, 2.0), gmm::scaled(A1, -(1.0 - 2.0 * gamma) * dt),
             Ap1);
    gmm::add(gmm::scaled(A0, -(0.5 + gamma - 2 * beta) * dt * dt), Ap1);
    // Ap2
    gmm::row_matrix<gmm::wsvector<double>> Ap2(2 * m, 2 * m);
    gmm::add(gmm::scaled(A2, -1.0), gmm::scaled(A1, -(gamma - 1.0) * dt), Ap2);
    gmm::add(gmm::scaled(A0, -(0.5 - gamma + beta) * dt * dt), Ap2);
    // second membre
    std::vector<double> bprime(2 * m);

    // Calcul de la matrice de préconditionnement et définition du critère
    // d'erreur
    std::cout << "preconditionner...\n";
    gmm::ilutp_precond<gmm::row_matrix<gmm::wsvector<double>>> P(Aprime, m, 0.);

    ///////////////////////////////
    // Boucle sur les pas de temps//
    ///////////////////////////////
    std::cout << "time integration...\n";
    double t = 0.0;

    plot.init(x, Zn);
    plot.update();
    double ymin = T0;
    double ymax = T0 + light.Q * bar.L / 4.0 / bar.k;
    plot.ymin = ymin - (ymax - ymin) * 0.1;
    plot.ymax = ymin + (ymax - ymin) * 1.4;

    results.init(Zn, t, Fn[(m - 1) / 2]);
    results.update();

    for (int k = 1; k < nt; k++)
    {
        t = k * dt;

        // calcul du second membre
        gmm::mult(Ap1, Zn, bprime);
        gmm::mult_add(Ap2, Znmoins1, bprime);
        gmm::add(gmm::scaled(Fnplus1, beta * dt * dt), bprime);
        gmm::add(gmm::scaled(Fn, (0.5 + gamma - 2 * beta) * dt * dt), bprime);
        gmm::add(gmm::scaled(Fnmoins1, (0.5 - gamma + beta) * dt * dt), bprime);
        // CL
        bprime[0] = T0;
        bprime[m - 1] = T0;
        bprime[m] = 0.0;
        bprime[2 * m - 1] = 0.0;

        // Résolution de l'équation
        gmm::iteration iter(1.e-6); // iter.set_noisy(1);
        gmm::gmres(Aprime, Znplus1, bprime, P, 100, iter);

        // Actualisation des vecteurs F et des 2 vecteurs Z (Zn-1 et Zn) pour le
        // pas de temps suivant.
        Fnmoins1[(m - 1) / 2] = Fn[(m - 1) / 2];
        Fn[(m - 1) / 2] = Fnplus1[(m - 1) / 2];
        Fnplus1[(m - 1) / 2] = light.eval(t + dt);

        gmm::copy(Zn, Znmoins1); // Zn-1 = Zn
        gmm::copy(Znplus1, Zn);  // Zn = Zn+1

        results.update();
        plot.update();
    }
}

void
Problem::applyBC(gmm::row_matrix<gmm::wsvector<double>> &mat, int line)
{
    for (size_t j = 0; j < gmm::mat_ncols(mat); ++j)
        mat(line, j) = 0.0;
    mat(line, line) = 1.0;
}
