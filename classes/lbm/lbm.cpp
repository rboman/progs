#include "lbm.h"

#include <iostream>
#include <random>

LBM::LBM(int _Nx, int _Ny) : Nx(_Nx), Ny(_Ny)
{
    // default parameters
    rho0 = 100; //100; //< average density
    tau = 0.6;  //< collision timescale

    // Lattice speeds / weights

    cxs = {0, 0, 1, 1, 1, 0, -1, -1, -1};
    // cxs.resize(NL);
    // cxs << 0e0, 0e0, 1e0, 1e0, 1e0, 0e0, -1e0, -1e0, -1e0;
    cys = {0, 1, 1, 0, -1, -1, -1, 0, 1};
    // cys.resize(NL);
    // cys << 0e0, 1e0, 1e0, 0e0, -1e0, -1e0, -1e0, 0e0, 1e0;

    // D2Q9 Velocities     --    weights
    //
    //      8     1   2       1/36   1/9   1/36
    //       +   +   +
    //        \  |  /
    //         \ |0/
    //     7 +---o---+ 3      1/9    4/9    1/9
    //         / | \ 
    //        /  |  \ 
    //       +   +   +
    //      6    5    4       1/36   1/9   1/36

    weights.resize(NL);
    weights << 4. / 9, 1. / 9, 1. / 36, 1. / 9,
        1. / 36, 1. / 9, 1. / 36, 1. / 9, 1. / 36;

    double sumw = 0.0;
    for (int l = 0; l < NL; ++l)
        sumw += weights.coeffRef(l);
    //std::cout << "sumw = " << sumw << '\n'; // check that sum(weights)=1

    // store inverse map for reflective BCs
    bcMap = {0, 5, 6, 7, 8, 1, 2, 3, 4};
}

/**
 * @brief allocate things depending on Nx, Ny and compute the initial 
 *        state of the lattice.
 */

void LBM::init()
{
    std::cout << "LBM::init()\n";

    // Mesh
    X.resize(Nx);
    X.setLinSpaced(0.0, Nx - 1);
    Y.resize(Ny);
    Y.setLinSpaced(0.0, Ny - 1);

    // Initial Conditions
    initState();
    initCylinder();

    // allocate temporary array for boundary conditions
    bndryF.resize(NL);
    for (int l = 0; l < NL; ++l)
        bndryF[l].resize(Ny, Nx);

    // allocate vorticity array
    vorticity.resize(Ny, Nx);

    std::cout << "LBM::init() done.\n";
}


void LBM::update()
{
    // Philip Mocz
    drift();
    storeBndryF(); // for reflective bcs

    computeRho();
    computeU();
    collide();
    applyBndryF();
    //applyOutletBC(); // new
    computeVorticity();
    computeUmag();
}

/**
 * @brief Set F, u and rho at time t=0
 */

void LBM::initState()
{
    F.resize(NL);
    for (int l = 0; l < NL; ++l)
        F[l].resize(Ny, Nx);

    std::random_device rd;
    std::default_random_engine eng(rd());
    //std::uniform_real_distribution<double> distr(0.0, 1.0);
    std::normal_distribution<double> distr(0.0, 1.0);

    for (int l = 0; l < NL; ++l)
        for (int i = 0; i < Ny; ++i)
            for (int j = 0; j < Nx; ++j)
            {
                double rnd = distr(eng);
                //std::cout << "f=" << f << '\n';
                F[l].coeffRef(i, j) = 10.0;
                F[l].coeffRef(i, j) += 0.01 * rnd;
            }


    // add perturbation
    for (int i = 0; i < Ny; ++i)
        for (int j = 0; j < Nx; ++j)
        {
            F[Dir::E].coeffRef(i, j) += 2 * (1 + 0.2 * cos(2 * M_PI * X.coeffRef(j) / Nx * 4)); // not very useful
            //F[Dir::E].coeffRef(i, j) += 1;
        }


    // initial density
    rho.resize(Ny, Nx);
    computeRho();

    // scale F
    for (int l = 0; l < NL; ++l)
        F[l] *= rho0 / rho;

    // display statistics on F
    for (int l = 0; l < NL; ++l)
        std::cout << l << ": min(F) = " << F[l].minCoeff() << " max(F) = " << F[l].maxCoeff() << '\n';

    // recompute rho
    computeRho();

    // compute ux, uy
    ux.resize(Ny, Nx);
    uy.resize(Ny, Nx);
    computeU();
}

/**
 * @brief fill cylinder array (1 means "inside the obstacle")
 */

void LBM::initCylinder()
{
    // cylinder
    cylinder.resize(Ny, Nx);
    cylinder.setZero();

    double R2 = (double)Ny / 4;
    R2 *= R2;

    for (int i = 0; i < Ny; ++i)
    {
        double y = Y[i];
        double y2 = y - (double)Ny / 2;
        y2 *= y2;
        for (int j = 0; j < Nx; ++j)
        {
            double x = X[j];
            double x2 = x - (double)Nx / 4;
            x2 *= x2;
            if (x2 + y2 < R2)
                cylinder.coeffRef(i, j) = 1;
        }
    }
}

/**
 * @brief Compute velocity (ux,uy) at each point of the lattice 
 *        from F and density.
 */

void LBM::computeU()
{
    ux.setZero();
    uy.setZero();
    for (int l = 0; l < NL; ++l)
    {
        ux += cxs[l] * F[l];
        uy += cys[l] * F[l];
    }
    ux /= rho;
    uy /= rho;
}

/**
 * @brief Compute density at each point of the lattice from F.
 */

void LBM::computeRho()
{
    rho.setZero();
    for (int l = 0; l < NL; ++l)
        rho += F[l];
}

/**
 * @brief Drift of F[l] tables according to (cx,cy)[l]  
 *        with 0 <= l < NL
 */

void LBM::drift()
{
    Eigen::ArrayXXd tmp(Ny, Nx);
    for (int l = 0; l < NL; ++l)
    {
        //tmp.setZero(); // not necessary
        Eigen::ArrayXXd &Fl = F[l];
        int cx = cxs[l];
        int cy = cys[l];

        for (int i = 0; i < Ny; ++i)
            for (int j = 0; j < Nx; ++j)
            {
                int i1 = i + cy;
                if (i1 >= Ny)
                    i1 -= Ny;
                else if (i1 < 0)
                    i1 += Ny;
                int j1 = j + cx;
                if (j1 >= Nx)
                    j1 -= Nx;
                else if (j1 < 0)
                    j1 += Nx;
                tmp.coeffRef(i1, j1) = Fl.coeffRef(i, j);
            }
        Fl = tmp;
    }
}

void LBM::storeBndryF()
{
    for (int i = 0; i < Ny; ++i)
        for (int j = 0; j < Nx; ++j)
            if (cylinder.coeffRef(i, j) == 1)
            {
                for (int l = 0; l < NL; ++l)
                    bndryF[l].coeffRef(i, j) = F[bcMap[l]].coeffRef(i, j);
            }
}

void LBM::applyBndryF()
{
    for (int i = 0; i < Ny; ++i)
        for (int j = 0; j < Nx; ++j)
            if (cylinder.coeffRef(i, j) == 1)
            {
                for (int l = 0; l < NL; ++l)
                    F[l].coeffRef(i, j) = bndryF[l].coeffRef(i, j);
                ux.coeffRef(i, j) = 0.0;
                uy.coeffRef(i, j) = 0.0;
            }
}

void LBM::applyOutletBC()
{
    for (int l = 0; l < NL; ++l)
    {
        for (int i = 0; i < Ny; ++i)
        {
            F[l].coeffRef(i, Nx-1) = F[l].coeffRef(i,Nx-2);
        }
    }
}

void LBM::collide()
{
    Eigen::ArrayXXd Feq(Ny, Nx);
    for (int l = 0; l < NL; ++l)
    {
        //Feq.setZero();
        int cx = cxs[l];
        int cy = cys[l];
        double w = weights.coeffRef(l);
        Feq = rho * w * (1 + 3 * (cx * ux + cy * uy) + 9 * (cx * ux + cy * uy) * (cx * ux + cy * uy) / 2 - 3 * (ux * ux + uy * uy) / 2);
        F[l] += -(1.0 / tau) * (F[l] - Feq);
    }
}

/**
 * @brief compute vorticity for display
 */

void LBM::computeVorticity()
{
    for (int i = 0; i < Ny; ++i)
        for (int j = 0; j < Nx; ++j)
        {
            int i1 = i - 1;
            if (i1 < 0)
                i1 += Ny;
            int j1 = j - 1;
            if (j1 < 0)
                j1 += Nx;
            vorticity.coeffRef(i, j) = ux.coeffRef(i1, j) - ux.coeffRef(i, j) - (uy.coeffRef(i, j1) - uy.coeffRef(i, j));
        }

}

/**
 * @brief compute velocity magnitude
 */

void LBM::computeUmag()
{
    umag = (ux*ux + uy*uy).sqrt();
}