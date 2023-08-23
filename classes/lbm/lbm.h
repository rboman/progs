#ifndef LBM_H
#define LBM_H

#include <vector>
#include <Eigen/Dense>

struct Dir
{
    enum
    {

        O = 0,
        N = 1,
        NE = 2,
        E = 3,
        SE = 4,
        S = 5,
        SW = 6,
        W = 7,
        NW = 8
    };
};

    //      8     1   2       1/36   1/9   1/36
    //       +   +   +
    //        \  |  /
    //         \ |0/
    //     7 +---o---+ 3      1/9    4/9    1/9
    //         / | \ 
    //        /  |  \ 
    //       +   +   +
    //      6    5    4       1/36   1/9   1/36


class LBM
{

public:
    // Simulation parameters
    int Nx;            //< resolution x-dir
    int Ny;            //< resolution y-dir
    double rho0; //< average density
    double tau;  //< collision timescale
    //int Nt = 4000;     //< number of timesteps
    const int NL = 9;

    std::vector<int> cxs;
    std::vector<int> cys;

    Eigen::ArrayXd weights;  // could be a std::vector

    Eigen::ArrayXd X;               ///< x coordinates of the lattice
    Eigen::ArrayXd Y;               ///< y coordinates of the lattice
    std::vector<Eigen::ArrayXXd> F; ///<
    std::vector<Eigen::ArrayXXd> bndryF;
    Eigen::ArrayXXd rho;
    Eigen::ArrayXXd ux;
    Eigen::ArrayXXd uy;

    Eigen::ArrayXXi cylinder;       ///< array for the obstacles (1 = inside)
    Eigen::ArrayXXd vorticity;      ///< only used for the display
    Eigen::ArrayXXd umag;           ///< magnitude of the velocity (display)

    std::vector<int> bcMap;

    LBM(int _Nx = 400, int _Ny = 100);
    void init();
    void update();

protected:
    void initState();
    void initCylinder();
    void storeBndryF();
    void collide();
    void applyBndryF();
    void computeU();
    void computeRho();
    void computeVorticity();
    void computeUmag();
    void drift();

    void applyOutletBC();
};

#endif //LBM_H
