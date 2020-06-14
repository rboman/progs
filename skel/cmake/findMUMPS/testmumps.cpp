#include "gmm_gmm.h"
#include <gmm/gmm_MUMPS_interface.h>

void
testMUMPS()
{
    double A[5][5] = {{1, 3, 0, 9, 0},
                      {1, 2, 0, 9, 0},
                      {1, 2, 3, 0, 0},
                      {3, 2, 11, 4, 0},
                      {0, 2, 0, 0, 7}};

    int n = 5;

    gmm::row_matrix<gmm::wsvector<double>> K(n, n);

    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            // if(A[i][j]!=0.0 || A[j][i] != 0.0)
            K(i, j) = A[i][j];

    gmm::csr_matrix<double> Kcsr;
    gmm::copy(K, Kcsr);

    std::vector<double> b(n), x(n);
    for (int i = 0; i < n; ++i)
        b[i] = 1.0;

    // version "dense"
    gmm::dense_matrix<double> Kdense(n, n);
    gmm::copy(K, Kdense);

    // solver LU
    // gmm::lu_solve(Kdense,x,b);

    // solver MUMPS
    gmm::MUMPS_solve(Kcsr, x, b);

    // resultat
    std::cout << "Kcsr=" << Kcsr;
    std::cout << "b=" << b << '\n';
    std::cout << "x=" << x << '\n';

    // check
    std::vector<double> b2(n);
    gmm::mult(Kcsr, x, b2);
    std::cout << "b2=" << b2 << '\n';
}

int
main(int argc, char *argv[])
{
    // MPI_Init(&argc,&argv);
    try
    {
        testMUMPS();
    }
    GMM_STANDARD_CATCH_ERROR;

    // MPI_Finalize();
    return 0;
}
