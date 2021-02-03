#include <gmm.h>
#include <gmm_MUMPS_interface.h>

void testMUMPS()
{
    double A[5][5] = { { 1, 3, 0, 9, 0},
                       { 1, 2, 0, 9, 0},
                       { 1, 2, 3, 0, 0},
                       { 3, 2, 11, 4, 0},
                       { 0, 2, 0, 0, 7 }};
    int n=5;

    gmm::row_matrix< gmm::wsvector<double> > K(n,n);
    for(int i=0; i<n; ++i)
        for(int j=0; j<n; ++j)
                K(i, j) = A[i][j];

    gmm::csr_matrix<double> Kcsr;    
    gmm::copy(K, Kcsr);