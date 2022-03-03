// from https://gist.github.com/jgreitemann/99b95fff2393cd9e2478

#include <stdio.h>
#include <cblas.h>
#include <lapacke.h>

int main(int argc, char *argv[])
{
    const double A[3 * 3] = {1, 2, 3,
                             4, 5, 6,
                             7, 8, 9};
    double B[3 * 3] = {0.5, 0.0, -0.5,
                       0.0, 1.0, 0.0,
                       -.5, 0.0, 0.5};
    double C[3 * 3] = {0, 0, 0,
                       0, 0, 0,
                       0, 0, 0};
    cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, 3, 3, 3, 1.0,
                A, 3, B, 3, 0.0, C, 3);
    int i, j;
    for (i = 0; i < 3; i++)
    {
        for (j = 0; j < 3; j++)
            printf("%3f\t", C[3 * i + j]);
        printf("\n");
    }

    double D[3];
    double E[2];
    double TAU[2];
    LAPACKE_dsytrd(LAPACK_ROW_MAJOR, 'U', 3, B, 3, D, E, TAU);
    printf("Tridiagonal:\n%3f\t%3f\t%3f\n%3f\t%3f\t%3f\n%3f\t%3f\t%3f\n",
           D[0], E[0], 0.,
           E[0], D[1], E[1],
           0., E[1], D[2]);
    LAPACKE_dsterf(3, D, E);
    printf("Eigenvalues: %3f, %3f, %3f\n", D[0], D[1], D[2]);
    return 0;
}
