#include <iostream>
#include <sstream>
#include <omp.h>
#include <stdlib.h>

void
mxv(int m, int n, double *a, double *b, double *c, int nbt, int tmax)
{
#pragma omp parallel for num_threads(nbt)
    for (int i = 0; i < m; i++)
    {
        for (int t = 0; t < tmax; ++t)
        {
            a[i] = 0.0;
            for (int j = 0; j < n; j++)
                a[i] += b[i * n + j] * c[j];
        }
    }
}

void
usage(char *argv[])
{
    std::cout << "usage:  " << argv[0] << " m tmax1 tmax2\n";
    exit(1);
}

void
getarg(int argc, char *argv[], int nn, int *val)
{
    if (nn >= argc)
        return;

    int num = 0;
    std::istringstream str(argv[nn]);
    if (str >> num)
        *val = num;
    else
        usage(argv);
}

int
main(int argc, char *argv[])
{
#ifdef _OPENMP
    (void)omp_set_dynamic(false);
    if (omp_get_dynamic())
    {
        printf("Warning: dynamic adjustment of threads has been set\n");
    }
    (void)omp_set_num_threads(8);
#endif

    int m = 12800;
    int n = 12800;
    int tmax1 = 1;
    int tmax2 = 100;
    int nbth = 8;

    // m,n, tmax1, tmax2
    getarg(argc, argv, 1, &n);
    getarg(argc, argv, 1, &m);
    getarg(argc, argv, 2, &tmax1);
    getarg(argc, argv, 3, &tmax2);

    /*
      for(int nn=100; nn<=20000; nn*=2)
      {
          n=nn;
          m=nn;
  */
    // a(m) = b(m,n)*c(n)
    double *a, *b, *c, *cpu;
    try
    {
        a = new double[m];
        b = new double[m * n];
        c = new double[n];
        cpu = new double[nbth];
    }
    catch (...)
    {
        std::cerr << "not enough memory (" << (m * n * 8.) / 1024 / 1024
                  << "Mo)!\n";
        exit(1);
    }

    // std::cout << "matrix size:" << (m*n*8.)/1024/1024 << "Mo:\n";
    for (int i = 0; i < m; ++i)
        a[i] = 0.0;
    for (int i = 0; i < m; ++i)
        for (int j = 0; j < m; ++j)
            b[m * i + j] = i;
    for (int i = 0; i < n; ++i)
        c[i] = 2.0;

    // loop on thread nb
    for (int nbt = 1; nbt <= nbth; ++nbt)
    {
        double mbsiz = (m * n * 8.) / 1024 / 1024;
        std::cout << m << '\t' << mbsiz << '\t' << nbt << '\t';
        double tstart = omp_get_wtime();

        for (int t = 0; t < tmax1; ++t)
            mxv(m, n, a, b, c, nbt, tmax2);

        double tstop = omp_get_wtime();
        cpu[nbt - 1] = tstop - tstart;
        double mflops = double(m) * double(n) * double(tmax1) * double(tmax2) /
                        cpu[nbt - 1] / 1e6;

        std::cout << cpu[nbt - 1] << '\t' << cpu[0] / cpu[nbt - 1] << '\t'
                  << cpu[0] / cpu[nbt - 1] / nbt << '\t' << mflops << '\n';
    }

    delete[] a;
    delete[] b;
    delete[] c;
    delete[] cpu;

    //} // loop on nn

    return 0;
}
