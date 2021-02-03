// solves a Laplacian over a cube with mumps

#include "laplace.h"
#include "vtlSPoints.h"

/**
 * @brief fills a basic distretised-Laplace system of equations.
 *        Particular Dirichlet and Neumann BCs are implemented.
 */

void fill_system(SPoints &grid,
                 std::vector<MUMPS_INT> &irn,
                 std::vector<MUMPS_INT> &jcn,
                 std::vector<double> &A,
                 std::vector<double> &rhs)
{
    // build matrix & rhs

    int npz1 = grid.np1[2];
    int npz2 = grid.np2[2];
    int npy1 = grid.np1[1];
    int npy2 = grid.np2[1];
    int npx1 = grid.np1[0];
    int npx2 = grid.np2[0];
    Vec3i np = grid.np();

    auto loc = [=](int i, int j, int k) { return (k - npz1) * (np[1] * np[0]) + (j - npy1) * np[0] + (i - npx1) + 1; }; // +1 (fortran)

    double dx2 = grid.dx[0] * grid.dx[0];
    double dy2 = grid.dx[1] * grid.dx[1];
    double dz2 = grid.dx[2] * grid.dx[2];

    for (int k = npz1; k <= npz2; ++k)
    {
        double z = k * grid.dx[2] + grid.o[2];
        for (int j = npy1; j <= npy2; ++j)
        {
            double y = j * grid.dx[1] + grid.o[1];
            for (int i = npx1; i <= npx2; ++i)
            {
                double x = i * grid.dx[0] + grid.o[0];

                int id = loc(i, j, k);

                // BCs ===========================================

                if (k == npz1) // impose dirichlet first
                {
                    // dirichlet
                    irn.push_back(id);
                    jcn.push_back(id);
                    A.push_back(1.0);
                    //irn.push_back(id);
                    //jcn.push_back(loc(i, j, k + 1));
                    //A.push_back(-1.0);
                }
                else if (k == npz2)
                {
                    // dirichlet
                    irn.push_back(id);
                    jcn.push_back(id);
                    A.push_back(1.0);
                    //irn.push_back(id);
                    //jcn.push_back(loc(i, j, k - 1));
                    //A.push_back(-1.0);
                }
                else if (j == npy1)
                {
                    // neumann
                    irn.push_back(id);
                    jcn.push_back(id);
                    A.push_back(1.0);
                    irn.push_back(id);
                    jcn.push_back(loc(i, j + 1, k));
                    A.push_back(-1.0);
                }
                else if (j == npy2)
                {
                    // neumann
                    irn.push_back(id);
                    jcn.push_back(id);
                    A.push_back(1.0);
                    irn.push_back(id);
                    jcn.push_back(loc(i, j - 1, k));
                    A.push_back(-1.0);
                }
                else if (i == npx1)
                {
                    // neumann
                    irn.push_back(id);
                    jcn.push_back(id);
                    A.push_back(1.0);
                    irn.push_back(id);
                    jcn.push_back(loc(i + 1, j, k));
                    A.push_back(-1.0);
                }
                else if (i == npx2)
                {
                    // neumann
                    irn.push_back(id);
                    jcn.push_back(loc(i - 1, j, k));
                    A.push_back(1.0);
                    irn.push_back(id);
                    jcn.push_back(loc(i + 1, j, k));
                    A.push_back(-1.0);
                }
                else
                {

                    if ((i != npx1) && (i != npx2) && (j != npy1) && (j != npy2) && (k != npz1) && (k != npz2))
                    {
                        irn.push_back(id);
                        jcn.push_back(id);
                        A.push_back(-2.0 * (1.0 / dx2 + 1.0 / dy2 + 1.0 / dz2));
                    }
                    // x
                    if (i != npx1)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i - 1, j, k));
                        A.push_back(1.0 / dx2);
                    }
                    if (i != npx2)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i + 1, j, k));
                        A.push_back(1.0 / dx2);
                    }
                    // y
                    if (j != npy1)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i, j - 1, k));
                        A.push_back(1.0 / dy2);
                    }
                    if (j != npy2)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i, j + 1, k));
                        A.push_back(1.0 / dy2);
                    }

                    // y
                    if (k != npz1)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i, j, k - 1));
                        A.push_back(1.0 / dz2);
                    }
                    if (k != npz2)
                    {
                        irn.push_back(id);
                        jcn.push_back(loc(i, j, k + 1));
                        A.push_back(1.0 / dz2);
                    }
                }

                // rhs
                if (k == npz1)
                {
                    rhs.push_back(1.0);
                }
                else if (k == npz2)
                {
                    rhs.push_back(2.0);
                }
                else
                {
                    rhs.push_back(0.0);
                }
                //std::cout << "rhs.size()=" << rhs.size() << '\n';
            }
        }
    }
}

/**
 * @brief fills a basic distretised-Laplace system of equations.
 *        Particular Dirichlet and Neumann BCs are implemented.
 */

void save_matrix(std::string const &fname,
                 std::vector<MUMPS_INT> &irn,
                 std::vector<MUMPS_INT> &jcn,
                 std::vector<double> &A)
{
    std::cout << "saving matrix to file \"" << fname << ".m\"..." << std::endl;
    std::ofstream f(fname+".m");
    for (size_t i = 0; i < A.size(); ++i)
        f << fname << "(" << irn[i] << "," << jcn[i] << ")=" << A[i] << ";\n";
    f.close();
}

void save_vector(std::string const &fname,
                 std::vector<double> &rhs)
{
    std::cout << "saving vector to file \"" << fname << ".m\"..." << std::endl;
    std::ofstream f(fname+".m");
    for (size_t i = 0; i < rhs.size(); ++i)
        f << fname << "(" << i + 1 << ")=" << rhs[i] << ";\n";
    f.close();
}

//f << "[A\\rhs' sol']\n";
//f << "error = norm(A\\rhs'-sol')\n";
