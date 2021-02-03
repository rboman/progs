#include "linalg.h"
#include <cstring>

// this files requires Eigen.
// Linux:
//   sudo apt install libeigen3-dev
// Windows:
//   download it from http://eigen.tuxfamily.org/
//   unzip it in ./gmsh-api/eigen
//   be sure to load the environment
//   (the eigen folder should be added to INCLUDE)

#include <Eigen/Dense>

// print a vector<double> as "[v1, v2, v3, ...]"

std::ostream &operator<<(std::ostream &s, std::vector<double> const &v)
{
    s << '[';
    for (int i = 0; i < v.size(); ++i)
    {
        if (std::abs(v[i]) < 1e-10)
            s << "0";
        else
            s << v[i];
        if (i != v.size() - 1)
            s << ", ";
    }
    s << ']';
    return s;
}

// create and return a random matrix of dimension "dim"

std::vector<double> randomMatrix(int dim)
{
    // create a dim x dim mrandom matrix using Eigen
    Eigen::MatrixXd mat = Eigen::MatrixXd::Random(dim, dim);
    // copy its values into a std::vector
    std::vector<double> v(dim * dim);
    std::memcpy(&(v[0]), mat.data(), dim * dim * sizeof(double));
    return v;
}

// compute the inverse of a square matrix of size (dim,dim) given as a std::vector<double>
// return the inverse as a std::vector<double>

std::vector<double> inverse(std::vector<double> &M, int dim)
{
    Eigen::Map<Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> mapA(&(M[0]), dim, dim);
    Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> B = mapA.inverse();
    std::vector<double> invM(dim * dim);
    std::memcpy(&(invM[0]), B.data(), dim * dim * sizeof(double));
    return invM;
}

// multiply 2 matrices given as std::vector<double>
// return the result as a std::vector<double>

std::vector<double> matmult(std::vector<double> &A, std::vector<double> &B, int dim)
{
    assert(A.size() == dim * dim);
    assert(B.size() == dim * dim);

    std::vector<double> C(dim * dim);
    for (int i = 0; i < dim; ++i)
        for (int j = 0; j < dim; ++j)
        {
            double v = 0;
            for (int k = 0; k < dim; ++k)
                v += A[i * dim + k] * B[k * dim + j];
            C[i * dim + j] = v;
        }
    return C;
}
