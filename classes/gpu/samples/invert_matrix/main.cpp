// compute the inverse of a random matrix of size "dim"
//
// example:
//    ./invert_matrix 10

#include "linalg.h"
#include <string>

int main(int argc, char *argv[])
{
    // check program arguments and set the dimension "dim"
    const int dimmax = 100;
    if (argc != 2)
    {
        std::cout << "\nusage: " << argv[0] << " <dimension>\n\n";
        exit(1);
    }
    int dim = std::stoi(argv[1]);
    if (dim < 1 || dim > dimmax)
    {
        std::cerr << "\nerror: dimension should be in [1," << dimmax << "]!\n\n";
        exit(1);
    }

    // create a random matrix => A
    std::vector<double> A = randomMatrix(dim);
    std::cout << "A=" << A << "\n\n";

    // compute the inverse => invA
    std::vector<double> invA = inverse(A, dim);
    std::cout << "invA=" << invA << "\n\n";

    // check that A x invA = I
    std::vector<double> C = matmult(A, invA, dim);
    std::cout << "A*invA=" << C << "\n\n";

    return 0;
}
