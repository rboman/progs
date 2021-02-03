#include <gmm.h> 
void main() 
{
    gmm::dense_matrix<double> A(2,2);  // dimension=2x2
    std::vector<double> x(2), b(2);    // dimension=3
    A(0,0) = 1.0; A(0,1) = 2.0;   x[0]=2.0; 
    A(1,0) = 3.0; A(1,1) = 4.0;   x[1]=1.0;

    gmm::mult(A, gmm::col_vector(x), 
                 gmm::col_vector(b));     // b = A x
    std::cout << "A=" << A;  // affiche "A"
    std::cout << "x=" << x << "\nb=" << b << '\n';
}