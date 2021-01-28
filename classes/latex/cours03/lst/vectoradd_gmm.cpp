#include <gmm.h> 
void main() 
{
    std::vector<double> a(3), b(3), c(3);  // dimension=3
    for(int i=0; i<3; ++i) 
        {a[i]=i+1; b[i]=2*(i+1); } // initialisation

    gmm::add(a, b, c);               // c = a+b
    std::cout << "a=" << a << '\n';  // affiche "a"
    std::cout << "b=" << b << "\nc=" << c << '\n';
}