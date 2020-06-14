#include <iostream>
#include <complex>
#include <vector>

void
misc()
{
    double d1 = 2.3;
    double d2{2.3}; // ok icc / pas ok vs11
    double d3 = {2.3};
    std::cout << "d1 = " << d1 << '\n';
    std::cout << "d2 = " << d2 << '\n';

    std::complex<double> z1{d1, d2};
    std::cout << "z1 = " << z1 << '\n';

    std::vector<int> v1{1, 2, 2, 3, 4, 5}; // pas ok icc
}
