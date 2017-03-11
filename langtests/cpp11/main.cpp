#include <iostream>
#include <complex>
#include <vector>

// Tests some "new" c++11 features
// mingw 5.3.0: g++ -std=c++11 main.cpp   [OK]

int main()
{
	double d1 = 2.3;
	double d2 {2.3}; // ok icc / pas ok vs11
	double d3 = {2.3};
	std::cout << "d1 = " << d1 << '\n';
	std::cout << "d2 = " << d2 << '\n';

	std::complex<double> z1{d1,d2};
	std::cout << "z1 = " << z1 << '\n';

	std::vector<int> v1{1,2,2,3,4,5}; // pas ok icc

	// le return 0 semble etre facultatif et ne gerere pas de warning s'il n'est pas la
}

