
#include <iostream>
#include <gmm.h>

#include "Bar.h"
#include "Mesh.h"


int main()
{
    std::cout << "starting simulation...\n";
    Bar bar;
    std::cout << bar;
    Mesh mesh;
    mesh.generate(0.0, bar.L/2.0, 100);
    std::cout << mesh << '\n';

    gmm::row_matrix<gmm::wsvector<double> > A(2, 2);

	// pause
	system("pause");
}

