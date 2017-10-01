
#include "dcm.h"
#include "Polynome.h"
#include "Plane.h"
using namespace dcm;

void main()
{
    Polynome::demo();
    std::cout << "\n\n---\n\n";
    Plane Plane;
    Plane.calcule();
}

