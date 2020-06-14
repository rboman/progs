#include "ExpMath.h"
#include <iostream>

int
main(int argc, char **argv)
{
    if (argc == 2)
    {
        ExpMath A(argv[1]);
        float val = A.eval();
        std::cout << "Valeur = " << val << '\n';
    }
    else
    {
        std::cout << "\nPetite calculette.\n";
        std::cout << "\nUsage : " << argv[0] << " 'expr.'\n\n";
    }
    return 0;
}
