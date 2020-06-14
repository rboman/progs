// Tests some "new" c++11 features
// mingw 5.3.0: g++ -std=c++11 main.cpp   [OK]

#include "cpp11.h"

int
main()
{
    range_based_for();
    misc();

    return 0;
}
