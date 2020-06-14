// Teste les fct statiques dans les templates
// RoBo - juin 2002

#include "stat.h"

int
main()
{
    std::cout << "objet Stat<int> :\n";
    Stat<int> RB(2);
    RB.print();

    std::cout << "objet StatI<int,1> :\n";
    StatI<int, 1> RBI1(1);
    RBI1.print();
    std::cout << "objet StatI<int,2> :\n";
    StatI<int, 2> RBI2(2);
    RBI2.print();

    return 0;
}
