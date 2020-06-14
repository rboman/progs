#include <iostream>

void
range_based_for()
{
    int myArray[5] = {1, 2, 3, 4, 5};
    for (auto x : myArray)
        std::cout << x << "-";
    std::cout << '\n';
}
