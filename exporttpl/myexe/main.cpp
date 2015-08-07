
#include "ObjectA.h"
#include "ObjectB.h"

#include <stdio.h>

int main()
{
    ObjectA<int> a(1);
    a.print();

    ObjectB b(1);
    b.print();

    getchar();
}