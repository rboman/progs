
#include "ObjectA.h"
#include "ObjectB.h"

//#include <stdio.h>

//extern template class MYDLL_API ObjectA<int>;


int main()
{
    ObjectA<int> a(1);
    a.print();

    ObjectB b(1);
    b.print();

    //getchar();
}