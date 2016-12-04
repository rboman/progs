//
// $Id$
//

#include "Global.h"
#include "Arc.h"
#include <iostream>

Arc::Arc(int p1, int p2, int p3) : Curve(3)
{
    setPointNumber(0, p1);
    setPointNumber(1, p2);
    setPointNumber(2, p3);
}

void
Arc::print() const
{
    std::cout << "Arc " << getPointNumber(0) << ' ' << getPointNumber(1) << ' ' << getPointNumber(2) ;
}

char *
Arc::name() const 
{ 
    return "Arc"; 
}

char *
Arc::carteBacon() const
{ 
    return ".arc"; 
}

int 
Arc::typeDon() const 
{ 
    return 2; 
}

