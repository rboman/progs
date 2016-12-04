//
// $Id$
//

#include "Global.h"
#include "Line.h"
#include <iostream>

Line::Line(int p1, int p2) : Curve(2)
{
    setPointNumber(0, p1);
    setPointNumber(1, p2);
}

void
Line::print() const
{
    std::cout << "Line " << getPointNumber(0) << ' ' << getPointNumber(1) ;
}

char *
Line::name() const 
{ 
    return "Line"; 
}

char *
Line::carteBacon() const
{ 
    return ".dro"; 
}

int 
Line::typeDon() const
{ 
    return 1; 
}


