//
// $Id$
//

#include "Global.h"
#include "PolarPoint.h"
#include <math.h>

PolarPoint::PolarPoint(const Point c, 
                   double a, 
                   double r) : c(c), a(a), r(r)
{
}

PolarPoint::PolarPoint(const Point &centre, 
                   const Point &axis, 
                   const Point &poi)
{
    Point dx = (poi-centre).rotate(-atan2(axis));

    setC( centre );
    setR( dx.length() );
    setA( atan2(dx) );
}

std::ostream & 
operator<<(std::ostream &o, const PolarPoint &v) 
{ 
    o << v.getC() << ' ' ;
    o << '(' << v.getA() << ',' << v.getR() << ')' ; 
    return o; 
}
