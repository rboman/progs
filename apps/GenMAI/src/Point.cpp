#include "Global.h"
#include "Point.h"
#include "PolarPoint.h"
#include <math.h>

Point::Point(double x, 
                 double y) : no(0), x(x), y(y)
{
}

Point::Point(const Point &centre, 
             const Point &axis, 
             double angle, 
             double rayon) : no(0)
{
    Point pt(  centre + rayon * cosin( angle+atan2(axis)) );
    setX( pt.getX() );
    setY( pt.getY() );
}

Point::Point(const PolarPoint &ppoi)
{
    Point pt(  ppoi.getC() + ppoi.getR() * cosin( ppoi.getA() ) );
    setX( pt.getX() );
    setY( pt.getY() );
}

Point::Point(const Point &p1, 
                 const Point &p2, 
                 double t) : no(0)
{
    Point pt(  t*p1 + (1.0-t)*p2 );
    setX( pt.getX() );
    setY( pt.getY() );
}

std::ostream & 
operator<<(std::ostream &o, const Point &v)
{ 
    if(v.getNo().isValid()) 
        o << v.getNo() << ' ' ;
    o << '(' << v.getX() << ',' << v.getY() << ')' ; 
    return o; 
}

bool 
Point::operator==(const Point &pt) const
{
    return ( getX()==pt.getX() && getY()==pt.getY() );
}
