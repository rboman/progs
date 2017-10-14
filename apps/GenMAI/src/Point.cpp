//   Copyright 2003-2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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
