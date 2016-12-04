//
// $Id$
//

#ifndef POLARPOINT_H
#define POLARPOINT_H

#include <iostream>

#include "Point.h"

/**
 * @brief 2D Polar Point defined by a centre, an angle and a radius. 
 */

class PolarPoint
{
    Point  c;
    double a; 
    double r;

public:
    PolarPoint(const Point c, double a, double r);
    PolarPoint(const Point &centre, const Point &axis, const Point &poi);

    // Get / Set

    void   setC(const Point &_c);
    Point  getC() const;
    void   setA(double a);
    double getA() const;
    void   setR(double r);
    double getR() const;

	friend std::ostream & operator<<(std::ostream &o, const PolarPoint &v);
};

#include "PolarPoint.inl"

#endif
