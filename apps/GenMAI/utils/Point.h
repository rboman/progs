//
// $Id$
//

#ifndef POINT_H
#define POINT_H

#include <iostream>
#include <math.h>

#include "PtNumber.h"
class PolarPoint;

/**
 * @brief 2D Cartesian Point with a number and some basic vector operations 
 *
 * @todo constructeur à partir d'un PolarPoint
 */

class Point
{
    PtNumber no;
    double x; 
    double y;
public:
    Point(double _x=0, double _y=0);
    Point(const PolarPoint &ppoi);

    PtNumber getNo() const;
    void     setNo(PtNumber _no);

    void   setX(double _x);
    double getX() const;

    void   setY(double _y);
    double getY() const;
   
    Point(const Point &centre, const Point &axis, double angle, double ray);
    Point(const Point &p1, const Point &p2, double t);

	friend std::ostream & operator<<(std::ostream &o, const Point &v);
    friend double atan2(const Point &pt);
    Point operator - (const Point &pt) const;
    Point rotate(double angle) const;
    double length() const;
    friend Point cosin(double angle);
    friend Point operator*(double alpha, const Point &pt);
    Point operator+(const Point &pt) const;
    double operator*(const Point &pt) const;
    double operator^(const Point &pt) const;
    bool operator==(const Point &pt) const;
};

#include "Point.inl"

#endif
