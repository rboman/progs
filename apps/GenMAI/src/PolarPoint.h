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
