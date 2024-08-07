//   Copyright 2003-2019 Romain Boman
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

#ifndef GMPOINT_H
#define GMPOINT_H

#include "genmai.h"
#include <iostream>

namespace genmai
{

/**
 * @brief 2D Cartesian Point with a number and some basic vector operations
 */

class GENMAI_API Point
{
public:
    int no;
    double x;
    double y;

public:
    explicit Point(double _x = 0.0, double _y = 0.0);
    explicit Point(const PolarPoint &ppoi);

    Point(const Point &centre, const Point &axis, double angle, double ray);
    Point(const Point &p1, const Point &p2, double t);

    Point operator-(const Point &pt) const;
    Point rotate(double angle) const;
    double length() const;
    Point operator+(const Point &pt) const;
    double operator*(const Point &pt) const;
    double operator^(const Point &pt) const;
    bool operator==(const Point &pt) const;

    friend GENMAI_API std::ostream &operator<<(std::ostream &o, const Point &v);
    friend double atan2(const Point &pt);
    friend Point cosin(double angle);
    friend Point operator*(double alpha, const Point &pt);
};

#include "gmPoint.inl"

} // namespace genmai

#endif // GMPOINT_H
