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

#ifndef GMPOLARPOINT_H
#define GMPOLARPOINT_H

#include "genmai.h"
#include "gmPoint.h"
#include <iostream>

namespace genmai {

/**
 * @brief 2D Polar Point defined by a centre, an angle and a radius. 
 */

class GENMAI_API PolarPoint
{
public:
    Point c;
    double a;
    double r;

public:
    PolarPoint(Point const &c, double a, double r);
    PolarPoint(Point const &centre, const Point &axis, const Point &poi);

    friend std::ostream &operator<<(std::ostream &o, const PolarPoint &v);
};

}

#endif //GMPOLARPOINT_H
