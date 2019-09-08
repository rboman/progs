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

#include "PolarPoint.h"

PolarPoint::PolarPoint(Point const &c,
                       double a,
                       double r) : c(c), a(a), r(r)
{
}

PolarPoint::PolarPoint(const Point &centre,
                       const Point &axis,
                       const Point &poi)
{
    Point dx = (poi - centre).rotate(-atan2(axis));

    c = centre;
    r = dx.length();
    a = atan2(dx);
}

std::ostream &
operator<<(std::ostream &o, const PolarPoint &v)
{
    o << v.c << ' ';
    o << '(' << v.a << ',' << v.r << ')';
    return o;
}
