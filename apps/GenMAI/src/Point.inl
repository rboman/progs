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

inline double
atan2(const Point &pt)
{
    return atan2(pt.y, pt.x);
}

inline Point
Point::operator-(const Point &pt) const
{
    return Point(x - pt.x, y - pt.y);
}

inline Point
cosin(double angle)
{
    return Point(cos(angle), sin(angle));
}

inline Point
Point::rotate(double angle) const
{
    const Point cs = cosin(angle);
    return Point(x * cs.x - y * cs.y,
                 x * cs.y + y * cs.x);
}

inline double
Point::length() const
{
    return sqrt(x * x + y * y);
}

inline Point
operator*(double alpha, const Point &pt)
{
    return Point(alpha * pt.x, alpha * pt.y);
}

inline Point
Point::operator+(const Point &pt) const
{
    return Point(x + pt.x, y + pt.y);
}

inline double
Point::operator*(const Point &pt) const
{
    return (x * pt.x + y * pt.y);
}

inline double
Point::operator^(const Point &pt) const
{
    return (x * pt.y - y * pt.x);
}
