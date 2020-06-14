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

#include "gmCurve.h"
#include <string>

using namespace genmai;

Curve::Curve(std::vector<size_t> const &_pts) : Object(), pts(_pts) {}

std::string
Curve::name() const
{
    switch (pts.size())
    {
    case 2:
        return "Line";
    case 3:
        return "Arc";
    default:
        return "Curve";
    }
}

void
Curve::write(std::ostream &out) const
{
    out << name();
    for (auto no : pts)
        out << ' ' << no;
}