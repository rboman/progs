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

#include "gmTool.h"
#include "gmPoint.h"
#include "gmCurve.h"

using namespace genmai;

Tool::Tool() : Object(), points(0), curves(0) { clear(); }

void
Tool::write(std::ostream &out) const
{
    out << "MATRICE:" << std::endl;
    out << "--------" << std::endl;
    out << " points     : " << points.size() << std::endl;
    out << " courbes    : " << curves.size() << std::endl;
    out << std::endl;
}

void
Tool::list() const
{
    for (auto i = 0; i < points.size(); ++i)
        std::cout << points[i] << std::endl;

    for (auto i = 0; i < curves.size(); ++i)
        std::cout << curves[i] << std::endl;
}

bool
Tool::isEmpty() const
{
    if (!points.size())
        return true;
    else
        return false;
}

void
Tool::clear()
{
    firstp = 0;
    firstc = 0;

    points.resize(0);

    for (auto i = 0; i < curves.size(); ++i)
        delete curves[i];
    curves.resize(0);
}
