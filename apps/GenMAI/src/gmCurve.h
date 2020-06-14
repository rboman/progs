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

#ifndef GMCURVE_H
#define GMCURVE_H

#include "genmai.h"
#include "gmObject.h"
#include <cstddef> // size_t for travis
#include <vector>
class Point;

namespace genmai
{

/**
 * @brief a curve (a line if 2 points - an arc if 3 points)
 */

class GENMAI_API Curve : public Object
{
public:
    std::vector<size_t> pts;

    Curve(std::vector<size_t> const &_pts);
    virtual void write(std::ostream &out) const override;
    std::string name() const;
};

} // namespace genmai

#endif // GMCURVE_H
