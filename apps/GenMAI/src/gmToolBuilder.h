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

#ifndef GMTOOLBUILDER_H
#define GMTOOLBUILDER_H

#include "genmai.h"
#include "gmObject.h"
#include "gmPoint.h"
#include <vector>

namespace genmai
{

/**
 * @brief Fills a Tool with the description of a skin pass roll with asperities.
 *        The asperities can be smoothed.
 */

class GENMAI_API ToolBuilder : public Object
{
    Tool &target;

public:
    double radius;
    double initialAngle;
    double asperityLength;
    double asperityAngle;
    double smoothnessAngle;
    double asperityInterval;
    int numberOfAsperities;
    Point centre;

public:
    ToolBuilder(Tool &_target);
    void genere();

private:
    void genereAsperity();
    void genereInterval();
    void genereSmoothMatrix(size_t np0, size_t *np1, size_t i);

    Point const &getRollAxis() const;

    static double d2r(double angle);
    static double r2d(double angle);

    virtual void write(std::ostream &out) const override;
};

} // namespace genmai

#endif // GMTOOLBUILDER_H
