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

#ifndef TOOLBUILDER_H
#define TOOLBUILDER_H

#include <vector>
#include "Point.h"
#include "Tool.h"
#include "ToolParameters.h"
#include "Builder.h"
class Curve;

/**
 * @brief Fills a Tool with the description of a skin pass roll with asperities.
 *        The asperities can be smoothed.
 */

class ToolBuilder : public Builder
{
    static double pi;
    Tool &target;
    ToolParameters par;

public:
    ToolBuilder(Tool &_target);

    void         setParameters(const ToolParameters &p);
    virtual void printParameters() const;
    virtual void genere();

private:
    void genereAsperity();//, double base, double angle, double rayr);
    void genereInterval();
    void genereSmoothMatrix(size_t np0, size_t *np1, size_t i);

    void addPoint(const Point &arg);
    void addCurve(Curve *arg);

    Point const & getRollAxis() const;

    static double d2r(double angle);
    static double r2d(double angle);
};

#include "ToolBuilder.inl"

#endif
