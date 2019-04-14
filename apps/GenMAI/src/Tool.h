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

#ifndef TOOL_H
#define TOOL_H

#include "genmai.h"
#include <vector>
#include "Point.h"
#include "Curve.h"
#include "TargetObject.h"

/**
 * @brief Defines a Tool (list of Point and list of Curve). 
 */

class GENMAI_API Tool : public TargetObject
{
    int firstp;
    int firstc;

    std::vector<Point> point;
    std::vector<Curve *> courbe;

  public:
    Tool();

    void addCurve(Curve *m);
    void addPoint(const Point &pt);
    void addPoint(double x, double y);

    size_t numberOfPoints() const;
    size_t numberOfCurves() const;

    virtual void print() const;
    virtual void list() const;
    virtual bool isEmpty() const;
    virtual void clear();

    void setFirstCurve(int firstCurve);
    int getFirstCurve() const;
    void setFirstPoint(int firstCurve);
    int getFirstPoint() const;

    double getPointX(int i) const;
    double getPointY(int i) const;
    Point const &getPoint(int i) const;

    Curve const &getCurve(int i) const;
};

#include "Tool.inl"

#endif
