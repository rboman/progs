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

#ifndef T_MPARAM_H
#define T_MPARAM_H

#include "genmai.h"
#include "Parameters.h"
#include <vector>
#include "Point.h"

/**
 * @brief Defines the Parameters for the ToolBuilder object, accessors and I/O routines
 */

class GENMAI_API ToolParameters : public Parameters
{
  public:
    ToolParameters();

    ToolParameters(const ToolParameters &obj);
    void operator=(const ToolParameters &obj);
    virtual ~ToolParameters();

    void setToDefault();

    // Get/Set

    double getRadius() const;
    void setRadius(double arg);

    double getInitialAngle() const;
    void setInitialAngle(double arg);

    double getAsperityLength() const;
    void setAsperityLength(double arg);

    double getAsperityAngle() const;
    void setAsperityAngle(double arg);

    double getSmoothnessAngle() const;
    void setSmoothnessAngle(double arg);

    double getAsperityInterval() const;
    void setAsperityInterval(double arg);

    int getNumberOfAsperities() const;
    void setNumberOfAsperities(int arg);

    Point const &getCentre() const;
    void setCentre(const Point &arg);
    void setCentreX(double arg);
    void setCentreY(double arg);
};

#include "ToolParameters.inl"

#endif
