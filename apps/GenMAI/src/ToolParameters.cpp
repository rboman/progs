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

#include "ToolParameters.h"
#include "DoubleParam.h"
#include "IntegerParam.h"
#include "PointParam.h"

/**
 * @brief Constructor
 */

ToolParameters::ToolParameters()
{
    setToDefault();
}

ToolParameters::ToolParameters(const ToolParameters &obj) : Parameters(obj)
{
}

void ToolParameters::operator=(const ToolParameters &obj)
{
    Parameters::operator=(obj);
}

ToolParameters::~ToolParameters()
{
}

/**
 * @brief Sets the parameters to the default values
 */

void ToolParameters::setToDefault()
{
    addParam(DoubleParam(P_R, "Radius", 100.0));
    addParam(DoubleParam(P_A1, "Initial Angle", 0.050));
    addParam(PointParam(P_CENTRE, "Centre", 10.0, 110.0));
    addParam(DoubleParam(P_ASP_B, "Asperity Length", 10.0));
    addParam(DoubleParam(P_ASP_A, "Asperity Angle", 40.0));
    addParam(DoubleParam(P_ASP_CR, "Smoothness Angle", 1.0));
    addParam(DoubleParam(P_ABS_INT, "Asperity Interval", 5.0));
    addParam(IntegerParam(P_ASP_NBR, "Number Of Asperities", 10));
}
