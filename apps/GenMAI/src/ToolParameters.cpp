#include "Global.h"
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

void
ToolParameters::operator=(const ToolParameters &obj)
{
    Parameters::operator =(obj);
}

ToolParameters::~ToolParameters() 
{
}

/**
 * @brief Sets the parameters to the default values
 */

void
ToolParameters::setToDefault()
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


