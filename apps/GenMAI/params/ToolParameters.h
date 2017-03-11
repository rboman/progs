#ifndef T_MPARAM_H
#define T_MPARAM_H

#include "Parameters.h"
#include <vector>
#include "Point.h"

/**
 * @brief Defines the Parameters for the ToolBuilder object, accessors and I/O routines
 */

class ToolParameters : public Parameters
{
public:
    ToolParameters();

    ToolParameters(const ToolParameters &obj);
    void operator=(const ToolParameters &obj);
    virtual ~ToolParameters();

    void setToDefault();

    // Get/Set

    double getRadius() const;
    void   setRadius(double arg);

    double getInitialAngle() const;
    void   setInitialAngle(double arg);

    double getAsperityLength() const;
    void   setAsperityLength(double arg);

    double getAsperityAngle() const;
    void   setAsperityAngle(double arg);

    double getSmoothnessAngle() const;
    void   setSmoothnessAngle(double arg);

    double getAsperityInterval() const;
    void   setAsperityInterval(double arg);

    int  getNumberOfAsperities() const;
    void setNumberOfAsperities(int arg);

    Point const & getCentre() const;
    void setCentre(const Point &arg);
    void setCentreX(double arg);
    void setCentreY(double arg);
};

#include "ToolParameters.inl"

#endif
