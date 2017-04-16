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
    void genereSmoothMatrix(int np0, int *np1, int i);

    void addPoint(const Point &arg);
    void addCurve(Curve *arg);

    Point const & getRollAxis() const;

    static double d2r(double angle);
    static double r2d(double angle);
};

#include "ToolBuilder.inl"

#endif
