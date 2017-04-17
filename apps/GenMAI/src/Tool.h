#ifndef TOOL_H
#define TOOL_H

#include <vector>
#include "Point.h"
#include "Curve.h"
#include "TargetObject.h"

/**
 * @brief Defines a Tool (list of Point and list of Curve). 
 */

class Tool : public TargetObject
{
    int firstp;
    int firstc;

    std::vector<Point>   point;
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
    int  getFirstCurve() const;
    void setFirstPoint(int firstCurve);
    int  getFirstPoint() const;

    double getPointX(int i) const;
    double getPointY(int i) const;
    Point const &getPoint(int i) const;

    Curve const &getCurve(int i) const;

};

#include "Tool.inl"

#endif
