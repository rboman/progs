#ifndef CURVE_H
#define CURVE_H

#include <vector>

/**
 * @brief Mother class of Curve objects
 */

class Curve
{
    std::vector<int> pt;
public:

    Curve(int nbpt);

    int  numberOfPoints() const;
    int  getPointNumber(int i) const;
    void setPointNumber(int i, int j);

    virtual void  print() const = 0;
    virtual char *name() const = 0;
    virtual char *carteBacon() const = 0;
    virtual int   typeDon() const = 0;
};

#include "curve.inl"

#endif
