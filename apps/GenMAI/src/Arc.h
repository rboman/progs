#ifndef ARC_H
#define ARC_H

#include "Curve.h"

/**
 * @brief Defines an Arc of circle with 3 points.
 */

class Arc : public Curve
{
public:
    Arc(int p1, int p2, int p3);

    void print() const;
    char *name() const;
    char *carteBacon() const;
    int typeDon() const;
};

#endif
