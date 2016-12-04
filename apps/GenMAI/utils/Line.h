//
// $Id$
//

#ifndef LINE_H
#define LINE_H

#include "curve.h"

/**
 * @brief Defines a segment of Line with 2 points.
 */

class Line : public Curve {
public:
    Line(int p1, int p2);

    void print() const;
    char * name() const;
    char * carteBacon() const;
    int typeDon() const;
};

#endif
