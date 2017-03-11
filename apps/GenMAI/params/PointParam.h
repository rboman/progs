#ifndef POINTPARAM_H
#define POINTPARAM_H

#include "Param.h"

/**
 * @brief Parameter with a "Point" value (2 coords)
 */

class PointParam : public Param
{
    Point defvalue; ///< default value
    Point value;    ///< current value

public:
    PointParam(ParamEnum id, const std::string &name="noname", double x=0, double y=0);
 
    virtual void setToDefault();

    virtual Point const &getPoint() const;
    virtual Point & getPoint();
    virtual void set(Point value);

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;
};

#endif
