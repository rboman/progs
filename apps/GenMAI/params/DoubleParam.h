//
// $Id$
//

#ifndef DOUBLEPARAM_H
#define DOUBLEPARAM_H

#include "Param.h"

/**
 * @brief Parameter with a "double" value
 */

class DoubleParam : public Param
{
    double defvalue;   ///< default value
    double value;      ///< current value
public:
    DoubleParam(ParamEnum id, const std::string &name="noname", double defvalue=0);

    virtual void setToDefault();

    virtual double getDouble() const;
    virtual void set(double value);

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;
};

#endif
