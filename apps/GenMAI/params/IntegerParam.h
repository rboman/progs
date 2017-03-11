#ifndef INTEGERPARAM_H
#define INTEGERPARAM_H

#include "Param.h"

/**
 * @brief Parameter with a "int" value
 */

class IntegerParam : public Param
{
    int defvalue;   ///< default value
    int value;      ///< current value
public:
    IntegerParam(ParamEnum id, const std::string &name="noname", int defvalue=0.0);

    virtual void setToDefault();

    virtual int getInt() const;
    virtual void set(int value);

    virtual Param * clone() const;
    virtual void print() const;
    virtual void load(FILE *file);
    virtual void save(FILE *file) const;

};

#endif
