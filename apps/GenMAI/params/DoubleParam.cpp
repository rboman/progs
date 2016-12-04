//
// $Id$
//

#include "Global.h"
#include "DoubleParam.h"

DoubleParam::DoubleParam(ParamEnum id, const std::string &name, double defvalue) : Param(id, name)
{
    this->defvalue = defvalue;
    setToDefault();
}

void 
DoubleParam::setToDefault()
{
    value = defvalue;
}

double 
DoubleParam::getDouble() const
{
    return value;
}

void 
DoubleParam::set(double value)
{
    this->value = value;
}

Param * 
DoubleParam::clone() const
{
    return new DoubleParam(*this);
}

void 
DoubleParam::print() const
{
    Param::print();
    std::cout << " " << value;
}

void 
DoubleParam::load(FILE *file)
{
    Param::load(file);
    set(loadDouble(file, true));
}

void 
DoubleParam::save(FILE *file) const
{ 
    Param::save(file);
    saveDouble(file, getDouble(), true);
}
