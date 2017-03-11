#include "Global.h"
#include "IntegerParam.h"

IntegerParam::IntegerParam(ParamEnum id, const std::string &name, int defvalue) : Param(id, name)
{
    this->defvalue = defvalue;
    setToDefault();
}

void 
IntegerParam::setToDefault()
{
    value = defvalue;
}

int 
IntegerParam::getInt() const
{
    return value;
}

void 
IntegerParam::set(int value)
{
    this->value = value;
}

Param * 
IntegerParam::clone() const
{
    return new IntegerParam(*this);
}

void 
IntegerParam::print() const
{
    Param::print();
    std::cout << " " << value;
}

void 
IntegerParam::load(FILE *file)
{
    Param::load(file);
    set(loadInteger(file, true));
}

void 
IntegerParam::save(FILE *file) const
{ 
    Param::save(file);
    saveInteger(file, getInt(), true);
}
