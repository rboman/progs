#include "Global.h"
#include "PointParam.h"


PointParam::PointParam(ParamEnum id, const std::string &name, double x, double y) : Param(id, name), 
                        defvalue(x,y)
{
    setToDefault();
}

void 
PointParam::setToDefault()
{
    value = defvalue;
}

Point const &
PointParam::getPoint() const
{
    return value;
}

Point &
PointParam::getPoint()
{
    return value;
}

void 
PointParam::set(Point value)
{
    this->value = value;
}

Param * 
PointParam::clone() const
{
    return new PointParam(*this);
}

void 
PointParam::print() const
{
    Param::print();
    std::cout << " " << value;
}

void 
PointParam::load(FILE *file)
{
    Param::load(file);
    getPoint().setX(loadDouble(file, true)); 
    getPoint().setY(loadDouble(file, true)); 
}

void 
PointParam::save(FILE *file) const
{ 
    Param::save(file);
    saveDouble(file, getPoint().getX(), true);
    saveDouble(file, getPoint().getY(), true);
}

