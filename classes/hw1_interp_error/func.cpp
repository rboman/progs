#include "func.h"
#include "picomath.hpp"

// we use https://github.com/Nitrillo/picomath to parse a string given as
// argument in the onelab database

double func(std::string const &fct, double x, double y, double z)
{
    picomath::PicoMath pm;
    auto &_x = pm.addVariable("x");
    _x = x;
    auto &_y = pm.addVariable("y");
    _y = y;
    auto result = pm.evalExpression(fct.c_str());
    double r = 0.0;
    if (result.isOk())
        r = result.getResult();
    return r;
}
