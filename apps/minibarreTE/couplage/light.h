// $Id$

#ifndef LIGHT_H
#define LIGHT_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Light
{
public:
	double Q;
	double f;
public:
    Light();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out, Light const &obj);
	void save(std::string const &filename) const;
	double eval(double t);
};

#endif
