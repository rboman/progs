// $Id: bar.h 1083 2013-02-14 11:31:55Z boman $

#ifndef BAR_H
#define BAR_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Bar
{
public:
	double k;
	double rho;
	double cv;
	double E;
	double alpha;
	double T0;
	double L;
public:
    Bar();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out, Bar const &obj);
	void save(std::string const &filename) const;
};

#endif
