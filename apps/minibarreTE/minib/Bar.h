#ifndef BAR_H
#define BAR_H

#include <iostream>

struct Bar
{
	double kappa;
	double rho;
	double cv;
	double Q;
	double f;
	double E;
	double alpha;
	double T0;
	double L;
public:
    Bar();
    friend std::ostream &operator<<(std::ostream &out, Bar const &obj);
};

#endif
