// $Id$

#ifndef NEWMARK_H
#define NEWMARK_H

#include "couplage.h"
#include <iostream>
#include <string>

class COUPLAGE_API Newmark
{
public:
	int nt;               // nombre de pas de temps
	double dt;           // pas de temps 
	double gamma;            // newmark
	double beta;             // newmark
public:
    Newmark();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out, Newmark const &obj);
	void save(std::string const &filename) const;
};

#endif
