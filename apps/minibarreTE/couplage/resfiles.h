// $Id$

#ifndef RESFILES_H
#define RESFILES_H

#include "couplage.h"
#include <vector>
#include <fstream>

class COUPLAGE_API ResFiles
{
	std::ofstream *dispfile;
	std::ofstream *tempfile;
	std::ofstream *timefile;
	std::ofstream *lightfile;

	std::vector<double> *inc;
	double *time;
	double *light;
    int nstep;
public:
    int freq;
public:
	ResFiles();
	~ResFiles();
    
#ifndef SWIG
	void init(std::vector<double> &_inc, double &_time, double &_light);
	void update();
#endif
};

#endif
