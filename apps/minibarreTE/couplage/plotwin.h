// $Id$

#ifndef PLOTWIN_H
#define PLOTWIN_H

#include "couplage.h"
#include <vector>
class Gnuplot;

class COUPLAGE_API PlotWin
{
	Gnuplot *plot;
	std::vector<double> *x;
	std::vector<double> *inc;
    int nstep;
public:
    int freq;
    double ymin;
    double ymax;
public:
	PlotWin();
	void init(std::vector<double> &_x, std::vector<double> &_inc);
	~PlotWin();
	void update();
};

#endif
