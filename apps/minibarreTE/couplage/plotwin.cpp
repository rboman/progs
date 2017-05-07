
#include "plotwin.h"
#include "gnuplot.h"
#include <sstream>

PlotWin::PlotWin()
{
	plot = NULL;
    nstep = 0;
    freq = 1;
    ymin = 0.0;
    ymax = 1.0;
}

PlotWin::~PlotWin()
{
	delete plot;
}

void
PlotWin::init(std::vector<double> &_x, std::vector<double> &_inc)
{
    x   = &_x;
    inc = &_inc;
}

void
PlotWin::update()
{
    nstep++;
    if(nstep%freq) return;

	if(!plot)
	{
		plot = new Gnuplot();
		(*plot)("set title 'Output windows'");
		//(*plot)("set linetype 1 lw 2 lc rgb 'blue' pointtype 1");
	}

	//(*plot)("plot sin(x)") ;
	//(*plot)("plot '-' with linespoints");
	(*plot)("plot '-' with lines");
	for(size_t i=0; i<x->size(); i++)
	{
		std::stringstream str; str << (*x)[i] << " " << (*inc)[i];
		(*plot)(str.str());
	}
	(*plot)("e");

    std::stringstream str; str << "set yrange [" << ymin << ":" << ymax << "]";
	(*plot)(str.str());
}

