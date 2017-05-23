// $Id$
// from http://liuxingguang.blogspot.be/2012/05/how-to-call-gnuplot-from-c.html

#ifndef GNUPLOT_H
#define GNUPLOT_H

#include <string>
#include <iostream>

class Gnuplot 
{
public:
	Gnuplot() ;
	~Gnuplot();
	void operator ()(const std::string & command);
	// send any command to gnuplot
protected:
	FILE *gnuplotpipe;
};

#endif
