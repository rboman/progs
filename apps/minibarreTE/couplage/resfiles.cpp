
#include "resfiles.h"
//#include <iomanip>

ResFiles::ResFiles()
{
    inc = NULL;
    time = NULL;
    light = NULL;

    dispfile = NULL;
    tempfile = NULL;
    timefile = NULL;
    lightfile = NULL;

    freq = 1;
    nstep = 0;
}

ResFiles::~ResFiles()
{
    if (dispfile)
    {
        dispfile->close();
        delete dispfile;
    }
    if (tempfile)
    {
        tempfile->close();
        delete tempfile;
    }
    if (timefile)
    {
        timefile->close();
        delete timefile;
    }
    if (lightfile)
    {
        lightfile->close();
        delete lightfile;
    }
}

void ResFiles::init(std::vector<double> &_inc, double &_time, double &_light)
{
    inc = &_inc;
    time = &_time;
    light = &_light;
}

void ResFiles::update()
{
    nstep++;
    if (nstep % freq)
        return;

    if (!dispfile)
    {
        dispfile = new std::ofstream("disp.txt");
        (*dispfile) << std::scientific;
        tempfile = new std::ofstream("temp.txt");
        (*tempfile) << std::scientific;
        timefile = new std::ofstream("time.txt");
        (*timefile) << std::scientific;
        lightfile = new std::ofstream("source.txt");
        (*lightfile) << std::scientific;
    }

    int m = inc->size() / 2;
    for (int i = 0; i < m; ++i)
        (*tempfile) << (*inc)[i] << "; ";
    (*tempfile) << '\n';

    for (int i = m; i < 2 * m; ++i)
        (*dispfile) << (*inc)[i] << "; ";
    (*dispfile) << '\n';

    (*timefile) << *time << '\n';
    (*lightfile) << *light << '\n';
}
