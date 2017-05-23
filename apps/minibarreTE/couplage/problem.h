// $Id: problem.h 1084 2013-02-18 08:46:33Z boman $

#ifndef PROBLEM_H
#define PROBLEM_H

#include "couplage.h"
#include "bar.h"
#include "light.h"
#include "mesh.h"
#include "newmark.h"
#include "resfiles.h"
#include "plotwin.h"

class COUPLAGE_API Problem
{
public:
    Bar bar;
    Light light;
    Mesh msh;
    Newmark nmark;
    ResFiles results;
    PlotWin plot;

public:
    Problem();
    friend COUPLAGE_API std::ostream &operator<<(std::ostream &out, Problem const &obj);
    Bar &getBar() { return bar;}
    Light &getLight() { return light;}
    Mesh &getMesh() { return msh;}
    Newmark &getNewmark() { return nmark;}
    ResFiles &getResFiles() { return results;}
    PlotWin &getPlotWin() { return plot;}

    void solve();
private:
    void applyBC(gmm::row_matrix<gmm::wsvector<double> > &mat, int line);
};

#endif
