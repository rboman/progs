//   Copyright 2017 Romain Boman
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

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
