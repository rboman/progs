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

#include "Barres.h"
#include "MechanismRenderer.h"
#include <QTimerEvent>

Barres::Barres(QWidget *parent)
    : QWidget(parent), geometryCache(nframes), geometryDirty(true)
{
    ox = 150;
    oy = 300;

    /*
    // initial set
    zoom = 30;
    a1 = 1.5; // AD
    a2 = 5.0; // DC
    a3 = 3.0; // BC
    xb = 4.5;
    ya = 3.0;

    L = 10.5; // DP'
    e = 1;
    dp = 0.5; // PP'
*/

    // optimsed set
    zoom = 60;

    params.a1 = 0.9501; // AD
    params.a2 = 3.3933; // DC
    params.a3 = 2.0360; // BC
    params.xb = 3.3427;
    params.ya = 1.5459;

    params.L = 6.1079; // DP'
    params.e = 1.4595;
    params.dp = 0.5459; // PP'

    this->setWindowTitle("Barres! (DCM1-1994)");
    this->resize(640, 480);

    frame = 1;
    myTimerId = 0;
}

void
Barres::timerEvent(QTimerEvent *event)
{
    if (event->timerId() == myTimerId)
    {
        // std::cout << "Timer!\n";
        frame += 1;
        if (frame == nframes)
            frame = 0;
        update();
    }
    else
        QWidget::timerEvent(event);
}

void
Barres::showEvent(QShowEvent *event)
{
    // create a timer using member fct
    myTimerId = startTimer(25); // in ms
}

void
Barres::hideEvent(QHideEvent *event)
{
    killTimer(myTimerId);
}

void
Barres::paintEvent(QPaintEvent *event)
{
    // std::cout << "paintEvent: please wait...\n";

    if (geometryDirty)
    {
        geometryCache = MechanismKinematicsSolver::compute(params, nframes);
        geometryDirty = false;
    }

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);
    MechanismRenderer::draw(painter, geometryCache, params, frame, nframes, ox,
                            oy, zoom, this->rect());

    // painter.end();   // avoids "qpainter : cannot destroy paint device that
    // is being painted"
}

void
Barres::set_a1_slot(int i)
{
    double a1min = 0.1;
    double a1max = 2.0;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.a1 = a1min + i * (a1max - a1min) / range;
    geometryDirty = true;
}

void
Barres::set_a2_slot(int i)
{
    double a2min = 2.5;
    double a2max = 4.5;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.a2 = a2min + i * (a2max - a2min) / range;
    geometryDirty = true;
}

void
Barres::set_a3_slot(int i)
{
    double a3min = 1.0;
    double a3max = 3.0;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.a3 = a3min + i * (a3max - a3min) / range;
    geometryDirty = true;
}

void
Barres::set_xb_slot(int i)
{
    double xbmin = 2.0;
    double xbmax = 4.0;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.xb = xbmin + i * (xbmax - xbmin) / range;
    geometryDirty = true;
}

void
Barres::set_ya_slot(int i)
{
    double yamin = 0.5;
    double yamax = 2.5;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.ya = yamin + i * (yamax - yamin) / range;
    geometryDirty = true;
}

void
Barres::set_L_slot(int i)
{
    double Lmin = 4.0;
    double Lmax = 8.0;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.L = Lmin + i * (Lmax - Lmin) / range;
    geometryDirty = true;
}

void
Barres::set_e_slot(int i)
{
    double emin = 0.0;
    double emax = 3.0;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.e = emin + i * (emax - emin) / range;
    geometryDirty = true;
}

void
Barres::set_dp_slot(int i)
{
    double dpmin = 0.5;
    double dpmax = 1.5;
    int range = 100;
    if (i < 0)
        i = 0;
    if (i > range)
        i = range;

    params.dp = dpmin + i * (dpmax - dpmin) / range;
    geometryDirty = true;
}
