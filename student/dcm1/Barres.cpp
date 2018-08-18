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

//          ---------SIMULATION DU MECANISME 4 BARRES---------
//          --------------------------------------------------

#include "Barres.h"
#include <math.h>

#include <QTimerEvent>
#include <QColor>
#include <QPen>

Barres::Barres(QWidget *parent) : QWidget(parent)
{
    pi = 4*atan(1.0);    
    
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

    a1 = 0.9501; // AD
    a2 = 3.3933; // DC
    a3 = 2.0360; // BC
    xb = 3.3427; 
    ya = 1.5459;

    L = 6.1079; // DP'
    e = 1.4595;
    dp = 0.5459; // PP'

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
        //std::cout << "Timer!\n";
        frame+=1;
        if (frame==nframes) frame=0;
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
    //std::cout << "paintEvent: please wait...\n";

    double theta1[nframes]; 

    double x1[nframes]; 
    double x2[nframes];

    double x[6][nframes]; 
    double y[6][nframes];

    for (int i = 0; i < nframes; i++)
    {
        theta1[i] = 2*pi *i/nframes;
        double k1 = xb - a1 * cos(theta1[i]);
        double k2 = -ya - a1 * sin(theta1[i]);
        double k3 = (k1 * k1 + k2 * k2 + a2 * a2 - a3 * a3) / (2.0 * a2);

        x1[i] = 2.0 * atan((k2 + sqrt(k2 * k2 - (k3 + k1) * (k3 - k1))) / (k3 + k1));
        if ( (k1 - a2 * cos(x1[i])) / a3 > 0.0)
            x2[i] = asin((k2 - a2 * sin(x1[i])) / a3);
        else
            x2[i] = -asin((k2 - a2 * sin(x1[i])) / a3) - pi;

        x[0][i] = 0.0;
        y[0][i] = ya;

        x[1][i] = a1 * cos(theta1[i]);
        y[1][i] = ya + a1 * sin(theta1[i]);

        x[2][i] = x[1][i] + a2 * cos(x1[i]);
        y[2][i] = y[1][i] + a2 * sin(x1[i]);

        x[3][i] = xb;
        y[3][i] = 0.0;

        x[4][i] = x[1][i] + L * cos(x1[i]);
        y[4][i] = y[1][i] + L * sin(x1[i]);

        x[5][i] = x[4][i] + dp * cos(x1[i] - pi / 2);
        y[5][i] = y[4][i] + dp * sin(x1[i] - pi / 2);
    }

    // apply transl + zoom

    for (int i = 0; i < 6; i++)
          for (int j = 0; j < nframes; j++)
          {
                x[i][j] = ox + x[i][j] * zoom;
                y[i][j] = oy - y[i][j] * zoom; // * 350 / 480;
          }

    int i = frame;

    QPainter painter(this);
    painter.setRenderHint(QPainter::Antialiasing, true);

    QPen pen1(Qt::black, 2.0);
    //pen.setColor(palette().dark().color());
    painter.setPen(pen1);

    painter.drawLine(x[0][i], y[0][i], x[1][i], y[1][i]); // 0-1
    painter.drawLine(x[1][i], y[1][i], x[4][i], y[4][i]); // 1-4
    painter.drawLine(x[4][i], y[4][i], x[5][i], y[5][i]); // 4-5
    painter.drawLine(x[3][i], y[3][i], x[2][i], y[2][i]); // 3-2

    // film
    painter.drawLine(x[3][i], y[3][i] - e * zoom, x[3][i] + 10 * zoom, y[3][i] - e * zoom);
    // ground near B
    painter.drawLine(x[3][i] - 0.5 * zoom, y[3][i], x[3][i] + 0.5 * zoom, y[3][i]);
    // ground near A
    painter.drawLine(ox - 0.5 * zoom, oy - ya * zoom, 
                     ox + 0.5 * zoom, oy - ya * zoom);    
    
    // -- draw labels
    pen1.setColor(Qt::black);
    painter.setPen(pen1);
    QFont font = painter.font();
    font.setPointSize(10);
    painter.setFont(font);
    painter.drawText(QPoint(x[0][i]+3, y[0][i]-3),"A");
    painter.drawText(QPoint(x[1][i]+3, y[1][i]-3),"D");
    painter.drawText(QPoint(x[2][i]+3, y[2][i]-3),"C");
    painter.drawText(QPoint(x[3][i]+3, y[3][i]-3),"B");
    painter.drawText(QPoint(x[4][i]+3, y[4][i]-3),"P'");
    painter.drawText(QPoint(x[5][i]+3, y[5][i]-3),"P");
    

    // -- trajectory

    painter.setPen(QPen(Qt::red, 2.0));
    for (int j = 0; j < nframes-1; j++)
        painter.drawLine(x[5][j], y[5][j], x[5][j + 1], y[5][j + 1]);
    painter.drawLine(x[5][nframes-1], y[5][nframes-1], x[5][0], y[5][0]);

    // -- trajectory pt2
    int npt=1;
    painter.setPen(QPen(Qt::darkBlue, 0.5));
    for (int j = 0; j < nframes-1; j++)
        painter.drawLine(x[npt][j], y[npt][j], x[npt][j + 1], y[npt][j + 1]);
    painter.drawLine(x[npt][nframes-1], y[npt][nframes-1], x[npt][0], y[npt][0]);


    // write parameters values
    painter.setPen(QPen(Qt::black));
    QRect rect(10, 10, 200, 300);
    QString argtxt = QString("a1 = %1\na2 = %2\na3 = %3\nxb = %4\nya = %5\nL = %6\ne = %7\ndp = %8")
    .arg(a1).arg(a2).arg(a3).arg(xb).arg(ya).arg(L).arg(e).arg(dp);
    painter.drawText(rect, Qt::AlignLeft|Qt::AlignTop, argtxt);
    // value of "i"
    painter.drawText(this->rect(), Qt::AlignHCenter|Qt::AlignTop, QString("frame=%1/%2").arg(frame).arg(nframes));   


    //painter.end();   // avoids "qpainter : cannot destroy paint device that is being painted" 
}

void 
Barres::set_a1_slot(int i)
{
    double a1min = 0.1;
    double a1max = 2.0;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    a1 = a1min + i*(a1max-a1min)/range;
}

void 
Barres::set_a2_slot(int i)
{
    double a2min = 2.5;
    double a2max = 4.5;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    a2 = a2min + i*(a2max-a2min)/range;
}

void 
Barres::set_a3_slot(int i)
{
    double a3min = 1.0;
    double a3max = 3.0;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    a3 = a3min + i*(a3max-a3min)/range;
}

void 
Barres::set_xb_slot(int i)
{
    double xbmin = 2.0;
    double xbmax = 4.0;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    xb = xbmin + i*(xbmax-xbmin)/range;
}

void 
Barres::set_ya_slot(int i)
{
    double yamin = 0.5;
    double yamax = 2.5;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    ya = yamin + i*(yamax-yamin)/range;
}

void 
Barres::set_L_slot(int i)
{
    double Lmin = 4.0;
    double Lmax = 8.0;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    L = Lmin + i*(Lmax-Lmin)/range;
}

void 
Barres::set_e_slot(int i)
{
    double emin = 0.0;
    double emax = 3.0;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    e = emin + i*(emax-emin)/range;
}

void 
Barres::set_dp_slot(int i)
{
    double dpmin = 0.5;
    double dpmax = 1.5;
    int range = 100;
    if(i<0) i=0;
    if(i>range) i=range;
    
    dp = dpmin + i*(dpmax-dpmin)/range;
}
