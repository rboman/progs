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
#include <QApplication>
#include <QColor>
#include <QPen>

Barres::Barres(QWidget *parent) : QWidget(parent)
{
    zoom = 30;
    ox = 150;
    oy = 300;
    a1 = 1.5;
    a2 = 5.0;
    a3 = 3.0;
    xb = 4.5; ya = 3.0;
    L = 10.5;
    e = 1;
    dp = 0.5;
    pi = 3.141592;
    traj = 0;

    this->setWindowTitle("Barres! (DCM1-1994)");
    this->resize(640, 480);

    offset = 1;
    myTimerId = 0;
}

void 
Barres::timerEvent(QTimerEvent *event)
{
    if (event->timerId() == myTimerId) 
    {
        std::cout << "Timer!\n";
        offset+=1;
        if (offset==74) offset=1;
        update();
    } 
    else 
        QWidget::timerEvent(event);

}

void 
Barres::showEvent(QShowEvent *event)
{
    myTimerId = startTimer(50); // in ms
}

void 
Barres::hideEvent(QHideEvent *event)
{
    killTimer(myTimerId);
}

void 
Barres::paintEvent(QPaintEvent *event)
{
    std::cout << "paintEvent: please wait...\n";
    QPainter *painter = new QPainter(this);

    //int page, i, j;
    float pas, k1, k2, k3, theta1[74], x1[74], x2[74], x[6][74], y[6][74];
    //int page = 0;
    for (int i = 1; i <= 73; i++)
    {
          pas = i;
          theta1[i] = (pas - 1) * 5 * pi / 180;
          k1 = xb - a1 * cos(theta1[i]);
          k2 = -ya - a1 * sin(theta1[i]);
          k3 = (k1 * k1 + k2 * k2 + a2 * a2 - a3 * a3) / (2 * a2);

          x1[i] = 2 * atan((k2 + sqrt(k2 * k2 - (k3 + k1) * (k3 - k1))) / (k3 + k1));
          if (((k1 - a2 * cos(x1[i])) / a3) > 0)
                x2[i] = asin((k2 - a2 * sin(x1[i])) / a3);
          else
                x2[i] = -asin((k2 - a2 * sin(x1[i])) / a3) - pi;
          x[0][i] = 0;
          y[0][i] = ya;
          x[1][i] = a1 * cos(theta1[i]);
          y[1][i] = ya + a1 * sin(theta1[i]);
          x[2][i] = x[1][i] + a2 * cos(x1[i]);
          y[2][i] = y[1][i] + a2 * sin(x1[i]);
          x[3][i] = xb;
          y[3][i] = 0;
          x[4][i] = x[1][i] + L * cos(x1[i]);
          y[4][i] = y[1][i] + L * sin(x1[i]);
          x[5][i] = x[4][i] + dp * cos(x1[i] - pi / 2);
          y[5][i] = y[4][i] + dp * sin(x1[i] - pi / 2);
    }

    // apply transl + zoom

    for (int i = 0; i < 6; i++)
          for (int j = 1; j < 74; j++)
          {
                x[i][j] = ox + x[i][j] * zoom;
                y[i][j] = oy - y[i][j] * zoom * 350 / 480;
          }
/*
    for (int i = 1; i < 73; i++)
    {
        QPen pen(Qt::black, 2.0);

        painter->setPen(pen);
        painter->drawLine(x[5][i], y[5][i], x[5][i + 1], y[5][i + 1]);
    }
    */

    //for (int i = 1; i <= 73; i++)
    //{
        int i=offset;

        QPen pen1(Qt::black, 2.0);
        //pen.setColor(palette().dark().color());
        painter->setPen(pen1);

        painter->drawLine(x[0][i], y[0][i], x[1][i], y[1][i]);
        painter->drawLine(x[1][i], y[1][i], x[4][i], y[4][i]);
        painter->drawLine(x[4][i], y[4][i], x[5][i], y[5][i]);
        painter->drawLine(x[3][i], y[3][i], x[2][i], y[2][i]);
        painter->drawLine(x[3][i], y[3][i] - e * zoom * 350 / 480, x[3][i] + 10 * zoom, y[3][i] - e * zoom * 350 / 480);
        painter->drawLine(x[3][i] - 0.5 * zoom, y[3][i], x[3][i] + 0.5 * zoom, y[3][i]);
        painter->drawLine(ox - 0.5 * zoom, oy - ya * zoom * 350 / 480, ox + 0.5 * zoom, oy - ya * zoom * 350 / 480);    
    //}
    

    // trajectoire
    QPen pen2(Qt::red, 2.0);
    painter->setPen(pen2);
    for (int j = 1; j < 73; j++)
        painter->drawLine(x[5][j], y[5][j], x[5][j + 1], y[5][j + 1]);


    painter->end();   // avoids "qpainter : cannot destroy paint device that is being painted" 
}

int main(int argc, char *argv[])
{
	QApplication *app = new QApplication(argc, argv);
	Barres *win = new Barres();
	win->show();
	app->exec();
	return 0;
}
