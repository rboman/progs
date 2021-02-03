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

#include "qtfract.h"
#include <QApplication>

#include <QPen>
#include <QPainter>
#include <iostream>
#include <vector>

Fract::Fract(QWidget *parent) : QWidget(parent)
{
    // input data
    x1 = -2.0;
    y1 = 1.2;
    x2 = 1.5;
    y2 = -1.2;
    nb_coul = 50;

    // setup palette
    for (int n = 0; n < nb_coul; ++n)
        colours.push_back(QColor((255 * n) / nb_coul, 0, 0));
    colours.push_back(QColor(0, 0, 0));

    this->setWindowTitle("Mandelbrot (Qt/C++)");
    this->resize(640, 480);
}

void
Fract::paintEvent(QPaintEvent *event)
{
    std::cout << "paintEvent: please wait..." << std::endl;
    QPainter painter(this);
    // QPainter *painter = new QPainter(this);

    double a1 = x2 - x1;
    double a2 = y2 - y1;
    int sl = this->width();
    int sh = this->height();

    for (int xe = 0; xe < sl; ++xe)
        for (int ye = 0; ye < sh; ++ye)
        {
            double xc = (a1 * xe) / sl + x1;
            double yc = (a2 * ye) / sh + y1;

            int n = 0;
            double xn = 0.0;
            double yn = 0.0;

            while (n < nb_coul && yn * yn + xn * xn < 4.0)
            {
                double xn2 = xn * xn - yn * yn + xc;
                double yn2 = 2 * xn * yn + yc;
                xn = xn2;
                yn = yn2;
                n++;
            }
            QPen pen;
            pen.setColor(colours[n]);
            painter.setPen(pen);
            painter.drawPoint(xe, ye);
            // painter->setPen(pen);
            // painter->drawPoint(xe, ye);
        }
    std::cout << "done." << std::endl;
    // painter->end(); // avoids "qpainter : cannot destroy paint device that is
    // being painted" painter.end();
}

int
main(int argc, char *argv[])
{
    QApplication *app = new QApplication(argc, argv);
    Fract *win = new Fract();
    win->show();
    app->exec();
    return 0;
}
