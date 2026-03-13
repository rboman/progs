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

#include "MechanismRenderer.h"
#include <QPen>

void
MechanismRenderer::draw(QPainter &painter, const TrajectoryGeometry &geometry,
                        const MechanismParameters &params, int frame,
                        int nframes, int ox, int oy, double zoom,
                        const QRect &widgetRect)
{
    auto sx = [ox, zoom](double wx) { return ox + wx * zoom; };
    auto sy = [oy, zoom](double wy) { return oy - wy * zoom; };
    auto pt = [&geometry, &sx, &sy](int p, int f) {
        return QPointF(sx(geometry.x[p][f]), sy(geometry.y[p][f]));
    };

    QPen pen1(Qt::black, 2.0);
    // pen.setColor(palette().dark().color());
    painter.setPen(pen1);

    painter.drawLine(pt(0, frame), pt(1, frame)); // 0-1
    painter.drawLine(pt(1, frame), pt(4, frame)); // 1-4
    painter.drawLine(pt(4, frame), pt(5, frame)); // 4-5
    painter.drawLine(pt(3, frame), pt(2, frame)); // 3-2

    // film
    painter.drawLine(QPointF(sx(geometry.x[3][frame]),
                             sy(geometry.y[3][frame]) - params.e * zoom),
                     QPointF(sx(geometry.x[3][frame]) + 10 * zoom,
                             sy(geometry.y[3][frame]) - params.e * zoom));
    // ground near B
    painter.drawLine(QPointF(sx(geometry.x[3][frame]) - 0.5 * zoom,
                             sy(geometry.y[3][frame])),
                     QPointF(sx(geometry.x[3][frame]) + 0.5 * zoom,
                             sy(geometry.y[3][frame])));
    // ground near A
    painter.drawLine(ox - 0.5 * zoom, oy - params.ya * zoom, ox + 0.5 * zoom,
                     oy - params.ya * zoom);

    // -- draw labels
    pen1.setColor(Qt::black);
    painter.setPen(pen1);
    QFont font = painter.font();
    font.setPointSize(10);
    painter.setFont(font);
    painter.drawText(
        QPoint(sx(geometry.x[0][frame]) + 3, sy(geometry.y[0][frame]) - 3),
        "A");
    painter.drawText(
        QPoint(sx(geometry.x[1][frame]) + 3, sy(geometry.y[1][frame]) - 3),
        "D");
    painter.drawText(
        QPoint(sx(geometry.x[2][frame]) + 3, sy(geometry.y[2][frame]) - 3),
        "C");
    painter.drawText(
        QPoint(sx(geometry.x[3][frame]) + 3, sy(geometry.y[3][frame]) - 3),
        "B");
    painter.drawText(
        QPoint(sx(geometry.x[4][frame]) + 3, sy(geometry.y[4][frame]) - 3),
        "P'");
    painter.drawText(
        QPoint(sx(geometry.x[5][frame]) + 3, sy(geometry.y[5][frame]) - 3),
        "P");

    // -- trajectory

    painter.setPen(QPen(Qt::red, 2.0));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(5, j), pt(5, j + 1));
    painter.drawLine(pt(5, nframes - 1), pt(5, 0));

    // -- trajectory pt2
    int npt = 1;
    painter.setPen(QPen(Qt::darkBlue, 0.5));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(npt, j), pt(npt, j + 1));
    painter.drawLine(pt(npt, nframes - 1), pt(npt, 0));

    // write parameters values
    painter.setPen(QPen(Qt::black));
    QRect rect(10, 10, 200, 300);
    QString argtxt = QString("a1 = %1\na2 = %2\na3 = %3\nxb = %4\nya = %5\nL = "
                             "%6\ne = %7\ndp = %8")
                         .arg(params.a1)
                         .arg(params.a2)
                         .arg(params.a3)
                         .arg(params.xb)
                         .arg(params.ya)
                         .arg(params.L)
                         .arg(params.e)
                         .arg(params.dp);
    painter.drawText(rect, Qt::AlignLeft | Qt::AlignTop, argtxt);
    // value of "i"
    painter.drawText(widgetRect, Qt::AlignHCenter | Qt::AlignTop,
                     QString("frame=%1/%2").arg(frame).arg(nframes));
}
