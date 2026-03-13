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
                        const RenderStyleSettings &style,
                        const QRect &widgetRect)
{
    auto sx = [ox, zoom](double wx) { return ox + wx * zoom; };
    auto sy = [oy, zoom](double wy) { return oy - wy * zoom; };
    auto pt = [&geometry, &sx, &sy](int p, int f) {
        return QPointF(sx(geometry.x[p][f]), sy(geometry.y[p][f]));
    };

    QPen pen1(Qt::black, style.linkPenWidth);
    // pen.setColor(palette().dark().color());
    painter.setPen(pen1);

    painter.drawLine(pt(A,      frame), pt(D,      frame)); // A-D
    painter.drawLine(pt(D,      frame), pt(Pprime, frame)); // D-P'
    painter.drawLine(pt(Pprime, frame), pt(P,      frame)); // P'-P
    painter.drawLine(pt(B,      frame), pt(C,      frame)); // B-C

    // film
    painter.drawLine(QPointF(sx(geometry.x[B][frame]),
                             sy(geometry.y[B][frame]) - params.e * zoom),
                 QPointF(sx(geometry.x[B][frame]) + style.filmExtension * zoom,
                             sy(geometry.y[B][frame]) - params.e * zoom));
    // ground near B
        painter.drawLine(QPointF(sx(geometry.x[B][frame]) - style.groundHalfLen * zoom,
                             sy(geometry.y[B][frame])),
                 QPointF(sx(geometry.x[B][frame]) + style.groundHalfLen * zoom,
                             sy(geometry.y[B][frame])));
    // ground near A
        painter.drawLine(ox - style.groundHalfLen * zoom, oy - params.ya * zoom,
                 ox + style.groundHalfLen * zoom, oy - params.ya * zoom);

    // -- draw labels
    pen1.setColor(Qt::black);
    painter.setPen(pen1);
    QFont font = painter.font();
    font.setPointSize(style.labelFontSize);
    painter.setFont(font);
    auto label = [&](PointIndex p, const char *text) {
        painter.drawText(
            QPoint(sx(geometry.x[p][frame]) + style.labelOffsetX,
                   sy(geometry.y[p][frame]) - style.labelOffsetY),
            text);
    };
    label(A,      "A");
    label(D,      "D");
    label(C,      "C");
    label(B,      "B");
    label(Pprime, "P'");
    label(P,      "P");

    // -- trajectory

    painter.setPen(QPen(Qt::red, style.trajectoryPWidth));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(P, j), pt(P, j + 1));
    painter.drawLine(pt(P, nframes - 1), pt(P, 0));

    // -- trajectory D
    painter.setPen(QPen(Qt::darkBlue, style.trajectoryDWidth));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(D, j), pt(D, j + 1));
    painter.drawLine(pt(D, nframes - 1), pt(D, 0));

    // write parameters values
    painter.setPen(QPen(Qt::black));
    QRect rect(style.paramBoxX, style.paramBoxY,
               style.paramBoxWidth, style.paramBoxHeight);
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
