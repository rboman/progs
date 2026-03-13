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
#include <cmath>
#include <QCoreApplication>
#include <QPen>

namespace
{
double normalizeAngleDegrees(double angleDeg)
{
    while (angleDeg < 0.0)
        angleDeg += 360.0;
    while (angleDeg >= 360.0)
        angleDeg -= 360.0;
    return angleDeg;
}

void drawWorldGrid(QPainter &painter, const QRect &widgetRect,
                   int ox, int oy, double zoom)
{
    double worldStep = 1.0;
    while (worldStep * zoom < 25.0)
        worldStep *= 2.0;
    while (worldStep * zoom > 120.0)
        worldStep *= 0.5;

    const double stepPx = worldStep * zoom;
    const int left = widgetRect.left();
    const int right = widgetRect.right();
    const int top = widgetRect.top();
    const int bottom = widgetRect.bottom();

    painter.setPen(QPen(QColor(220, 220, 220), 1.0));

    double x = ox + std::floor((left - ox) / stepPx) * stepPx;
    for (; x <= right; x += stepPx)
        painter.drawLine(QPointF(x, top), QPointF(x, bottom));

    double y = oy + std::floor((top - oy) / stepPx) * stepPx;
    for (; y <= bottom; y += stepPx)
        painter.drawLine(QPointF(left, y), QPointF(right, y));

    painter.setPen(QPen(QColor(170, 170, 170), 1.4));
    painter.drawLine(QPointF(ox, top), QPointF(ox, bottom));
    painter.drawLine(QPointF(left, oy), QPointF(right, oy));
}
} // namespace

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

    drawWorldGrid(painter, widgetRect, ox, oy, zoom);

    QPen pen1(style.mainColor, style.linkPenWidth);
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
    pen1.setColor(style.mainColor);
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

    painter.setPen(QPen(style.trajectoryPColor, style.trajectoryPWidth));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(P, j), pt(P, j + 1));
    painter.drawLine(pt(P, nframes - 1), pt(P, 0));

    // -- trajectory D
    painter.setPen(QPen(style.trajectoryDColor, style.trajectoryDWidth));
    for (int j = 0; j < nframes - 1; j++)
        painter.drawLine(pt(D, j), pt(D, j + 1));
    painter.drawLine(pt(D, nframes - 1), pt(D, 0));

    // value of "i"
    painter.setPen(QPen(Qt::black));
    const QString frameText =
        QCoreApplication::translate("MechanismRenderer", "frame=%1/%2");
    painter.drawText(widgetRect, Qt::AlignHCenter | Qt::AlignTop,
                     frameText.arg(frame).arg(nframes));

    const auto angleDeg = [](double vx, double vy) {
        const double pi = 3.14159265358979323846;
        const double rad = std::atan2(vy, vx);
        return normalizeAngleDegrees(rad * 180.0 / pi);
    };

    const double thetaAD = angleDeg(geometry.x[D][frame] - geometry.x[A][frame],
                                    geometry.y[D][frame] - geometry.y[A][frame]);
    const double thetaDC = angleDeg(geometry.x[C][frame] - geometry.x[D][frame],
                                    geometry.y[C][frame] - geometry.y[D][frame]);
    const double thetaBC = angleDeg(geometry.x[C][frame] - geometry.x[B][frame],
                                    geometry.y[C][frame] - geometry.y[B][frame]);

    const QString angleTextTemplate = QCoreApplication::translate(
        "MechanismRenderer", "thetaAD=%1 deg\nthetaDC=%2 deg\nthetaBC=%3 deg");
    const QString angleText =
        angleTextTemplate
            .arg(thetaAD, 0, 'f', 1)
            .arg(thetaDC, 0, 'f', 1)
            .arg(thetaBC, 0, 'f', 1);
    painter.drawText(widgetRect.adjusted(10, 24, -10, -10),
                     Qt::AlignLeft | Qt::AlignTop, angleText);
}
