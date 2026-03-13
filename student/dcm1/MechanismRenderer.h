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

#ifndef MECHANISM_RENDERER_H
#define MECHANISM_RENDERER_H

#include <QPainter>
#include <QRect>
#include "MechanismKinematicsSolver.h"

struct RenderStyleSettings
{
    double linkPenWidth = 2.0;
    double trajectoryPWidth = 2.0;
    double trajectoryDWidth = 0.5;
    double groundHalfLen = 0.5;   // world units
    double filmExtension = 10.0;  // world units
    int labelFontSize = 10;       // pt
    int labelOffsetX = 3;         // px
    int labelOffsetY = 3;         // px
    int paramBoxX = 10;           // px
    int paramBoxY = 10;           // px
    int paramBoxWidth = 200;      // px
    int paramBoxHeight = 300;     // px
};

class MechanismRenderer
{
public:
    static void draw(QPainter &painter, const TrajectoryGeometry &geometry,
                     const MechanismParameters &params, int frame, int nframes,
                     int ox, int oy, double zoom,
                     const RenderStyleSettings &style,
                     const QRect &widgetRect);
};

#endif
