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
#include <QSettings>
#include <QTimerEvent>

namespace
{
double mapSliderValue(int value, double minValue, double maxValue)
{
    const int range = 100;
    if (value < 0)
        value = 0;
    if (value > range)
        value = range;

    return minValue + value * (maxValue - minValue) / range;
}

void setParameterFromSlider(double &target, bool &dirty, int value,
                            double minValue, double maxValue)
{
    target = mapSliderValue(value, minValue, maxValue);
    dirty = true;
}

RenderStyleSettings loadRenderStyleFromSettings()
{
    QSettings settings;
    RenderStyleSettings style;
    auto readColor = [&settings](const char *key, const QColor &defaultColor) {
        const QColor color(settings.value(key, defaultColor.name(QColor::HexArgb))
                               .toString());
        return color.isValid() ? color : defaultColor;
    };

    style.linkPenWidth =
        settings.value("render/linkPenWidth", style.linkPenWidth).toDouble();
    style.trajectoryPWidth =
        settings.value("render/trajectoryPWidth", style.trajectoryPWidth).toDouble();
    style.trajectoryDWidth =
        settings.value("render/trajectoryDWidth", style.trajectoryDWidth).toDouble();
    style.groundHalfLen =
        settings.value("render/groundHalfLen", style.groundHalfLen).toDouble();
    style.filmExtension =
        settings.value("render/filmExtension", style.filmExtension).toDouble();

    style.labelFontSize =
        settings.value("render/labelFontSize", style.labelFontSize).toInt();
    style.labelOffsetX =
        settings.value("render/labelOffsetX", style.labelOffsetX).toInt();
    style.labelOffsetY =
        settings.value("render/labelOffsetY", style.labelOffsetY).toInt();

    style.mainColor = readColor("render/mainColor", style.mainColor);
    style.trajectoryPColor =
        readColor("render/trajectoryPColor", style.trajectoryPColor);
    style.trajectoryDColor =
        readColor("render/trajectoryDColor", style.trajectoryDColor);

    return style;
}

void saveRenderStyleToSettings(const RenderStyleSettings &style)
{
    QSettings settings;
    settings.setValue("render/linkPenWidth", style.linkPenWidth);
    settings.setValue("render/trajectoryPWidth", style.trajectoryPWidth);
    settings.setValue("render/trajectoryDWidth", style.trajectoryDWidth);
    settings.setValue("render/groundHalfLen", style.groundHalfLen);
    settings.setValue("render/filmExtension", style.filmExtension);

    settings.setValue("render/labelFontSize", style.labelFontSize);
    settings.setValue("render/labelOffsetX", style.labelOffsetX);
    settings.setValue("render/labelOffsetY", style.labelOffsetY);
    settings.setValue("render/mainColor", style.mainColor.name(QColor::HexArgb));
    settings.setValue("render/trajectoryPColor",
                      style.trajectoryPColor.name(QColor::HexArgb));
    settings.setValue("render/trajectoryDColor",
                      style.trajectoryDColor.name(QColor::HexArgb));
}
} // namespace

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

    renderStyle = loadRenderStyleFromSettings();

    frame = 1;
    myTimerId = 0;
}

void
Barres::timerEvent(QTimerEvent *event)
{
    if (event->timerId() == myTimerId)
    {
        // std::cout << "Timer!\n";
        ++frame;
        if (frame == nframes)
            frame = 0;
        update();
    }
    else
        QWidget::timerEvent(event);
}

void
Barres::showEvent(QShowEvent *)
{
    startAnimation();
}

MechanismParameters
Barres::currentParameters() const
{
    return params;
}

void
Barres::applyParameters(const MechanismParameters &newParams)
{
    params = newParams;
    geometryDirty = true;
    update();
}

RenderStyleSettings
Barres::currentRenderStyle() const
{
    return renderStyle;
}

void
Barres::applyRenderStyle(const RenderStyleSettings &newStyle)
{
    renderStyle = newStyle;
    saveRenderStyleToSettings(renderStyle);
    update();
}

void
Barres::hideEvent(QHideEvent *)
{
    stopAnimation();
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
                            oy, zoom, renderStyle, this->rect());

    // painter.end();   // avoids "qpainter : cannot destroy paint device that
    // is being painted"
}

void
Barres::startAnimation()
{
    if (myTimerId == 0)
    {
        myTimerId = startTimer(25);
        emit animationStarted();
    }
}

void
Barres::stopAnimation()
{
    if (myTimerId != 0)
    {
        killTimer(myTimerId);
        myTimerId = 0;
        emit animationStopped();
    }
}

void
Barres::toggleAnimation()
{
    if (myTimerId == 0)
        startAnimation();
    else
        stopAnimation();
}

void
Barres::resetAnimation()
{
    frame = 0;
    update();
}

void
Barres::stepForward()
{
    ++frame;
    if (frame == nframes)
        frame = 0;
    update();
}

void
Barres::stepBackward()
{
    --frame;
    if (frame < 0)
        frame = nframes - 1;
    update();
}

void
Barres::set_a1_slot(int i)
{
    setParameterFromSlider(params.a1, geometryDirty, i, 0.1, 2.0);
}

void
Barres::set_a2_slot(int i)
{
    setParameterFromSlider(params.a2, geometryDirty, i, 2.5, 4.5);
}

void
Barres::set_a3_slot(int i)
{
    setParameterFromSlider(params.a3, geometryDirty, i, 1.0, 3.0);
}

void
Barres::set_xb_slot(int i)
{
    setParameterFromSlider(params.xb, geometryDirty, i, 2.0, 4.0);
}

void
Barres::set_ya_slot(int i)
{
    setParameterFromSlider(params.ya, geometryDirty, i, 0.5, 2.5);
}

void
Barres::set_L_slot(int i)
{
    setParameterFromSlider(params.L, geometryDirty, i, 4.0, 8.0);
}

void
Barres::set_e_slot(int i)
{
    setParameterFromSlider(params.e, geometryDirty, i, 0.0, 3.0);
}

void
Barres::set_dp_slot(int i)
{
    setParameterFromSlider(params.dp, geometryDirty, i, 0.5, 1.5);
}
