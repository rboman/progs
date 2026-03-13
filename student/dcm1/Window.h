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

#ifndef WINDOW_H
#define WINDOW_H

#include <QMainWindow>
class Barres;
class QAction;
class QCloseEvent;
class QDragEnterEvent;
class QDropEvent;
class QSlider;
struct MechanismParameters;

class Window : public QMainWindow
{
    Q_OBJECT;

private:
    Barres  *viewer;
    QAction *actionStart;
    QAction *actionStop;
    QSlider *sliderA1;
    QSlider *sliderA2;
    QSlider *sliderA3;
    QSlider *sliderXb;
    QSlider *sliderYa;
    QSlider *sliderL;
    QSlider *sliderE;
    QSlider *sliderDp;

    bool importParametersFromJsonFile(const QString &fileName);
    void syncSlidersFromParameters(const MechanismParameters &p);

public:
    explicit Window(QWidget *parent = nullptr);

protected:
    void closeEvent(QCloseEvent *event) override;
    void dragEnterEvent(QDragEnterEvent *event) override;
    void dropEvent(QDropEvent *event) override;
};

#endif
