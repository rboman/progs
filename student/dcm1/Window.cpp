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

#include "Window.h"
#include "Barres.h"
#include <QAction>
#include <QApplication>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QFrame>
#include <QGroupBox>
#include <QFormLayout>
#include <QLabel>
#include <QMenuBar>
#include <QSlider>

Window::Window(QWidget *parent) : QMainWindow(parent)
{
    this->setWindowTitle("Barres");
    this->resize(800, 600);

    QWidget *central = new QWidget(this);
    setCentralWidget(central);

    viewer = new Barres(central);
    viewer->setMinimumSize(QSize(600, 400));
    viewer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QHBoxLayout *hbox = new QHBoxLayout(central);
    hbox->addWidget(viewer);

    QFrame *pan = new QFrame(central);
    pan->setMaximumSize(QSize(200, 999999));
    hbox->addWidget(pan);

    QVBoxLayout *vbox = new QVBoxLayout(pan);

    QGroupBox *groupBox = new QGroupBox("Parameters");
    QFormLayout *formLayout = new QFormLayout();
    groupBox->setLayout(formLayout);

    auto addSlider = [formLayout, this](const QString &label,
                                        void (Barres::*slot)(int)) {
        QSlider *slider = new QSlider(Qt::Horizontal);
        formLayout->addRow(label, slider);
        slider->setRange(0, 100);
        QObject::connect(slider, &QSlider::valueChanged, this->viewer, slot);
        slider->setValue(50);
    };

    addSlider("a1", &Barres::set_a1_slot);
    addSlider("a2", &Barres::set_a2_slot);
    addSlider("a3", &Barres::set_a3_slot);
    addSlider("xb", &Barres::set_xb_slot);
    addSlider("ya", &Barres::set_ya_slot);
    addSlider("L", &Barres::set_L_slot);
    addSlider("e", &Barres::set_e_slot);
    addSlider("dp", &Barres::set_dp_slot);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);

    // -- menu bar
    QMenu *menuAnimation = menuBar()->addMenu("&Animation");
    actionStart = menuAnimation->addAction("&Démarrer");
    actionStop  = menuAnimation->addAction("&Arrêter");
    actionStart->setEnabled(true);
    actionStop->setEnabled(false);

    QObject::connect(actionStart, &QAction::triggered,
                     viewer, &Barres::startAnimation);
    QObject::connect(actionStop, &QAction::triggered,
                     viewer, &Barres::stopAnimation);
    QObject::connect(viewer, &Barres::animationStarted, this, [this]() {
        actionStart->setEnabled(false);
        actionStop->setEnabled(true);
    });
    QObject::connect(viewer, &Barres::animationStopped, this, [this]() {
        actionStart->setEnabled(true);
        actionStop->setEnabled(false);
    });

    QMenu *menuFile = menuBar()->addMenu("&Fichier");
    QAction *actionQuit = menuFile->addAction("&Quitter");
    actionQuit->setShortcut(QKeySequence::Quit);
    QObject::connect(actionQuit, &QAction::triggered,
                     qApp, &QApplication::quit);
}
