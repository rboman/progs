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
#include <QHBoxLayout>
#include <QLabel>
#include <QMenuBar>
#include <QMessageBox>
#include <QSlider>
#include <QStatusBar>

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
    pan->setMaximumSize(QSize(320, 999999));
    hbox->addWidget(pan);

    QVBoxLayout *vbox = new QVBoxLayout(pan);

    QGroupBox *groupBox = new QGroupBox("Parameters");
    QFormLayout *formLayout = new QFormLayout();
    groupBox->setLayout(formLayout);

    auto addSlider = [formLayout, this](const QString &label,
                                        double minValue, double maxValue,
                                        void (Barres::*slot)(int)) {
        QSlider *slider = new QSlider(Qt::Horizontal);
        slider->setMinimumWidth(180);
        QLabel *nameLabel = new QLabel(label);
        nameLabel->setMinimumWidth(28);
        QLabel *valueLabel = new QLabel();
        valueLabel->setMinimumWidth(50);
        valueLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

        QWidget *rowWidget = new QWidget();
        QHBoxLayout *rowLayout = new QHBoxLayout(rowWidget);
        rowLayout->setContentsMargins(0, 0, 0, 0);
        rowLayout->addWidget(slider, 1);
        rowLayout->addWidget(valueLabel);

        formLayout->addRow(nameLabel, rowWidget);
        slider->setRange(0, 100);

        auto updateValueLabel = [valueLabel, minValue, maxValue](int rawValue) {
            const double value = minValue +
                                 rawValue * (maxValue - minValue) / 100.0;
            valueLabel->setText(QString::number(value, 'f', 3));
        };

        QObject::connect(slider, &QSlider::valueChanged, this->viewer, slot);
        QObject::connect(slider, &QSlider::valueChanged,
                         this, updateValueLabel);
        slider->setValue(50);
    };

    addSlider("a1", 0.1, 2.0, &Barres::set_a1_slot);
    addSlider("a2", 2.5, 4.5, &Barres::set_a2_slot);
    addSlider("a3", 1.0, 3.0, &Barres::set_a3_slot);
    addSlider("xb", 2.0, 4.0, &Barres::set_xb_slot);
    addSlider("ya", 0.5, 2.5, &Barres::set_ya_slot);
    addSlider("L", 4.0, 8.0, &Barres::set_L_slot);
    addSlider("e", 0.0, 3.0, &Barres::set_e_slot);
    addSlider("dp", 0.5, 1.5, &Barres::set_dp_slot);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);

    // -- menu bar
    QMenu *menuFile = menuBar()->addMenu("&Fichier");
    QAction *actionQuit = menuFile->addAction("&Quitter");
    actionQuit->setShortcut(QKeySequence::Quit);
    QObject::connect(actionQuit, &QAction::triggered,
                     qApp, &QApplication::quit);

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
        statusBar()->showMessage("Animation demarree");
    });
    QObject::connect(viewer, &Barres::animationStopped, this, [this]() {
        actionStart->setEnabled(true);
        actionStop->setEnabled(false);
        statusBar()->showMessage("Animation arretee");
    });

    QMenu *menuHelp = menuBar()->addMenu("&Aide");
    QAction *actionAbout = menuHelp->addAction("&A propos");
    QObject::connect(actionAbout, &QAction::triggered, this, [this]() {
        QMessageBox::about(this, "A propos",
                           "Barres\n"
                           "Simulation d'un mecanisme planar.\n"
                           "Projet Qt Widgets / C++.");
        statusBar()->showMessage("Affichage de la boite A propos", 2000);
    });

    statusBar()->showMessage("Pret");
}
