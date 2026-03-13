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
#include <QDateTime>
#include <QFile>
#include <QFileDialog>
#include <QHBoxLayout>
#include <QFrame>
#include <QFormLayout>
#include <QGroupBox>
#include <QJsonDocument>
#include <QJsonObject>
#include <QLabel>
#include <QMenuBar>
#include <QMessageBox>
#include <QShortcut>
#include <QSlider>
#include <QStatusBar>
#include <QVBoxLayout>

Window::Window(QWidget *parent) : QMainWindow(parent)
{
    this->setWindowTitle("Barres");
    this->resize(800, 600);
    this->setMinimumSize(900, 580);

    QWidget *central = new QWidget(this);
    setCentralWidget(central);

    viewer = new Barres(central);
    viewer->setMinimumSize(QSize(600, 400));
    viewer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QHBoxLayout *hbox = new QHBoxLayout(central);
    hbox->addWidget(viewer);

    QFrame *pan = new QFrame(central);
    pan->setMinimumWidth(300);
    pan->setMaximumWidth(380);
    pan->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
    hbox->addWidget(pan);

    hbox->setStretch(0, 4);
    hbox->setStretch(1, 1);

    QVBoxLayout *vbox = new QVBoxLayout(pan);

    QGroupBox *groupBox = new QGroupBox("Parameters");
    QFormLayout *formLayout = new QFormLayout();
    formLayout->setHorizontalSpacing(10);
    formLayout->setLabelAlignment(Qt::AlignRight | Qt::AlignVCenter);
    formLayout->setFieldGrowthPolicy(QFormLayout::ExpandingFieldsGrow);
    groupBox->setLayout(formLayout);

    auto addSlider = [formLayout, this](const QString &label,
                                        const QString &tooltip,
                                        double minValue, double maxValue,
                                        void (Barres::*slot)(int)) {
        QSlider *slider = new QSlider(Qt::Horizontal);
        slider->setMinimumWidth(140);
        QLabel *nameLabel = new QLabel(label);
        nameLabel->setMinimumWidth(28);
        nameLabel->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred);
        nameLabel->setToolTip(tooltip);
        slider->setToolTip(tooltip);
        QLabel *valueLabel = new QLabel();
        valueLabel->setMinimumWidth(50);
        valueLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

        QWidget *rowWidget = new QWidget();
        QHBoxLayout *rowLayout = new QHBoxLayout(rowWidget);
        rowLayout->setContentsMargins(0, 0, 0, 0);
        rowLayout->setSpacing(6);
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

    addSlider("a1", "Longueur AD (manivelle)", 0.1, 2.0, &Barres::set_a1_slot);
    addSlider("a2", "Longueur DC (bielle)", 2.5, 4.5, &Barres::set_a2_slot);
    addSlider("a3", "Longueur BC (balancier)", 1.0, 3.0, &Barres::set_a3_slot);
    addSlider("xb", "Position x du pivot B", 2.0, 4.0, &Barres::set_xb_slot);
    addSlider("ya", "Position y du pivot A", 0.5, 2.5, &Barres::set_ya_slot);
    addSlider("L", "Longueur DP' (point outil)", 4.0, 8.0, &Barres::set_L_slot);
    addSlider("e", "Offset vertical du film", 0.0, 3.0, &Barres::set_e_slot);
    addSlider("dp", "Distance P' -> P", 0.5, 1.5, &Barres::set_dp_slot);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);

    // -- menu bar
    QMenu *menuFile = menuBar()->addMenu("&Fichier");
    QAction *actionExport = menuFile->addAction("&Exporter parametres...");
    actionExport->setShortcut(QKeySequence::Save);
    QObject::connect(actionExport, &QAction::triggered, this, [this]() {
        const QString fileName = QFileDialog::getSaveFileName(
            this, "Exporter les parametres", "barres-params.json",
            "JSON files (*.json)");
        if (fileName.isEmpty())
            return;

        const MechanismParameters p = viewer->currentParameters();

        QJsonObject paramsObj;
        paramsObj["a1"] = p.a1;
        paramsObj["a2"] = p.a2;
        paramsObj["a3"] = p.a3;
        paramsObj["xb"] = p.xb;
        paramsObj["ya"] = p.ya;
        paramsObj["L"] = p.L;
        paramsObj["e"] = p.e;
        paramsObj["dp"] = p.dp;

        QJsonObject root;
        root["format"] = "barres-params";
        root["version"] = 1;
        root["savedAt"] = QDateTime::currentDateTimeUtc().toString(Qt::ISODate);
        root["params"] = paramsObj;

        QFile file(fileName);
        if (!file.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QMessageBox::warning(this, "Export impossible",
                                 "Impossible d'ouvrir le fichier pour ecriture.");
            statusBar()->showMessage("Echec de l'export JSON", 2000);
            return;
        }

        const QByteArray payload = QJsonDocument(root).toJson(QJsonDocument::Indented);
        const qint64 written = file.write(payload);
        if (written != payload.size())
        {
            QMessageBox::warning(this, "Export incomplet",
                                 "Le fichier n'a pas ete ecrit completement.");
            statusBar()->showMessage("Export JSON incomplet", 2000);
            return;
        }

        statusBar()->showMessage("Parametres exportes en JSON", 2000);
    });

    menuFile->addSeparator();
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
                           "Projet Qt Widgets / C++.\n\n"
                           "Commandes:\n"
                           "- Menu Fichier > Quitter\n"
                           "- Menu Animation > Demarrer / Arreter\n"
                           "- Sliders: reglage des parametres geometriques\n\n"
                           "Raccourcis clavier:\n"
                           "- Espace : demarrer / arreter l'animation\n"
                           "- R : reinitialiser a la frame 0\n"
                           "- Fleche droite : frame suivante\n"
                           "- Fleche gauche : frame precedente");
        statusBar()->showMessage("Affichage de la boite A propos", 2000);
    });

    QShortcut *shortcutToggle = new QShortcut(QKeySequence(Qt::Key_Space), this);
    shortcutToggle->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutToggle, &QShortcut::activated,
                     viewer, &Barres::toggleAnimation);

    QShortcut *shortcutReset = new QShortcut(QKeySequence(Qt::Key_R), this);
    shortcutReset->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutReset, &QShortcut::activated, this, [this]() {
        viewer->resetAnimation();
        statusBar()->showMessage("Animation reinitialisee", 1500);
    });

    QShortcut *shortcutStepPrev = new QShortcut(QKeySequence(Qt::Key_Left), this);
    shortcutStepPrev->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutStepPrev, &QShortcut::activated, this, [this]() {
        viewer->stepBackward();
        statusBar()->showMessage("Frame precedente", 1000);
    });

    QShortcut *shortcutStepNext = new QShortcut(QKeySequence(Qt::Key_Right), this);
    shortcutStepNext->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutStepNext, &QShortcut::activated, this, [this]() {
        viewer->stepForward();
        statusBar()->showMessage("Frame suivante", 1000);
    });

    statusBar()->showMessage("Pret");
}
