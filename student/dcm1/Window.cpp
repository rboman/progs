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
#include <cmath>
#include <QAction>
#include <QApplication>
#include <QDateTime>
#include <QDir>
#include <QFile>
#include <QFileDialog>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QFrame>
#include <QFormLayout>
#include <QGroupBox>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonParseError>
#include <QLabel>
#include <QMenuBar>
#include <QMessageBox>
#include <QSettings>
#include <QShortcut>
#include <QSlider>
#include <QStatusBar>
#include <QStandardPaths>
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

        return slider;
    };

    auto toSliderValue = [](double value, double minValue, double maxValue) {
        const double ratio = (value - minValue) / (maxValue - minValue);
        int raw = static_cast<int>(std::lround(ratio * 100.0));
        if (raw < 0)
            raw = 0;
        if (raw > 100)
            raw = 100;
        return raw;
    };

    QSlider *sliderA1 = addSlider("a1", "Longueur AD (manivelle)", 0.1, 2.0,
                                  &Barres::set_a1_slot);
    QSlider *sliderA2 = addSlider("a2", "Longueur DC (bielle)", 2.5, 4.5,
                                  &Barres::set_a2_slot);
    QSlider *sliderA3 = addSlider("a3", "Longueur BC (balancier)", 1.0, 3.0,
                                  &Barres::set_a3_slot);
    QSlider *sliderXb = addSlider("xb", "Position x du pivot B", 2.0, 4.0,
                                  &Barres::set_xb_slot);
    QSlider *sliderYa = addSlider("ya", "Position y du pivot A", 0.5, 2.5,
                                  &Barres::set_ya_slot);
    QSlider *sliderL = addSlider("L", "Longueur DP' (point outil)", 4.0, 8.0,
                                 &Barres::set_L_slot);
    QSlider *sliderE = addSlider("e", "Offset vertical du film", 0.0, 3.0,
                                 &Barres::set_e_slot);
    QSlider *sliderDp = addSlider("dp", "Distance P' -> P", 0.5, 1.5,
                                  &Barres::set_dp_slot);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);

    // -- menu bar
    QMenu *menuFile = menuBar()->addMenu("&Fichier");

    auto defaultParamsDir = []() {
        QSettings settings;
        const QString lastDir = settings.value("io/lastParamsDir").toString();
        if (!lastDir.isEmpty() && QDir(lastDir).exists())
            return lastDir;

        const QString docs =
            QStandardPaths::writableLocation(QStandardPaths::DocumentsLocation);
        if (!docs.isEmpty() && QDir(docs).exists())
            return docs;

        return QDir::homePath();
    };

    auto rememberParamsDir = [](const QString &fileName) {
        QSettings settings;
        settings.setValue("io/lastParamsDir", QFileInfo(fileName).absolutePath());
    };

    QAction *actionImport = menuFile->addAction("&Importer parametres...");
    actionImport->setShortcut(QKeySequence::Open);
    QObject::connect(actionImport, &QAction::triggered, this,
                     [this, sliderA1, sliderA2, sliderA3, sliderXb,
                      sliderYa, sliderL, sliderE, sliderDp, toSliderValue,
                      defaultParamsDir, rememberParamsDir]() {
        const QString fileName = QFileDialog::getOpenFileName(
            this, "Importer les parametres", defaultParamsDir(),
            "JSON files (*.json)");
        if (fileName.isEmpty())
            return;

        rememberParamsDir(fileName);

        QFile file(fileName);
        if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
        {
            QMessageBox::warning(this, "Import impossible",
                                 "Impossible d'ouvrir le fichier en lecture.");
            statusBar()->showMessage("Echec de l'import JSON", 2000);
            return;
        }

        const QByteArray payload = file.readAll();
        QJsonParseError parseError;
        const QJsonDocument doc = QJsonDocument::fromJson(payload, &parseError);
        if (parseError.error != QJsonParseError::NoError || !doc.isObject())
        {
            QMessageBox::warning(this, "Format invalide",
                                 "Le fichier JSON est invalide.");
            statusBar()->showMessage("JSON invalide", 2000);
            return;
        }

        const QJsonObject root = doc.object();
        if (root.value("format").toString() != "barres-params")
        {
            QMessageBox::warning(this, "Format non reconnu",
                                 "Le fichier n'est pas un export Barres.");
            statusBar()->showMessage("Format JSON non reconnu", 2000);
            return;
        }

        if (root.value("version").toInt(-1) != 1)
        {
            QMessageBox::warning(this, "Version non supportee",
                                 "Version de fichier non supportee.");
            statusBar()->showMessage("Version JSON non supportee", 2000);
            return;
        }

        const QJsonValue paramsValue = root.value("params");
        if (!paramsValue.isObject())
        {
            QMessageBox::warning(this, "Fichier incomplet",
                                 "Objet 'params' manquant.");
            statusBar()->showMessage("Objet params manquant", 2000);
            return;
        }

        const QJsonObject paramsObj = paramsValue.toObject();
        auto readParam = [&](const char *name, double minValue, double maxValue,
                             bool &ok) {
            const QJsonValue value = paramsObj.value(name);
            if (!value.isDouble())
            {
                ok = false;
                return 0.0;
            }

            const double v = value.toDouble();
            if (!std::isfinite(v) || v < minValue || v > maxValue)
            {
                ok = false;
                return 0.0;
            }

            return v;
        };

        bool ok = true;
        MechanismParameters p;
        p.a1 = readParam("a1", 0.1, 2.0, ok);
        p.a2 = readParam("a2", 2.5, 4.5, ok);
        p.a3 = readParam("a3", 1.0, 3.0, ok);
        p.xb = readParam("xb", 2.0, 4.0, ok);
        p.ya = readParam("ya", 0.5, 2.5, ok);
        p.L = readParam("L", 4.0, 8.0, ok);
        p.e = readParam("e", 0.0, 3.0, ok);
        p.dp = readParam("dp", 0.5, 1.5, ok);

        if (!ok)
        {
            QMessageBox::warning(this, "Parametres invalides",
                                 "Un ou plusieurs parametres sont manquants ou hors bornes.");
            statusBar()->showMessage("Parametres JSON invalides", 2000);
            return;
        }

        viewer->applyParameters(p);

        // Keep UI controls in sync with imported parameters.
        sliderA1->setValue(toSliderValue(p.a1, 0.1, 2.0));
        sliderA2->setValue(toSliderValue(p.a2, 2.5, 4.5));
        sliderA3->setValue(toSliderValue(p.a3, 1.0, 3.0));
        sliderXb->setValue(toSliderValue(p.xb, 2.0, 4.0));
        sliderYa->setValue(toSliderValue(p.ya, 0.5, 2.5));
        sliderL->setValue(toSliderValue(p.L, 4.0, 8.0));
        sliderE->setValue(toSliderValue(p.e, 0.0, 3.0));
        sliderDp->setValue(toSliderValue(p.dp, 0.5, 1.5));

        statusBar()->showMessage("Parametres importes depuis JSON", 2000);
    });

    QAction *actionExport = menuFile->addAction("&Exporter parametres...");
    actionExport->setShortcut(QKeySequence::Save);
    QObject::connect(actionExport, &QAction::triggered, this,
                     [this, defaultParamsDir, rememberParamsDir]() {
        const QString defaultFile =
            QDir(defaultParamsDir()).filePath("barres-params.json");
        const QString fileName = QFileDialog::getSaveFileName(
            this, "Exporter les parametres", defaultFile,
            "JSON files (*.json)");
        if (fileName.isEmpty())
            return;

        rememberParamsDir(fileName);

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
