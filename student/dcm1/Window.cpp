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
#include <QActionGroup>
#include <QApplication>
#include <QCloseEvent>
#include <QColorDialog>
#include <QDateTime>
#include <QDialog>
#include <QDialogButtonBox>
#include <QDir>
#include <QDragEnterEvent>
#include <QDoubleSpinBox>
#include <QDropEvent>
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
#include <QMimeData>
#include <QMessageBox>
#include <QPushButton>
#include <QSettings>
#include <QShortcut>
#include <QSlider>
#include <QSpinBox>
#include <QStatusBar>
#include <QStandardPaths>
#include <QTabWidget>
#include <QUrl>
#include <QVBoxLayout>

namespace
{
int toSliderValue(double value, double minValue, double maxValue)
{
    const double ratio = (value - minValue) / (maxValue - minValue);
    int raw = static_cast<int>(std::lround(ratio * 100.0));
    if (raw < 0)
        raw = 0;
    if (raw > 100)
        raw = 100;
    return raw;
}
} // namespace

Window::Window(QWidget *parent) : QMainWindow(parent)
{
    this->setWindowTitle(tr("Barres"));
    this->setMinimumSize(900, 580);

    {
        QSettings settings;
        const QSize defaultSize(1000, 650);
        QSize savedSize = settings.value("window/size", defaultSize).toSize();
        if (!savedSize.isValid())
            savedSize = defaultSize;
        if (savedSize.width() < minimumWidth())
            savedSize.setWidth(minimumWidth());
        if (savedSize.height() < minimumHeight())
            savedSize.setHeight(minimumHeight());
        this->resize(savedSize);
    }

    QWidget *central = new QWidget(this);
    setCentralWidget(central);
    setAcceptDrops(true);

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

    QGroupBox *groupBox = new QGroupBox(tr("Parameters"));
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

    sliderA1 = addSlider("a1", tr("Longueur AD (manivelle)"), 0.1, 2.0,
                         &Barres::set_a1_slot);
    sliderA2 = addSlider("a2", tr("Longueur DC (bielle)"), 2.5, 4.5,
                         &Barres::set_a2_slot);
    sliderA3 = addSlider("a3", tr("Longueur BC (balancier)"), 1.0, 3.0,
                         &Barres::set_a3_slot);
    sliderXb = addSlider("xb", tr("Position x du pivot B"), 2.0, 4.0,
                         &Barres::set_xb_slot);
    sliderYa = addSlider("ya", tr("Position y du pivot A"), 0.5, 2.5,
                         &Barres::set_ya_slot);
    sliderL = addSlider("L", tr("Longueur DP' (point outil)"), 4.0, 8.0,
                        &Barres::set_L_slot);
    sliderE = addSlider("e", tr("Offset vertical du film"), 0.0, 3.0,
                        &Barres::set_e_slot);
    sliderDp = addSlider("dp", tr("Distance P' -> P"), 0.5, 1.5,
                         &Barres::set_dp_slot);

    vbox->addWidget(groupBox);

    QGroupBox *animGroup = new QGroupBox(tr("Animation"));
    QFormLayout *animLayout = new QFormLayout();
    animGroup->setLayout(animLayout);

    QSpinBox *speedMsSpin = new QSpinBox();
    speedMsSpin->setRange(5, 200);
    speedMsSpin->setSingleStep(5);
    speedMsSpin->setSuffix(tr(" ms/frame"));
    speedMsSpin->setToolTip(tr("Intervalle entre deux frames d'animation"));
    speedMsSpin->setValue(viewer->currentAnimationIntervalMs());
    animLayout->addRow(tr("Vitesse"), speedMsSpin);

    QObject::connect(speedMsSpin, QOverload<int>::of(&QSpinBox::valueChanged),
                     viewer, &Barres::setAnimationIntervalMs);
    QObject::connect(speedMsSpin, QOverload<int>::of(&QSpinBox::valueChanged),
                     this, [this](int intervalMs) {
                         statusBar()->showMessage(
                             tr("Vitesse animation: %1 ms/frame")
                                 .arg(intervalMs),
                             1200);
                     });

    vbox->addWidget(animGroup);
    vbox->addStretch(1);

    // -- menu bar
    QMenu *menuFile = menuBar()->addMenu(tr("&Fichier"));

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

    QAction *actionImport = menuFile->addAction(tr("&Importer parametres..."));
    actionImport->setShortcut(QKeySequence::Open);
    QObject::connect(actionImport, &QAction::triggered, this,
                     [this, defaultParamsDir, rememberParamsDir]() {
        const QString fileName = QFileDialog::getOpenFileName(
            this, tr("Importer les parametres"), defaultParamsDir(),
            tr("JSON files (*.json)"));
        if (fileName.isEmpty())
            return;

        if (!importParametersFromJsonFile(fileName))
            return;

        rememberParamsDir(fileName);
    });

    QAction *actionExport = menuFile->addAction(tr("&Exporter parametres..."));
    actionExport->setShortcut(QKeySequence::Save);
    QObject::connect(actionExport, &QAction::triggered, this,
                     [this, defaultParamsDir, rememberParamsDir]() {
        const QString defaultFile =
            QDir(defaultParamsDir()).filePath("barres-params.json");
        const QString fileName = QFileDialog::getSaveFileName(
            this, tr("Exporter les parametres"), defaultFile,
            tr("JSON files (*.json)"));
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
            QMessageBox::warning(this, tr("Export impossible"),
                                 tr("Impossible d'ouvrir le fichier pour ecriture."));
            statusBar()->showMessage(tr("Echec de l'export JSON"), 2000);
            return;
        }

        const QByteArray payload = QJsonDocument(root).toJson(QJsonDocument::Indented);
        const qint64 written = file.write(payload);
        if (written != payload.size())
        {
            QMessageBox::warning(this, tr("Export incomplet"),
                                 tr("Le fichier n'a pas ete ecrit completement."));
            statusBar()->showMessage(tr("Export JSON incomplet"), 2000);
            return;
        }

        statusBar()->showMessage(tr("Parametres exportes en JSON"), 2000);
    });

    menuFile->addSeparator();
    QAction *actionQuit = menuFile->addAction(tr("&Quitter"));
    actionQuit->setShortcut(QKeySequence::Quit);
    QObject::connect(actionQuit, &QAction::triggered,
                     qApp, &QApplication::quit);

    auto openRenderStyleDialog = [this]() {
        QDialog dialog(this);
        dialog.setWindowTitle(tr("Parametres de dessin"));
        dialog.setMinimumWidth(460);

        QVBoxLayout *rootLayout = new QVBoxLayout(&dialog);
        QTabWidget *tabs = new QTabWidget(&dialog);
        rootLayout->addWidget(tabs);

        const RenderStyleSettings current = viewer->currentRenderStyle();

        QWidget *tabLines = new QWidget(&dialog);
        QFormLayout *linesLayout = new QFormLayout(tabLines);
        tabs->addTab(tabLines, tr("Traits"));

        auto makeDouble = [](double minV, double maxV, double value,
                             int decimals = 2) {
            QDoubleSpinBox *box = new QDoubleSpinBox();
            box->setRange(minV, maxV);
            box->setDecimals(decimals);
            box->setValue(value);
            return box;
        };

        auto makeInt = [](int minV, int maxV, int value) {
            QSpinBox *box = new QSpinBox();
            box->setRange(minV, maxV);
            box->setValue(value);
            return box;
        };

        QDoubleSpinBox *sbLinkPenWidth =
            makeDouble(0.1, 10.0, current.linkPenWidth, 2);
        QDoubleSpinBox *sbTrajPWidth =
            makeDouble(0.1, 10.0, current.trajectoryPWidth, 2);
        QDoubleSpinBox *sbTrajDWidth =
            makeDouble(0.1, 10.0, current.trajectoryDWidth, 2);
        QDoubleSpinBox *sbGroundHalfLen =
            makeDouble(0.0, 10.0, current.groundHalfLen, 2);
        QDoubleSpinBox *sbFilmExtension =
            makeDouble(0.0, 50.0, current.filmExtension, 2);

        linesLayout->addRow(tr("Epaisseur liens"), sbLinkPenWidth);
        linesLayout->addRow(tr("Epaisseur trajectoire P"), sbTrajPWidth);
        linesLayout->addRow(tr("Epaisseur trajectoire D"), sbTrajDWidth);
        linesLayout->addRow(tr("Demi-longueur sol"), sbGroundHalfLen);
        linesLayout->addRow(tr("Longueur film"), sbFilmExtension);

        QWidget *tabLabels = new QWidget(&dialog);
        QFormLayout *labelsLayout = new QFormLayout(tabLabels);
        tabs->addTab(tabLabels, tr("Labels"));

        QSpinBox *sbLabelFontSize =
            makeInt(6, 48, current.labelFontSize);
        QSpinBox *sbLabelOffsetX =
            makeInt(-50, 50, current.labelOffsetX);
        QSpinBox *sbLabelOffsetY =
            makeInt(-50, 50, current.labelOffsetY);

        labelsLayout->addRow(tr("Taille police"), sbLabelFontSize);
        labelsLayout->addRow(tr("Offset X"), sbLabelOffsetX);
        labelsLayout->addRow(tr("Offset Y"), sbLabelOffsetY);

        QWidget *tabColors = new QWidget(&dialog);
        QFormLayout *colorsLayout = new QFormLayout(tabColors);
        tabs->addTab(tabColors, tr("Couleurs"));

        QColor mainColor = current.mainColor;
        QColor trajectoryPColor = current.trajectoryPColor;
        QColor trajectoryDColor = current.trajectoryDColor;

        auto makeColorButton = [this]() {
            QPushButton *button = new QPushButton(tr("Choisir..."));
            button->setMinimumWidth(120);
            return button;
        };

        auto updateColorButton = [](QPushButton *button, const QColor &color) {
            button->setText(color.name(QColor::HexRgb));
            button->setStyleSheet(QString(
                                      "QPushButton { background-color: %1; color: %2; }")
                                      .arg(color.name(QColor::HexRgb))
                                      .arg(color.lightness() < 128 ? "white" : "black"));
        };

        auto wireColorButton = [this, &dialog, &updateColorButton](QPushButton *button,
                                                                   QColor *target,
                                                                   const QString &title) {
            updateColorButton(button, *target);
            QObject::connect(button, &QPushButton::clicked, this,
                             [&dialog, button, target, title, &updateColorButton]() {
                                 const QColor chosen = QColorDialog::getColor(
                                     *target, &dialog, title,
                                     QColorDialog::ShowAlphaChannel);
                                 if (!chosen.isValid())
                                     return;

                                 *target = chosen;
                                 updateColorButton(button, *target);
                             });
        };

        QPushButton *btnMainColor = makeColorButton();
        QPushButton *btnTrajectoryPColor = makeColorButton();
        QPushButton *btnTrajectoryDColor = makeColorButton();

        wireColorButton(btnMainColor, &mainColor, tr("Couleur principale"));
        wireColorButton(btnTrajectoryPColor, &trajectoryPColor,
                tr("Couleur trajectoire P"));
        wireColorButton(btnTrajectoryDColor, &trajectoryDColor,
                tr("Couleur trajectoire D"));

        colorsLayout->addRow(tr("Couleur principale"), btnMainColor);
        colorsLayout->addRow(tr("Trajectoire P"), btnTrajectoryPColor);
        colorsLayout->addRow(tr("Trajectoire D"), btnTrajectoryDColor);

        auto collectStyle = [&]() {
            RenderStyleSettings s;
            s.linkPenWidth = sbLinkPenWidth->value();
            s.trajectoryPWidth = sbTrajPWidth->value();
            s.trajectoryDWidth = sbTrajDWidth->value();
            s.groundHalfLen = sbGroundHalfLen->value();
            s.filmExtension = sbFilmExtension->value();
            s.labelFontSize = sbLabelFontSize->value();
            s.labelOffsetX = sbLabelOffsetX->value();
            s.labelOffsetY = sbLabelOffsetY->value();
            s.mainColor = mainColor;
            s.trajectoryPColor = trajectoryPColor;
            s.trajectoryDColor = trajectoryDColor;
            return s;
        };

        QDialogButtonBox *buttonBox =
            new QDialogButtonBox(QDialogButtonBox::Ok |
                                 QDialogButtonBox::Cancel |
                                 QDialogButtonBox::Apply,
                                 &dialog);
        QPushButton *resetButton =
            buttonBox->addButton(tr("Reinitialiser"), QDialogButtonBox::ResetRole);
        rootLayout->addWidget(buttonBox);

        auto applyStyle = [&]() {
            viewer->applyRenderStyle(collectStyle());
            statusBar()->showMessage(tr("Style de dessin applique"), 1500);
        };

        QObject::connect(buttonBox->button(QDialogButtonBox::Apply),
                         &QPushButton::clicked, &dialog, applyStyle);
        QObject::connect(buttonBox, &QDialogButtonBox::accepted,
                         &dialog, [&]() {
                             applyStyle();
                             dialog.accept();
                         });
        QObject::connect(buttonBox, &QDialogButtonBox::rejected,
                         &dialog, &QDialog::reject);

        QObject::connect(resetButton, &QPushButton::clicked, &dialog, [&]() {
            const RenderStyleSettings defaults;
            sbLinkPenWidth->setValue(defaults.linkPenWidth);
            sbTrajPWidth->setValue(defaults.trajectoryPWidth);
            sbTrajDWidth->setValue(defaults.trajectoryDWidth);
            sbGroundHalfLen->setValue(defaults.groundHalfLen);
            sbFilmExtension->setValue(defaults.filmExtension);
            sbLabelFontSize->setValue(defaults.labelFontSize);
            sbLabelOffsetX->setValue(defaults.labelOffsetX);
            sbLabelOffsetY->setValue(defaults.labelOffsetY);
            mainColor = defaults.mainColor;
            trajectoryPColor = defaults.trajectoryPColor;
            trajectoryDColor = defaults.trajectoryDColor;
            updateColorButton(btnMainColor, mainColor);
            updateColorButton(btnTrajectoryPColor, trajectoryPColor);
            updateColorButton(btnTrajectoryDColor, trajectoryDColor);
        });

        dialog.exec();
    };

    QMenu *menuView = menuBar()->addMenu(tr("&Affichage"));
    QAction *actionResetView = menuView->addAction(tr("Réinitialiser la vue"));
    actionResetView->setShortcut(Qt::Key_H);
    QObject::connect(actionResetView, &QAction::triggered,
                     viewer, &Barres::resetView);
    menuView->addSeparator();
    QAction *actionRenderStyle =
        menuView->addAction(tr("Parametres de dessin..."));
    QObject::connect(actionRenderStyle, &QAction::triggered,
                     this, openRenderStyleDialog);

    menuView->addSeparator();
    QMenu *menuLanguage = menuView->addMenu(tr("Langue"));
    QActionGroup *languageGroup = new QActionGroup(this);
    languageGroup->setExclusive(true);

    QAction *actionLangFr = menuLanguage->addAction(tr("Francais"));
    actionLangFr->setCheckable(true);
    languageGroup->addAction(actionLangFr);

    QAction *actionLangEn = menuLanguage->addAction(tr("Anglais"));
    actionLangEn->setCheckable(true);
    languageGroup->addAction(actionLangEn);

    const QString currentLanguage =
        QSettings().value("ui/language", "fr").toString().toLower();
    if (currentLanguage == "en")
        actionLangEn->setChecked(true);
    else
        actionLangFr->setChecked(true);

    auto setLanguage = [this](const QString &languageCode) {
        QSettings settings;
        const QString oldLanguage =
            settings.value("ui/language", "fr").toString().toLower();
        if (oldLanguage == languageCode)
            return;

        settings.setValue("ui/language", languageCode);
        QMessageBox::information(this, tr("Langue"),
                                 tr("La langue sera appliquee au prochain demarrage."));
        statusBar()->showMessage(tr("Preference de langue sauvegardee"), 2000);
    };

    QObject::connect(actionLangFr, &QAction::triggered, this,
                     [setLanguage]() { setLanguage("fr"); });
    QObject::connect(actionLangEn, &QAction::triggered, this,
                     [setLanguage]() { setLanguage("en"); });

    QMenu *menuAnimation = menuBar()->addMenu(tr("&Animation"));
    actionStart = menuAnimation->addAction(tr("&Démarrer"));
    actionStop  = menuAnimation->addAction(tr("&Arrêter"));
    actionStart->setEnabled(true);
    actionStop->setEnabled(false);

    QObject::connect(actionStart, &QAction::triggered,
                     viewer, &Barres::startAnimation);
    QObject::connect(actionStop, &QAction::triggered,
                     viewer, &Barres::stopAnimation);
    QObject::connect(viewer, &Barres::animationStarted, this, [this]() {
        actionStart->setEnabled(false);
        actionStop->setEnabled(true);
        statusBar()->showMessage(tr("Animation demarree"));
    });
    QObject::connect(viewer, &Barres::animationStopped, this, [this]() {
        actionStart->setEnabled(true);
        actionStop->setEnabled(false);
        statusBar()->showMessage(tr("Animation arretee"));
    });

    QMenu *menuHelp = menuBar()->addMenu(tr("&Aide"));
    QAction *actionAbout = menuHelp->addAction(tr("&A propos"));
    QObject::connect(actionAbout, &QAction::triggered, this, [this]() {
        QMessageBox::about(this, tr("A propos"),
                           tr("Barres\n"
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
                              "- Fleche gauche : frame precedente"));
        statusBar()->showMessage(tr("Affichage de la boite A propos"), 2000);
    });

    QShortcut *shortcutToggle = new QShortcut(QKeySequence(Qt::Key_Space), this);
    shortcutToggle->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutToggle, &QShortcut::activated,
                     viewer, &Barres::toggleAnimation);

    QShortcut *shortcutReset = new QShortcut(QKeySequence(Qt::Key_R), this);
    shortcutReset->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutReset, &QShortcut::activated, this, [this]() {
        viewer->resetAnimation();
        statusBar()->showMessage(tr("Animation reinitialisee"), 1500);
    });

    QShortcut *shortcutStepPrev = new QShortcut(QKeySequence(Qt::Key_Left), this);
    shortcutStepPrev->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutStepPrev, &QShortcut::activated, this, [this]() {
        viewer->stepBackward();
        statusBar()->showMessage(tr("Frame precedente"), 1000);
    });

    QShortcut *shortcutStepNext = new QShortcut(QKeySequence(Qt::Key_Right), this);
    shortcutStepNext->setContext(Qt::ApplicationShortcut);
    QObject::connect(shortcutStepNext, &QShortcut::activated, this, [this]() {
        viewer->stepForward();
        statusBar()->showMessage(tr("Frame suivante"), 1000);
    });

    statusBar()->showMessage(tr("Pret"));
}

void
Window::closeEvent(QCloseEvent *event)
{
    QSettings settings;
    settings.setValue("window/size", size());
    QMainWindow::closeEvent(event);
}

void
Window::dragEnterEvent(QDragEnterEvent *event)
{
    const QMimeData *mimeData = event->mimeData();
    if (!mimeData || !mimeData->hasUrls())
        return;

    const QList<QUrl> urls = mimeData->urls();
    for (const QUrl &url : urls)
    {
        if (!url.isLocalFile())
            continue;

        const QString localFile = url.toLocalFile();
        if (QFileInfo(localFile).suffix().compare("json", Qt::CaseInsensitive) == 0)
        {
            event->acceptProposedAction();
            return;
        }
    }
}

void
Window::dropEvent(QDropEvent *event)
{
    const QMimeData *mimeData = event->mimeData();
    if (!mimeData || !mimeData->hasUrls())
        return;

    const QList<QUrl> urls = mimeData->urls();
    for (const QUrl &url : urls)
    {
        if (!url.isLocalFile())
            continue;

        const QString localFile = url.toLocalFile();
        if (QFileInfo(localFile).suffix().compare("json", Qt::CaseInsensitive) != 0)
            continue;

        if (importParametersFromJsonFile(localFile))
        {
            QSettings settings;
            settings.setValue("io/lastParamsDir", QFileInfo(localFile).absolutePath());
            event->acceptProposedAction();
        }
        return;
    }

    statusBar()->showMessage(tr("Deposer un fichier .json valide"), 2000);
}

bool
Window::importParametersFromJsonFile(const QString &fileName)
{
    QFile file(fileName);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        QMessageBox::warning(this, tr("Import impossible"),
                             tr("Impossible d'ouvrir le fichier en lecture."));
        statusBar()->showMessage(tr("Echec de l'import JSON"), 2000);
        return false;
    }

    const QByteArray payload = file.readAll();
    QJsonParseError parseError;
    const QJsonDocument doc = QJsonDocument::fromJson(payload, &parseError);
    if (parseError.error != QJsonParseError::NoError || !doc.isObject())
    {
        QMessageBox::warning(this, tr("Format invalide"),
                             tr("Le fichier JSON est invalide."));
        statusBar()->showMessage(tr("JSON invalide"), 2000);
        return false;
    }

    const QJsonObject root = doc.object();
    if (root.value("format").toString() != "barres-params")
    {
        QMessageBox::warning(this, tr("Format non reconnu"),
                             tr("Le fichier n'est pas un export Barres."));
        statusBar()->showMessage(tr("Format JSON non reconnu"), 2000);
        return false;
    }

    if (root.value("version").toInt(-1) != 1)
    {
        QMessageBox::warning(this, tr("Version non supportee"),
                             tr("Version de fichier non supportee."));
        statusBar()->showMessage(tr("Version JSON non supportee"), 2000);
        return false;
    }

    const QJsonValue paramsValue = root.value("params");
    if (!paramsValue.isObject())
    {
        QMessageBox::warning(this, tr("Fichier incomplet"),
                             tr("Objet 'params' manquant."));
        statusBar()->showMessage(tr("Objet params manquant"), 2000);
        return false;
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
        QMessageBox::warning(this, tr("Parametres invalides"),
                             tr("Un ou plusieurs parametres sont manquants ou hors bornes."));
        statusBar()->showMessage(tr("Parametres JSON invalides"), 2000);
        return false;
    }

    viewer->applyParameters(p);
    syncSlidersFromParameters(p);
    statusBar()->showMessage(tr("Parametres importes depuis JSON"), 2000);
    return true;
}

void
Window::syncSlidersFromParameters(const MechanismParameters &p)
{
    sliderA1->setValue(toSliderValue(p.a1, 0.1, 2.0));
    sliderA2->setValue(toSliderValue(p.a2, 2.5, 4.5));
    sliderA3->setValue(toSliderValue(p.a3, 1.0, 3.0));
    sliderXb->setValue(toSliderValue(p.xb, 2.0, 4.0));
    sliderYa->setValue(toSliderValue(p.ya, 0.5, 2.5));
    sliderL->setValue(toSliderValue(p.L, 4.0, 8.0));
    sliderE->setValue(toSliderValue(p.e, 0.0, 3.0));
    sliderDp->setValue(toSliderValue(p.dp, 0.5, 1.5));
}
