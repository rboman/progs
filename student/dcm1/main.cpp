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
#include <QApplication>
#include <QCoreApplication>
#include <QDir>
#include <QSettings>
#include <QTranslator>

int
main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QCoreApplication::setOrganizationName("rboman");
    QCoreApplication::setApplicationName("barres");

    // i18n step 1: define preferred UI language setting, not yet applied.
    QSettings settings;
    if (!settings.contains("ui/language"))
        settings.setValue("ui/language", "fr");

    const QString uiLanguage =
        settings.value("ui/language", "fr").toString().toLower();
    if (uiLanguage == "en")
    {
        QTranslator *translator = new QTranslator(&app);
        const QString qmInI18nDir =
            QDir(QCoreApplication::applicationDirPath()).filePath("i18n/barres_en.qm");
        const QString qmNextToExe =
            QDir(QCoreApplication::applicationDirPath()).filePath("barres_en.qm");

        if (translator->load(qmInI18nDir) || translator->load(qmNextToExe))
            app.installTranslator(translator);
    }

    Window win;
    win.show();
    return app.exec();
}
