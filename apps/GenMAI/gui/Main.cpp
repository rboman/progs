//   Copyright 2003-2019 Romain Boman
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

#include "genmai.h"
#include <qapplication.h>
#include "mywidgeti.h"

/**
 * @brief Stupid main : start the Qt app
 */

int
main(int argc, char **argv)
{
    QApplication a(argc, argv);
    MyWidgetI *mw = new MyWidgetI();
    mw->setCaption("GenMAI - by RoBo");
    mw->show();
    a.connect(&a, SIGNAL(lastWindowClosed()), &a, SLOT(quit()));

    return a.exec();
}
