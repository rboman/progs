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

#include <QWidget>
#include <QColor>

class Fract : public QWidget
{
    Q_OBJECT;

    double x1;
    double y1;
    double x2;
    double y2;
    int nb_coul;

    std::vector<QColor> colours;
    
public:
    Fract(QWidget *parent = 0);

public slots:
    virtual void paintEvent(QPaintEvent *event);
};
