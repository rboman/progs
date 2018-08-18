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
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QFrame>
#include <QGroupBox>
#include <QGridLayout>
#include <QLabel>
#include <QSlider>

Window::Window(QWidget *parent) : QWidget(parent)
{
    this->setWindowTitle("Barres");
    this->resize(800, 600);

    viewer = new Barres(this);
    viewer->setMinimumSize(QSize(600, 500));
    viewer->setSizePolicy(QSizePolicy::Expanding,
                                QSizePolicy::Expanding);     


    QHBoxLayout *hbox = new QHBoxLayout();
    this->setLayout(hbox);
    hbox->addWidget(viewer);

    QFrame * pan = new QFrame();
    pan->setMaximumSize(QSize(200, 999999));
    hbox->addWidget(pan);

    QVBoxLayout * vbox = new QVBoxLayout();
    pan->setLayout(vbox);

    QGroupBox *groupBox = new QGroupBox("Parameters");
    QGridLayout *gbox = new QGridLayout();
    groupBox->setLayout(gbox);

    QLabel *label_a = new QLabel("a");
    gbox->addWidget(label_a, 0, 0);

    QSlider *slider_a = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_a, 0, 1);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);
}
