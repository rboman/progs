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
    viewer->setMinimumSize(QSize(600, 400));
    viewer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    QHBoxLayout *hbox = new QHBoxLayout();
    this->setLayout(hbox);
    hbox->addWidget(viewer);

    QFrame *pan = new QFrame();
    pan->setMaximumSize(QSize(200, 999999));
    hbox->addWidget(pan);

    QVBoxLayout *vbox = new QVBoxLayout();
    pan->setLayout(vbox);

    QGroupBox *groupBox = new QGroupBox("Parameters");
    QGridLayout *gbox = new QGridLayout();
    groupBox->setLayout(gbox);

    // a1
    QLabel *label_a1 = new QLabel("a1");
    gbox->addWidget(label_a1, 0, 0);
    QSlider *slider_a1 = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_a1, 0, 1);
    slider_a1->setRange(0, 100);
    QObject::connect(slider_a1, &QSlider::valueChanged, viewer,
                     &Barres::set_a1_slot);

    // a2
    QLabel *label_a2 = new QLabel("a2");
    gbox->addWidget(label_a2, 1, 0);
    QSlider *slider_a2 = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_a2, 1, 1);
    slider_a2->setRange(0, 100);
    QObject::connect(slider_a2, &QSlider::valueChanged, viewer,
                     &Barres::set_a2_slot);

    // a3
    QLabel *label_a3 = new QLabel("a3");
    gbox->addWidget(label_a3, 2, 0);
    QSlider *slider_a3 = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_a3, 2, 1);
    slider_a3->setRange(0, 100);
    QObject::connect(slider_a3, &QSlider::valueChanged, viewer,
                     &Barres::set_a3_slot);

    // xb
    QLabel *label_xb = new QLabel("xb");
    gbox->addWidget(label_xb, 3, 0);
    QSlider *slider_xb = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_xb, 3, 1);
    slider_xb->setRange(0, 100);
    QObject::connect(slider_xb, &QSlider::valueChanged, viewer,
                     &Barres::set_xb_slot);

    // ya
    QLabel *label_ya = new QLabel("ya");
    gbox->addWidget(label_ya, 4, 0);
    QSlider *slider_ya = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_ya, 4, 1);
    slider_ya->setRange(0, 100);
    QObject::connect(slider_ya, &QSlider::valueChanged, viewer,
                     &Barres::set_ya_slot);

    // L
    QLabel *label_L = new QLabel("L");
    gbox->addWidget(label_L, 5, 0);
    QSlider *slider_L = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_L, 5, 1);
    slider_L->setRange(0, 100);
    QObject::connect(slider_L, &QSlider::valueChanged, viewer,
                     &Barres::set_L_slot);

    // e
    QLabel *label_e = new QLabel("e");
    gbox->addWidget(label_e, 6, 0);
    QSlider *slider_e = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_e, 6, 1);
    slider_e->setRange(0, 100);
    QObject::connect(slider_e, &QSlider::valueChanged, viewer,
                     &Barres::set_e_slot);

    // dp
    QLabel *label_dp = new QLabel("dp");
    gbox->addWidget(label_dp, 7, 0);
    QSlider *slider_dp = new QSlider(Qt::Horizontal);
    gbox->addWidget(slider_dp, 7, 1);
    slider_dp->setRange(0, 100);
    QObject::connect(slider_dp, &QSlider::valueChanged, viewer,
                     &Barres::set_dp_slot);

    vbox->addWidget(groupBox);
    vbox->addStretch(1);

    // set initial values
    slider_a1->setValue(50);
    slider_a2->setValue(50);
    slider_a3->setValue(50);
    slider_xb->setValue(50);
    slider_ya->setValue(50);
    slider_L->setValue(50);
    slider_e->setValue(50);
    slider_dp->setValue(50);
}
