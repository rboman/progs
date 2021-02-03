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

#ifndef MYWIDGETI_H
#define MYWIDGETI_H

#include "ui_mywidget.h"

#include "Mesh.h"
#include "MeshBuilder.h"

#include "OofelieMeshExporter.h"
#include "BaconMeshExporter.h"
#include "MatlabMeshExporter.h"

#include "Tool.h"
#include "ToolBuilder.h"

#include "OofelieToolExporter.h"
#include "BaconToolExporter.h"
#include "BaconDatToolExporter.h"
#include "MatlabToolExporter.h"
#include "NodeRenumberer.h"

class MyWidgetI : public Ui::MyWidget
{
    Q_OBJECT

private:
    MeshParameters tpar;
    ToolParameters mpar;

public:
    MyWidgetI(QWidget *parent = 0, const char *name = 0, WFlags fl = 0);
    ~MyWidgetI();

private:
    void update();
    int integerLineEdit(QLineEdit *ledit);
    double floatLineEdit(QLineEdit *ledit);
    void updateTextLineEdit(QLineEdit *ledit, double val);

public slots:
    virtual void a_slot();
    virtual void addtype_slot();
    virtual void aspa_slot();
    virtual void aspbase_slot();
    virtual void aspint_slot();
    virtual void aspn_slot();
    virtual void aspr_slot();
    virtual void centrex_slot();
    virtual void centrey_slot();
    virtual void coef_slot();
    virtual void dimx_slot();
    virtual void dimy_slot();
    virtual void mload_slot();
    virtual void msave_slot();
    virtual void nbm_slot();
    virtual void deltype_slot();
    virtual void nox_slot();
    virtual void origx_slot();
    virtual void origy_slot();
    virtual void rayon_slot();
    virtual void tload_slot();
    virtual void tobacon_slot();
    virtual void tomatlab_slot();
    virtual void tooofelie_slot();
    virtual void tsave_slot();
};

#endif
