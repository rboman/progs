//   Copyright 2003-2017 Romain Boman
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

#include "mywidgeti.h"

#include <qlineedit.h>

MyWidgetI::MyWidgetI(QWidget* parent, 
                     const char* name, 
                     WFlags fl) : MyWidget(parent, name, fl)

{
    update();
}

MyWidgetI::~MyWidgetI()
{

}

void MyWidgetI::update()
{

    updateTextLineEdit(OrigXLineEdit,   tpar.getOrigin().x);
    updateTextLineEdit(OrigYLineEdit,   tpar.getOrigin().y);
    updateTextLineEdit(DimXLineEdit,    tpar.getDimension().x);
    updateTextLineEdit(DimYLineEdit,    tpar.getDimension().y);
    updateTextLineEdit(NoXLineEdit,     tpar.getNumberOfElementOnX());
    updateTextLineEdit(NbMLineEdit,     tpar.getNumberOfElementOnY());
    updateTextLineEdit(CoefLineEdit,    tpar.getReductionCoefficient());

    updateTextLineEdit(CentreXLineEdit, mpar.centre.x);
    updateTextLineEdit(CentreYLineEdit, mpar.centre.y);
    updateTextLineEdit(RayonLineEdit,   mpar.radius);
    updateTextLineEdit(ALineEdit,       mpar.initialAngle);
    updateTextLineEdit(AspALineEdit,    mpar.asperityAngle);
    updateTextLineEdit(AspBaseLineEdit, mpar.asperityLength);
    updateTextLineEdit(AspRLineEdit,    mpar.smoothnessAngle);
    updateTextLineEdit(AspIntLineEdit,  mpar.asperityInterval);
    updateTextLineEdit(AspNLineEdit,    mpar.numberOfAsperities);

}

int 
MyWidgetI::integerLineEdit(QLineEdit *ledit)
{
    int val=0;
    bool ok;
    int test = ledit->text().toInt(&ok);
    if(ok) val = test;
    updateTextLineEdit(ledit,val);
    return val;
}

double 
MyWidgetI::floatLineEdit(QLineEdit *ledit)
{
    double val;
    bool ok;
    double test = ledit->text().toFloat(&ok);
    if(ok) val = test;
    updateTextLineEdit(ledit,val);
    return val;
}

void MyWidgetI::updateTextLineEdit(QLineEdit *ledit, double val)
{
    QString s; 
    s.setNum( val ); ledit->setText(s);
}

// -- param�tres maillage

void MyWidgetI::origx_slot()
{
    tpar.setOriginX(floatLineEdit(OrigXLineEdit));
}
void MyWidgetI::origy_slot()
{
    tpar.setOriginY(floatLineEdit(OrigYLineEdit));
}
void MyWidgetI::dimx_slot()
{
    tpar.setDimensionX(floatLineEdit(DimXLineEdit));
}
void MyWidgetI::dimy_slot()
{
    tpar.setDimensionY(floatLineEdit(DimYLineEdit));
}
void MyWidgetI::nox_slot()
{
    tpar.setNumberOfElementOnX(integerLineEdit(NoXLineEdit));
}
void MyWidgetI::nbm_slot()
{
    tpar.setNumberOfElementOnY(integerLineEdit(NbMLineEdit));
}
void MyWidgetI::coef_slot()
{
    tpar.setReductionCoefficient(floatLineEdit(CoefLineEdit));
}

void MyWidgetI::addtype_slot()
{
    qWarning( "MyWidget::addtype_slot(): Not implemented yet!" );
}
void MyWidgetI::deltype_slot()
{
    qWarning( "MyWidget::deltype_slot(): Not implemented yet!" );
}

// -- param�tres matrice

void MyWidgetI::centrex_slot()
{
    mpar.centre.x = floatLineEdit(CentreXLineEdit);
}
void MyWidgetI::centrey_slot()
{
    mpar.centre.y = floatLineEdit(CentreYLineEdit);
}
void MyWidgetI::rayon_slot()
{
    mpar.radius = floatLineEdit(RayonLineEdit);
}
void MyWidgetI::a_slot()
{
    mpar.initialAngle = floatLineEdit(ALineEdit);
}
void MyWidgetI::aspa_slot()
{
    mpar.asperityAngle = floatLineEdit(AspALineEdit);
}
void MyWidgetI::aspbase_slot()
{
    mpar.asperityLength = floatLineEdit(AspBaseLineEdit);
}
void MyWidgetI::aspr_slot()
{
    mpar.smoothnessAngle = floatLineEdit(AspRLineEdit);
}
void MyWidgetI::aspint_slot()
{
    mpar.asperityInterval = floatLineEdit(AspIntLineEdit);
}
void MyWidgetI::aspn_slot()
{
    mpar.numberOfAsperities = integerLineEdit(AspNLineEdit);
}

// mesh - load/save

void MyWidgetI::tload_slot()
{
    tpar.load("mesh.txt");
    update();
}
void MyWidgetI::tsave_slot()
{
    tpar.save("mesh.txt");
    update();
}

// matrix - load/save

void MyWidgetI::mload_slot()
{
    mpar.load("matrix.txt");
    update();
}
void MyWidgetI::msave_slot()
{
    mpar.save("matrix.txt");
    update();
}

// export

void MyWidgetI::tobacon_slot()
{
    Tool        matrix;
    ToolBuilder mbuilder(matrix);
    Mesh   mesh;
    MeshBuilder mesher(mesh);

    mesher.setParameters(tpar);
    mesher.genere();
    mbuilder.setParameters(mpar);
    mbuilder.genere();

    NodeRenumberer rnb(mesh); 
    rnb.setStyle(BACONSTYLE);rnb.execute();

    BaconMeshExporter writer1(mesh);
    writer1.save();
    BaconToolExporter writer2(matrix);
    writer2.save();
    BaconDatToolExporter writer2b(matrix);
    writer2b.save();
}

void MyWidgetI::tomatlab_slot()
{
    Tool        matrix;
    ToolBuilder mbuilder(matrix);
    Mesh   mesh;
    MeshBuilder mesher(mesh);

    mesher.setParameters(tpar);
    mesher.genere(); 
    mbuilder.setParameters(mpar);
    mbuilder.genere();

    NodeRenumberer rnb(mesh); 
    rnb.setStyle(NORMALSTYLE);rnb.execute();

    MatlabMeshExporter writer1(mesh);
    writer1.save();
    MatlabToolExporter writer2(matrix);
    writer2.save();
}

void MyWidgetI::tooofelie_slot()
{
    Tool        matrix;
    ToolBuilder mbuilder(matrix);
    Mesh   mesh;
    MeshBuilder mesher(mesh);

    mesher.setParameters(tpar);
    mesher.genere(); 
    mbuilder.setParameters(mpar);
    mbuilder.genere();

    NodeRenumberer rnb(mesh); 
    rnb.setStyle(NORMALSTYLE);rnb.execute();

    OofelieMeshExporter writer1(mesh);
    writer1.save();
    OofelieToolExporter writer2(matrix);
    writer2.save();
}

