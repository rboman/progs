

#ifndef MYWIDGETI_H
#define MYWIDGETI_H

#include "mywidget.h"

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


class MyWidgetI : public MyWidget
{
    Q_OBJECT

private:
    MeshParameters  tpar;
    ToolParameters mpar;

public:
    MyWidgetI( QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
    ~MyWidgetI();

private:
    void   update();
    int    integerLineEdit(QLineEdit *ledit);
    double floatLineEdit(QLineEdit *ledit);
    void   updateTextLineEdit(QLineEdit *ledit, double val);

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
