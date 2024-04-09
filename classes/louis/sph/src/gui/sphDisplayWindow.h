#ifndef SPH_DISPLAYWINDOW_H
#define SPH_DISPLAYWINDOW_H

#include "sph.h"
#include "sphDisplayHook.h"

#include <QMainWindow>
#include <QVTKOpenGLNativeWidget.h>

#include <vtkSmartPointer.h>
#include <vtkRenderer.h>
#include <vtkPoints.h>
#include <vtkOrientationMarkerWidget.h>
#include <vtkScalarBarActor.h>

// https://doc.qt.io/qt-5/designer-creating-mainwindows.html
// https://doc.qt.io/qt-5/designer-using-a-ui-file.html
namespace Ui
{
class DisplayWindow;
}


namespace sph
{

/// this class manages the VTK widget (window) for 3D display.
/// TODO: this should be converted to a MainWindow class later

class SPH_API DisplayWindow : public QMainWindow
{
    Q_OBJECT;

    Model &model;

    QVTKOpenGLNativeWidget *vtkwidget; ///< Qt widget for VTK display
    
    vtkSmartPointer<vtkRenderer> renderer;

    // particles
    vtkSmartPointer<vtkPoints> fixed_points;
    vtkSmartPointer<vtkPoints> mobile_points;

    vtkSmartPointer<vtkActor> fixed_actor;
    vtkSmartPointer<vtkPolyData> mobile_polydata;
    vtkSmartPointer<vtkActor> mobile_actor;

    vtkSmartPointer<vtkScalarBarActor> scalarBar;

    // x,y,z axes widget
    vtkSmartPointer<vtkOrientationMarkerWidget> axes_marker;

    // domain boundaries
    vtkSmartPointer<vtkActor> box_actor;
    vtkSmartPointer<vtkActor> boxwf_actor;



public:
    explicit DisplayWindow(Model &model, QWidget *parent = nullptr);
    virtual ~DisplayWindow();

    // disable copy and move
    DisplayWindow(const DisplayWindow &) = delete;
    DisplayWindow(DisplayWindow &&) = delete;
    DisplayWindow &operator=(const DisplayWindow &) = delete;

#ifndef SWIG
    void updateParticlePositions();
#endif

private slots:
    void on_resetCamera_pushButton_clicked();
    void on_stop_pushButton_clicked();
    void on_pause_pushButton_clicked();
    void on_showBox_checkBox_toggled(bool checked);
    void on_showFixed_checkBox_toggled(bool checked);
    void on_fixedAlpha_slider_valueChanged(int value);

private:
    Ui::DisplayWindow *ui; ///< Qt Designer UI

    void setupGUI();
    void resetCamera();
    void addParticles();
    void addDomainBox();
    void addXYZAxes();
};

}; // namespace sph

#endif // SPH_DISPLAYWINDOW_H
