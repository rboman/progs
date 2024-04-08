#ifndef SPH_DISPLAYWINDOW_H
#define SPH_DISPLAYWINDOW_H

#include "sph.h"
#include "sphDisplayHook.h"
#include <QWidget>
#include <QVTKOpenGLNativeWidget.h>
#include <vtkSmartPointer.h>
#include <vtkRenderer.h>
#include <vtkPoints.h>

namespace sph
{

/// this class manages the VTK widget (window) for 3D display.
/// TODO: this should be converted to a MainWindow class later

class SPH_API DisplayWindow : public QWidget
{
    Q_OBJECT;

    Model &model;

    QVTKOpenGLNativeWidget *vtkwidget; ///< Qt widget for VTK display
    vtkSmartPointer<vtkRenderer> renderer;
    vtkSmartPointer<vtkPoints> points;

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

private:
    void setupGUI();
    void addCube();
    void addParticles();
};

}; // namespace sph

#endif // SPH_DISPLAYWINDOW_H
