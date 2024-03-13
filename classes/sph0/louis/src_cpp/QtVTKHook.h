#ifndef QTVTKHOOK_H
#define QTVTKHOOK_H

#include "sph.h"
#include "DisplayHook.h"
#include <QWidget>
#include <QApplication>
#include <QVTKOpenGLNativeWidget.h>
#include <vtkSmartPointer.h>
#include <vtkRenderer.h>

/// this class manages the VTK widget (window) for 3D display.
/// TODO: this should be converted to a MainWindow class later

class DisplayWindow : public QWidget
{
    Q_OBJECT;

    QVTKOpenGLNativeWidget *vtkwidget;
    vtkSmartPointer<vtkRenderer> renderer;

public:
    DisplayWindow(QWidget *parent = nullptr);
    ~DisplayWindow();

private:
    void setupGUI();
    void addCube();
};

// -----------------------------------------------------------------------------

/// this class manages the Qt application.

class QtVTKHook : public DisplayHook
{
    QApplication *app;
    DisplayWindow *window;
    Model &model;

public:
    QtVTKHook(int &argc, char **argv, Model &model);
    virtual ~QtVTKHook();
    virtual void display() override;

    virtual void loop() override;

    static void standalone_VTK_demo(); // for testing
};

#endif // QTVTKHOOK_H
