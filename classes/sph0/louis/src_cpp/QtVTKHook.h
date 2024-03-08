#ifndef QTVTKHOOK_H
#define QTVTKHOOK_H

#include "sph.h"
#include "DisplayHook.h"
#include <QWidget>
#include <QApplication>
#include <QVTKOpenGLNativeWidget.h>
#include <vtkSmartPointer.h>
#include <vtkRenderer.h>

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

    void demo();
};

#endif // QTVTKHOOK_H
