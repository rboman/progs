#ifndef SPH_QTVTKHOOK_H
#define SPH_QTVTKHOOK_H

#include "sph.h"
#include "sphDisplayHook.h"
#include <QWidget>
#include <QApplication>
#include <QVTKOpenGLNativeWidget.h>
#include <vtkSmartPointer.h>
#include <vtkRenderer.h>
#include <vtkPoints.h>

// #ifndef sph_EXPORTS
// #error "sph_EXPORTS not defined"
// #endif

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

    void updateParticlePositions();

private:
    void setupGUI();
    void addCube();
    void addParticles();
};

}; // namespace sph

// -----------------------------------------------------------------------------

namespace sph
{

/// this class manages the Qt application.

class SPH_API QtVTKHook : public DisplayHook
{
    QApplication *app;
    DisplayWindow *window;
    Model &model;

public:
    QtVTKHook(int &argc, char **argv, Model &model);
    virtual ~QtVTKHook();
    virtual void interact() override;
    virtual void update_data() override;

    virtual void loop() override;

    static void standalone_VTK_demo(); // for testing
};

}; // namespace sph

#endif // SPH_QTVTKHOOK_H
