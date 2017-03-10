#include <QApplication>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkSmartPointer.h>
#include <vtkSphereSource.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <QVTKWidget.h>

#define VTK_NEW(type, instance) \
  vtkSmartPointer<type> instance = vtkSmartPointer<type>::New();

#ifdef Q_OS_OSX
#include "osxHelper.h"
#endif

int main(int argc, char** argv)
{
  QApplication app(argc, argv);
  QVTKWidget widget;
#ifdef Q_OS_OSX
  disableGLHiDPI(widget.winId());
#endif

  VTK_NEW(vtkSphereSource, sphereSource);
  VTK_NEW(vtkPolyDataMapper, sphereMapper);
  VTK_NEW(vtkActor, sphereActor);
  VTK_NEW(vtkRenderWindow, renderWindow);
  VTK_NEW(vtkRenderer, renderer);

  sphereMapper->SetInputConnection(sphereSource->GetOutputPort());
  sphereActor->SetMapper(sphereMapper);
  renderWindow->AddRenderer(renderer);
  renderer->SetBackground(0.1, 0.3, 0.4);
  renderer->AddActor(sphereActor);
  widget.SetRenderWindow(renderWindow);
  widget.resize(256,256);
  widget.show();

  app.exec();

  return 0;
}

