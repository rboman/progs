#include "QtVTKHook.h"
#include "Model.h"
#include <vtkCubeSource.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkGenericOpenGLRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCamera.h>
#include <vtkInteractorStyleTrackballCamera.h>
#include <QHBoxLayout>

DisplayWindow::DisplayWindow(QWidget *parent) : QWidget(parent)
{
    setWindowTitle("VTK in Qt");
    resize(800, 600);

    setupGUI();

    QObject::connect(QApplication::instance(), SIGNAL(lastWindowClosed()),
                     QApplication::instance(), SLOT(quit()));

    this->show();
    // this->vtkwidget->Initialize();
}

DisplayWindow::~DisplayWindow()
{
}

void
DisplayWindow::setupGUI()
{
    this->vtkwidget = new QVTKOpenGLNativeWidget(this);

    vtkNew<vtkGenericOpenGLRenderWindow> window;
    vtkwidget->setRenderWindow(window.Get());

    // Camera
    vtkSmartPointer<vtkCamera> camera = vtkSmartPointer<vtkCamera>::New();
    camera->SetViewUp(0, 1, 0);
    camera->SetPosition(0, 0, 10);
    camera->SetFocalPoint(0, 0, 0);

    // Renderer
    renderer = vtkSmartPointer<vtkRenderer>::New();
    renderer->SetActiveCamera(camera);
    renderer->SetBackground(0.5, 0.5, 0.5);
    vtkwidget->renderWindow()->AddRenderer(renderer);

    // renderer = vtkSmartPointer<vtkRenderer>::New();
    // renderer->SetBackground(1.0, 1.0, 1.0);
    // vtkwidget->GetRenderWindow()->AddRenderer(renderer);

    // style = vtk.vtkInteractorStyleTrackballCamera()
    // self.vtkwidget.SetInteractorStyle(style)

    // vtkSmartPointer<vtkInteractorStyleTrackballCamera> style = vtkSmartPointer<vtkInteractorStyleTrackballCamera>::New();
    // this->vtkwidget->SetInteractorStyle(style);

    QHBoxLayout *hbox = new QHBoxLayout();
    this->setLayout(hbox);
    hbox->addWidget(this->vtkwidget);
}

// -----------------------------------------------------------------------------

QtVTKHook::QtVTKHook(int &argc, char **argv,
                     Model &model) : DisplayHook(), model(model)
{
    app = new QApplication(argc, argv);

    DisplayWindow *window = new DisplayWindow();
    window->show();
    app->exec();

    model.displayHook = this;
}

QtVTKHook::~QtVTKHook()
{
    delete app;
}

void
QtVTKHook::display()
{
    app->exec();
}

void
QtVTKHook::demo()
{
    // Create a cube source
    vtkSmartPointer<vtkCubeSource> cubeSource = vtkSmartPointer<vtkCubeSource>::New();

    // Create a mapper
    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->SetInputConnection(cubeSource->GetOutputPort());

    // Create an actor
    vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();
    actor->SetMapper(mapper);

    // Create a renderer
    vtkSmartPointer<vtkRenderer> renderer = vtkSmartPointer<vtkRenderer>::New();
    renderer->AddActor(actor);

    // Create a render window
    vtkSmartPointer<vtkRenderWindow> renderWindow = vtkSmartPointer<vtkRenderWindow>::New();
    renderWindow->AddRenderer(renderer);

    // Create an interactor
    vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    renderWindowInteractor->SetRenderWindow(renderWindow);

    // Initialize the interactor and start the rendering loop
    renderWindowInteractor->Initialize();
    renderWindowInteractor->Start();
}
