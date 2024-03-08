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
#include <vtkProperty.h>


DisplayWindow::DisplayWindow(QWidget *parent) : QWidget(parent)
{
    setWindowTitle("VTK in Qt");
    resize(800, 600);

    setupGUI();
    addCube();
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
    // vtkSmartPointer<vtkCamera> camera = vtkSmartPointer<vtkCamera>::New();
    // camera->SetViewUp(0, 1, 0);
    // camera->SetPosition(0, 0, 10);
    // camera->SetFocalPoint(0, 0, 0);

    // Renderer
    renderer = vtkSmartPointer<vtkRenderer>::New();
    // renderer->SetActiveCamera(camera);
    renderer->SetBackground(1.0, 1.0, 1.0);
    vtkwidget->renderWindow()->AddRenderer(renderer);

    // Reset the camera
    renderer->ResetCamera();

    // Layout Qt
    QHBoxLayout *hbox = new QHBoxLayout();
    this->setLayout(hbox);
    hbox->addWidget(this->vtkwidget);
}

void
DisplayWindow::addCube()
{
    // Create a cube source
    vtkSmartPointer<vtkCubeSource> cubeSource = vtkSmartPointer<vtkCubeSource>::New();

    // Create a mapper
    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->SetInputConnection(cubeSource->GetOutputPort());

    // Create an actor
    vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();
    actor->SetMapper(mapper);
    actor->GetProperty()->SetColor(1.0, 0.0, 0.0); // Set color to red
    actor->GetProperty()->SetSpecular(1.0); // Enable specular reflection
    actor->GetProperty()->SetSpecularPower(50.0); // Set specular power

    // Add the actor to the scene
    renderer->AddActor(actor);
}

// -----------------------------------------------------------------------------

QtVTKHook::QtVTKHook(int &argc, char **argv,
                     Model &model) : DisplayHook(), model(model)
{
    app = new QApplication(argc, argv);

    window = new DisplayWindow();
    //window->show();

    //window->setAttribute(Qt::WA_QuitOnClose);
    QObject::connect( app, SIGNAL( lastWindowClosed() ), app, SLOT( quit() ) );

    // app->exec();
    

    model.displayHook = this;
}

QtVTKHook::~QtVTKHook()
{
    delete app;
}

void
QtVTKHook::display()
{
    //window->show();

    // app->exec();
    // std::cout << "quit()" << std::endl;
}


void
QtVTKHook::loop()
{
    //window->show();

    app->exec();
    std::cout << "quit()" << std::endl;
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
