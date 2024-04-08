#include "sphQtVTKHook.h"
#include "sphDisplayWindow.h"
#include "sphModel.h"

#include <vtkCubeSource.h>
#include <vtkPolyDataMapper.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkGenericOpenGLRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkCamera.h>
#include <vtkInteractorStyleTrackballCamera.h>
#include <vtkProperty.h>
#include <vtkVertexGlyphFilter.h>
#include <vtkPoints.h>
#include <vtkPolyData.h>

using namespace sph;


// -----------------------------------------------------------------------------
// Notes: pourquoi 2 objets? (DisplayWindow / QtVTKHook)
//  A terme, on veut garder le code SPH indépendant de Qt.
//  Il n'est donc pas question de créer un objet QApplication dans main().
//  Il n'est pas possible de créer un seul objet qui serait un DisplayHook et un
//  QWidget comme je le fais en python. Ceci parce qu'il n'est pas permis de
//  créer un widget sans QApplication.
//  QtVTKHook doit donc être un DisplayHook, et il doit créer un QApplication,
//  et ensuite le widget Qt.


QtVTKHook::QtVTKHook(int &argc, char **argv,
                     Model &model) : DisplayHook(), model(model)
{
    app = new QApplication(argc, argv);

    window = new DisplayWindow(model);

    // window->setAttribute(Qt::WA_QuitOnClose);
    QObject::connect(app, SIGNAL(lastWindowClosed()), app, SLOT(quit()));

    window->show();
    // app->exec();

    model.displayHook = this;
}

QtVTKHook::~QtVTKHook()
{
    delete app;
}

void
QtVTKHook::interact()
{
    app->processEvents();
}

void
QtVTKHook::update_data()
{
    window->updateParticlePositions();
    app->processEvents();
}

void
QtVTKHook::loop()
{
    window->show();

    app->exec();
    std::cout << "quit()" << std::endl;
}

/// This demo is used to test the VTK library.
/// It does not use Qt.

void
QtVTKHook::standalone_VTK_demo()
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
