#include "sphDisplayWindow.h"
#include "sphModel.h"
#include "sphParticle.h"

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

#include <QHBoxLayout>

using namespace sph;


// -----------------------------------------------------------------------------
// Notes: pourquoi 2 objets?
//  A terme, on veut garder le code SPH indépendant de Qt.
//  Il n'est donc pas question de créer un objet QApplication dans main().
//  Il n'est pas possible de créer un seul objet qui serait un DisplayHook et un
//  QWidget comme je le fais en python. Ceci parce qu'il n'est pas permis de
//  créer un widget sans QApplication.
//  QtVTKHook doit donc être un DisplayHook, et il doit créer un QApplication,
//  et ensuite le widget Qt.

DisplayWindow::DisplayWindow(Model &model, QWidget *parent) : QWidget(parent), model(model)
{
    setWindowTitle("SPH (Louis++)");
    resize(800, 600);

    setupGUI();
    // addCube();
    addParticles();
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
DisplayWindow::addParticles()
{
    // Assuming models.particles is a std::vector of some 3D point type
    points = vtkSmartPointer<vtkPoints>::New();

    for (const auto &particle : model.particles)
    {
        auto const &pos = particle->coord[0];
        points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
    polydata->SetPoints(points);

    vtkSmartPointer<vtkVertexGlyphFilter> vertexFilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    vertexFilter->SetInputData(polydata);
    vertexFilter->Update();

    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->SetInputConnection(vertexFilter->GetOutputPort());

    vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();
    actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    // actor->GetProperty()->SetSpecular(1.0);        // Enable specular reflection
    // actor->GetProperty()->SetSpecularPower(50.0);  // Set specular power
    actor->GetProperty()->SetPointSize(3);
    actor->SetMapper(mapper);

    renderer->AddActor(actor);
}

void
DisplayWindow::updateParticlePositions()
{
    if(!points) return;
    // loop over points 

    // std::cout << "updateParticlePositions()" << std::endl;
    int i=0;
    for (const auto &particle : model.particles)
    {
        auto &pos = particle->coord[0];
        points->SetPoint(i++, pos(0), pos(1), pos(2));
    }   

    points->Modified();

    vtkwidget->renderWindow()->Render();
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
    actor->GetProperty()->SetSpecular(1.0);        // Enable specular reflection
    actor->GetProperty()->SetSpecularPower(50.0);  // Set specular power

    // Add the actor to the scene
    renderer->AddActor(actor);
}