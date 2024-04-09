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
#include <vtkAxesActor.h>
#include <vtkOrientationMarkerWidget.h>
#include <vtkTextProperty.h>
#include <vtkCaptionActor2D.h>

#include "ui_DisplayWindow.h"

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

DisplayWindow::DisplayWindow(Model &model, QWidget *parent) : QMainWindow(parent), model(model), ui(new Ui::DisplayWindow)
{
    ui->setupUi(this);

    setWindowTitle("SPH (Louis++)");
    resize(800, 600);

    setupGUI();
    addParticles();
    addDomainBox();
    addXYZAxes();

    renderer->ResetCamera();
}

DisplayWindow::~DisplayWindow()
{
    delete ui;
}

void
DisplayWindow::setupGUI()
{
    this->vtkwidget = new QVTKOpenGLNativeWidget(this);
    this->setCentralWidget(this->vtkwidget);

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
    // QHBoxLayout *hbox = new QHBoxLayout();
    // this->setLayout(hbox);
    // hbox->addWidget(this->vtkwidget);
}

void
DisplayWindow::addParticles()
{
    // Fixed particles
    fixed_points = vtkSmartPointer<vtkPoints>::New();

    for(int i=0; i<model.numFP; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        fixed_points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
    polydata->SetPoints(fixed_points);

    vtkSmartPointer<vtkVertexGlyphFilter> vertexFilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    vertexFilter->SetInputData(polydata);
    vertexFilter->Update();

    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->SetInputConnection(vertexFilter->GetOutputPort());

    vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();
    actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    actor->GetProperty()->SetOpacity(0.3);
    actor->GetProperty()->SetPointSize(3);
    actor->SetMapper(mapper);

    renderer->AddActor(actor);

    // Mobile particles
    mobile_points = vtkSmartPointer<vtkPoints>::New();

    for(int i=model.numFP; i<model.numPart; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        mobile_points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    vtkSmartPointer<vtkPolyData> polydata2 = vtkSmartPointer<vtkPolyData>::New();
    polydata2->SetPoints(mobile_points);

    vtkSmartPointer<vtkVertexGlyphFilter> vertexFilter2 = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    vertexFilter2->SetInputData(polydata2);
    vertexFilter2->Update();

    vtkSmartPointer<vtkPolyDataMapper> mapper2 = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper2->SetInputConnection(vertexFilter2->GetOutputPort());

    vtkSmartPointer<vtkActor> actor2 = vtkSmartPointer<vtkActor>::New();
    actor2->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    actor2->GetProperty()->SetOpacity(1.0);
    actor2->GetProperty()->SetPointSize(3);
    actor2->SetMapper(mapper2);

    renderer->AddActor(actor2);
}

/// display a box representing the computational domain

void
DisplayWindow::addDomainBox()
{
    // Create a cube source
    vtkSmartPointer<vtkCubeSource> cubeSource = vtkSmartPointer<vtkCubeSource>::New();
    cubeSource->SetBounds(0.0, model.dom_dim, 0.0, model.dom_dim, 0.0, model.dom_dim);

    // Create a mapper
    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->SetInputConnection(cubeSource->GetOutputPort());

    // Transparent box
    box_actor = vtkSmartPointer<vtkActor>::New();
    box_actor->SetMapper(mapper);
    box_actor->GetProperty()->SetColor(0.0, 0.0, 1.0);
    box_actor->GetProperty()->SetOpacity(0.05);
    renderer->AddActor(box_actor);

    // Add a wireframe box
    boxwf_actor = vtkSmartPointer<vtkActor>::New();
    boxwf_actor->SetMapper(mapper);
    boxwf_actor->GetProperty()->SetRepresentationToWireframe();
    boxwf_actor->GetProperty()->SetColor(0.0, 0.0, 0.0);
    boxwf_actor->GetProperty()->SetLineWidth(1.0);
    renderer->AddActor(boxwf_actor);
}

/// small x,y,z axes in the corner of the window

void
DisplayWindow::addXYZAxes()
{
    vtkSmartPointer<vtkAxesActor> axes = vtkSmartPointer<vtkAxesActor>::New();
    axes->SetTotalLength(1, 1, 1);
    axes->SetShaftTypeToCylinder();
    axes->SetXAxisLabelText("x");
    axes->SetYAxisLabelText("y");
    axes->SetZAxisLabelText("z");

    vtkSmartPointer<vtkTextProperty> tprop = vtkSmartPointer<vtkTextProperty>::New();
    tprop->ItalicOn();
    tprop->SetColor(0.0, 0.0, 0.0);
    axes->GetXAxisCaptionActor2D()->SetCaptionTextProperty(tprop);
    vtkSmartPointer<vtkTextProperty> tprop2 = vtkSmartPointer<vtkTextProperty>::New();
    tprop2->ShallowCopy(tprop);
    axes->GetYAxisCaptionActor2D()->SetCaptionTextProperty(tprop2);
    vtkSmartPointer<vtkTextProperty> tprop3 = vtkSmartPointer<vtkTextProperty>::New();
    tprop3->ShallowCopy(tprop);
    axes->GetZAxisCaptionActor2D()->SetCaptionTextProperty(tprop3);

    axes_marker = vtkSmartPointer<vtkOrientationMarkerWidget>::New();
    axes_marker->SetOrientationMarker(axes);
    axes_marker->SetInteractor(vtkwidget->renderWindow()->GetInteractor());
    // axes_marker->SetViewport(0.0, 0.0, 0.3, 0.3);
    axes_marker->SetEnabled(1);
    axes_marker->InteractiveOff();
    // no actor to add to the renderer!
}

/// loop over particles and update their positions
void
DisplayWindow::updateParticlePositions()
{
    if(!mobile_points) return;

    // mobile particles only
    for(int i=model.numFP; i<model.numPart; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        mobile_points->SetPoint(i-model.numFP, pos(0), pos(1), pos(2));
    }
    mobile_points->Modified();

    vtkwidget->renderWindow()->Render();
}
