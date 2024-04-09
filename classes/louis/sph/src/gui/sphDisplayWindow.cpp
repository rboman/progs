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
#include <vtkScalarBarActor.h>
#include <vtkDoubleArray.h>
#include <vtkPointData.h>
#include <vtkLookupTable.h>
#include <vtkNamedColors.h>
#include <vtkNew.h>

#include "ui_DisplayWindow.h"

#include <QHBoxLayout>
#include <QMessageBox>

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
    resize(800*1.5, 600*1.5);

    setupGUI();
    addParticles();
    addDomainBox();
    addXYZAxes();

    vtkNew<vtkNamedColors> colors;
    renderer->GradientBackgroundOn();
    renderer->SetBackground(colors->GetColor3d("White").GetData());
    renderer->SetBackground2(colors->GetColor3d("LightBlue").GetData());

    resetCamera();
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

    // Renderer
    renderer = vtkSmartPointer<vtkRenderer>::New();
    // renderer->SetActiveCamera(camera);
    renderer->SetBackground(1.0, 1.0, 1.0);
    vtkwidget->renderWindow()->AddRenderer(renderer);
}

/// reset camera to default position / zoom

void DisplayWindow::resetCamera()
{
    auto camera = renderer->GetActiveCamera();

    camera->SetPosition(model.dom_dim, -model.dom_dim, model.dom_dim);
    camera->SetFocalPoint(model.dom_dim/2, model.dom_dim/2, model.dom_dim/2);
    camera->SetViewUp(0, 0, 1); 

    renderer->ResetCameraClippingRange();
    renderer->ResetCamera();
    vtkwidget->renderWindow()->Render();
}

void DisplayWindow::on_resetCamera_pushButton_clicked()
{
    //std::cout << "resetting Camera..." << std::endl;
    resetCamera();
}

void DisplayWindow::on_stop_pushButton_clicked()
{
    std::cout << "STOP..." << std::endl;

    // opens a dialog to ask whether the user is sure to quit
    // if the user clicks "yes", the program will throw an exception    
    // if the user clicks "no", the dialog will close and the program will continue
    // if the user closes the dialog, the program will continue

    QMessageBox::StandardButton reply;
    reply = QMessageBox::question(this, "STOP", "Are you sure you want to stop the simulation?",
                                  QMessageBox::Yes|QMessageBox::No);
    if (reply == QMessageBox::Yes)
        throw std::runtime_error("STOP");
}

void DisplayWindow::on_pause_pushButton_clicked()
{
    std::cout << "PAUSE..." << std::endl;
    // ...
}

void DisplayWindow::on_showBox_checkBox_toggled(bool checked)
{
    box_actor->SetVisibility(checked);
    boxwf_actor->SetVisibility(checked);
    vtkwidget->renderWindow()->Render();
}
void DisplayWindow::on_showFixed_checkBox_toggled(bool checked)
{
    fixed_actor->SetVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void DisplayWindow::on_fixedAlpha_slider_valueChanged(int value)
{
    // get max value of the slider
    int max = ui->fixedAlpha_slider->maximum();


    fixed_actor->GetProperty()->SetOpacity(value/(double)max);
    vtkwidget->renderWindow()->Render();

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

    fixed_actor = vtkSmartPointer<vtkActor>::New();
    fixed_actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    fixed_actor->GetProperty()->SetOpacity(ui->fixedAlpha_slider->value()/(double)ui->fixedAlpha_slider->maximum());
    fixed_actor->GetProperty()->SetPointSize(3);
    fixed_actor->SetVisibility(ui->showFixed_checkBox->isChecked());
    fixed_actor->SetMapper(mapper);

    renderer->AddActor(fixed_actor);

    // Mobile particles
    mobile_points = vtkSmartPointer<vtkPoints>::New();

    for(int i=model.numFP; i<model.numPart; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        mobile_points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    mobile_polydata = vtkSmartPointer<vtkPolyData>::New();
    mobile_polydata->SetPoints(mobile_points);

    // add scalar pressure to the particles
    vtkSmartPointer<vtkDoubleArray> pressureArray = vtkSmartPointer<vtkDoubleArray>::New();
    pressureArray->SetName("Pressure");
    pressureArray->SetNumberOfComponents(1);
    pressureArray->SetNumberOfTuples(model.numPart - model.numFP);
    for (int i = model.numFP; i < model.numPart; i++)
    {
        double pressure = model.particles[i]->p[0];
        pressureArray->SetValue(i - model.numFP, pressure);
    }
    // mobile_polydata->GetPointData()->AddArray(pressureArray);
    mobile_polydata->GetPointData()->SetScalars(pressureArray);  

    // glyph filter
    vtkSmartPointer<vtkVertexGlyphFilter> vertexFilter2 = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    vertexFilter2->SetInputData(mobile_polydata);
    // vertexFilter2->Update();

    // mapper
    vtkSmartPointer<vtkPolyDataMapper> mapper2 = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper2->SetInputConnection(vertexFilter2->GetOutputPort());
    mapper2->ScalarVisibilityOn();    
    mapper2->SetColorModeToMapScalars();
    mapper2->SetScalarModeToUsePointData();
    // mapper2->SelectColorArray("Pressure");

    //mapper2->SetScalarRange(0.0, 1.0);
    //mapper2->SetLookupTable(vtkSmartPointer<vtkLookupTable>::New());

    // add a scalar bar
    scalarBar = vtkSmartPointer<vtkScalarBarActor>::New();
    scalarBar->SetLookupTable(mapper2->GetLookupTable());
    scalarBar->SetTitle("Pressure");
    scalarBar->SetNumberOfLabels(5);
    // scalarBar->SetLabelFormat("%6.2f");
    // scalarBar->SetPosition(0.1, 0.1);
    // scalarBar->SetWidth(0.8);
    // scalarBar->SetHeight(0.1);
    // scalarBar->SetOrientationToHorizontal();
    // scalarBar->SetVisibility(1);
    renderer->AddActor2D(scalarBar);

    // Create a lookup table to share between the mapper and the scalarbar.
    // vtkNew<vtkLookupTable> hueLut;
    // hueLut->SetTableRange(0, 1);
    // hueLut->SetHueRange(0, 1);
    // hueLut->SetSaturationRange(1, 1);
    // hueLut->SetValueRange(1, 1);
    // hueLut->Build();

    // BASIC RED TO BLUE LUT
    vtkSmartPointer<vtkLookupTable> hueLut = vtkSmartPointer<vtkLookupTable>::New();
    hueLut->SetHueRange(0.667, 0.0);
    hueLut->SetSaturationRange(1.0, 1.0);
    hueLut->SetValueRange(1.0, 1.0);
    hueLut->SetAlphaRange(1.0, 1.0);
    hueLut->SetNumberOfTableValues(256);
    hueLut->Build();


    mapper2->SetLookupTable(hueLut);
    scalarBar->SetLookupTable(hueLut);

    mobile_actor = vtkSmartPointer<vtkActor>::New();
    mobile_actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    mobile_actor->GetProperty()->SetOpacity(1.0);
    mobile_actor->GetProperty()->SetPointSize(3);
    mobile_actor->SetMapper(mapper2);

    renderer->AddActor(mobile_actor);
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
    box_actor->GetProperty()->SetOpacity(0.02);
    box_actor->SetVisibility(ui->showBox_checkBox->isChecked());
    
    renderer->AddActor(box_actor);

    // Add a wireframe box
    boxwf_actor = vtkSmartPointer<vtkActor>::New();
    boxwf_actor->SetMapper(mapper);
    boxwf_actor->GetProperty()->SetRepresentationToWireframe();
    boxwf_actor->GetProperty()->SetColor(0.0, 0.0, 0.0);
    boxwf_actor->GetProperty()->SetLineWidth(1.0);
    boxwf_actor->SetVisibility(ui->showBox_checkBox->isChecked());
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

    auto pressureArray = mobile_polydata->GetPointData()->GetScalars();
    
    double pmin = std::numeric_limits<double>::max();
    double pmax = -std::numeric_limits<double>::max();
    for (int i = model.numFP; i < model.numPart; i++)
    {
        double pressure = model.particles[i]->p[0];
        if(pressure < pmin) pmin = pressure;
        if(pressure > pmax) pmax = pressure;
        pressureArray->SetTuple1(i - model.numFP, pressure);
    }
    pressureArray->Modified();

    // update lookup table
    // auto lut = mobile_actor->GetMapper()->GetLookupTable();
    // lut->SetRange(pmin, pmax);
    // std::cout << "lut range: " << pmin << " " << pmax << std::endl;
    // lut->Modified();


    // update the scalar bar
    // scalarBar->SetLookupTable(mobile_actor->GetMapper()->GetLookupTable());
    // scalarBar->Modified();

    // update the mapper
    mobile_actor->GetMapper()->SetScalarRange(pmin, pmax);
    mobile_actor->GetMapper()->Modified();

    vtkwidget->renderWindow()->Render();
}
