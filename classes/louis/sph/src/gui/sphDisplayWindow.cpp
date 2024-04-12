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
#include <QElapsedTimer>

namespace sph {

std::vector<ScalarField> scalarFields = {
    ScalarField(ScalarCode::DENSITY, "Density"),
    ScalarField(ScalarCode::PRESSURE, "Pressure"),
    ScalarField(ScalarCode::VELOCITY, "Velocity"),
    ScalarField(ScalarCode::MASS, "Mass"),
    ScalarField(ScalarCode::SPEED_OF_SOUND, "Speed of sound"),
    ScalarField(ScalarCode::NB_NEIGHBOURS, "Number of neighbours"),
    ScalarField(ScalarCode::MAX_MU_AB, "Max mu_ab"),
    ScalarField(ScalarCode::SMTH_LENGTH, "Smoothing length")
};

}; // namespace sph

using namespace sph;

// -----------------------------------------------------------------------------
// Notes: pourquoi 2 objets (QtVTKHook et DisplayHook) ?
//  A terme, on veut garder le code SPH indépendant de Qt.
//  Il n'est donc pas question de créer un objet QApplication dans main().
//  Il n'est pas possible de créer un seul objet qui serait un DisplayHook et un
//  QWidget comme je le fais en python. Ceci parce qu'il n'est pas permis de
//  créer un widget sans QApplication.
//  QtVTKHook doit donc être un DisplayHook, et il doit créer un QApplication,
//  et ensuite le widget Qt.

DisplayWindow::DisplayWindow(Model &model, QWidget *parent)
    : QMainWindow(parent), model(model),
      paused(false),
      ui(new Ui::DisplayWindow)
{
    ui->setupUi(this);

    setWindowTitle("SPH (Louis++)");
    resize(800 * 1.5, 600 * 1.5);
    setMinimumSize(800, 600);

    setupGUI();
    addParticles();
    addDomainBox();
    addXYZAxes();

    resetCamera();

    // add QDoubleValidator to lineEdits
    QLocale lo(QLocale::C);
    lo.setNumberOptions(QLocale::RejectGroupSeparator);

    QDoubleValidator *validator = new QDoubleValidator(this);
    validator->setLocale(lo); // '.' as decimal separator
    ui->minScalar_lineEdit->setValidator(validator);
    ui->maxScalar_lineEdit->setValidator(validator);

    // fill the comboBox with scalar fields
    ui->scalars_comboBox->clear();
    for(auto const &fields : scalarFields)
        ui->scalars_comboBox->addItem(QString::fromStdString(fields.name));

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

    renderer->GradientBackgroundOn();
    vtkNew<vtkNamedColors> colors;
    renderer->SetBackground(colors->GetColor3d("White").GetData());
    renderer->SetBackground2(colors->GetColor3d("LightBlue").GetData());

    vtkwidget->renderWindow()->AddRenderer(renderer);
}

/// reset camera to default position / zoom

void
DisplayWindow::resetCamera()
{
    auto camera = renderer->GetActiveCamera();

    camera->SetPosition(model.dom_dim, -model.dom_dim, model.dom_dim);
    camera->SetFocalPoint(model.dom_dim / 2, model.dom_dim / 2, model.dom_dim / 2);
    camera->SetViewUp(0, 0, 1);

    renderer->ResetCameraClippingRange();
    renderer->ResetCamera();
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_resetCamera_pushButton_clicked()
{
    resetCamera();
}

void
DisplayWindow::on_stop_pushButton_clicked()
{
    std::cout << "STOP..." << std::endl;

    QMessageBox::StandardButton reply;
    reply = QMessageBox::question(this, "STOP", "Are you sure you want to stop the simulation?",
                                  QMessageBox::Yes | QMessageBox::No);
    if (reply == QMessageBox::Yes)
        throw std::runtime_error("STOP");
}

/// pause the simulation

void
DisplayWindow::on_pause_pushButton_clicked()
{
    QApplication *app = qobject_cast<QApplication *>(QApplication::instance());

    if (!paused)
    {
        paused = true;
        ui->pause_pushButton->setText("Resume");
        app->exec();
    }
    else
    {
        paused = false;
        ui->pause_pushButton->setText("Pause");
        app->exit();
    }
}

void
DisplayWindow::pause()
{
    on_pause_pushButton_clicked();
}

void
DisplayWindow::on_showBox_checkBox_toggled(bool checked)
{
    box_actor->SetVisibility(checked);
    boxwf_actor->SetVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_showFixed_checkBox_toggled(bool checked)
{
    fixed_actor->SetVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_showMobile_checkBox_toggled(bool checked)
{
    mobile_actor->SetVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_fixedAlpha_slider_valueChanged(int value)
{
    int max = ui->fixedAlpha_slider->maximum();
    fixed_actor->GetProperty()->SetOpacity(value / (double)max);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_mobileAlpha_slider_valueChanged(int value)
{
    int max = ui->mobileAlpha_slider->maximum();
    mobile_actor->GetProperty()->SetOpacity(value / (double)max);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_minScalar_checkBox_toggled(bool checked)
{
    if (paused)
        this->heavy_update();
}

void
DisplayWindow::on_maxScalar_checkBox_toggled(bool checked)
{
    if (paused)
        this->heavy_update();
}

void
DisplayWindow::on_fixedScalars_checkBox_toggled(bool checked)
{
    fixed_actor->GetMapper()->SetScalarVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_mobileScalars_checkBox_toggled(bool checked)
{
    mobile_actor->GetMapper()->SetScalarVisibility(checked);
    vtkwidget->renderWindow()->Render();
}

void
DisplayWindow::on_particleSize_slider_valueChanged(int value)
{
    fixed_actor->GetProperty()->SetPointSize(value);
    mobile_actor->GetProperty()->SetPointSize(value);
    vtkwidget->renderWindow()->Render();
}

void DisplayWindow::on_scalars_comboBox_currentIndexChanged(int index)
{

    if (paused)
        this->heavy_update();
}

void
DisplayWindow::addParticles()
{
    // Fixed particles =================================================

    fixed_points = vtkSmartPointer<vtkPoints>::New();

    for (int i = 0; i < model.numFP; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        fixed_points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    fixed_polydata = vtkSmartPointer<vtkPolyData>::New();
    fixed_polydata->SetPoints(fixed_points);

    // add scalar pressure to the fixed particles
    vtkSmartPointer<vtkDoubleArray> fixed_scalars = vtkSmartPointer<vtkDoubleArray>::New();
    fixed_scalars->SetName("Pressure");
    fixed_scalars->SetNumberOfComponents(1);
    fixed_scalars->SetNumberOfTuples(model.numFP);
    for (int i = 0; i < model.numFP; i++)
    {
        double pressure = model.particles[i]->p[0];
        fixed_scalars->SetValue(i, pressure);
    }
    fixed_polydata->GetPointData()->SetScalars(fixed_scalars);

    vtkSmartPointer<vtkVertexGlyphFilter> fixed_vfilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    fixed_vfilter->SetInputData(fixed_polydata);
    fixed_vfilter->Update();

    vtkSmartPointer<vtkPolyDataMapper> fixed_mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    fixed_mapper->SetInputConnection(fixed_vfilter->GetOutputPort());
    fixed_mapper->SetColorModeToMapScalars();
    fixed_mapper->SetScalarModeToUsePointData();
    fixed_mapper->SetScalarVisibility(ui->fixedScalars_checkBox->isChecked());

    fixed_actor = vtkSmartPointer<vtkActor>::New();
    fixed_actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    fixed_actor->GetProperty()->SetOpacity(ui->fixedAlpha_slider->value() / (double)ui->fixedAlpha_slider->maximum());
    fixed_actor->GetProperty()->SetPointSize(ui->particleSize_slider->value());
    fixed_actor->SetVisibility(ui->showFixed_checkBox->isChecked());
    fixed_actor->SetMapper(fixed_mapper);

    renderer->AddActor(fixed_actor);

    // Mobile particles ================================================

    mobile_points = vtkSmartPointer<vtkPoints>::New();

    for (int i = model.numFP; i < model.numPart; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        mobile_points->InsertNextPoint(pos(0), pos(1), pos(2));
    }

    mobile_polydata = vtkSmartPointer<vtkPolyData>::New();
    mobile_polydata->SetPoints(mobile_points);

    // add scalar pressure to the particles
    vtkSmartPointer<vtkDoubleArray> mobile_scalars = vtkSmartPointer<vtkDoubleArray>::New();
    mobile_scalars->SetName("Pressure");
    mobile_scalars->SetNumberOfComponents(1);
    mobile_scalars->SetNumberOfTuples(model.numPart - model.numFP);
    for (int i = model.numFP; i < model.numPart; i++)
    {
        double pressure = model.particles[i]->p[0];
        mobile_scalars->SetValue(i - model.numFP, pressure);
    }
    mobile_polydata->GetPointData()->SetScalars(mobile_scalars);

    // glyph filter (est-ce bien utile??)
    vtkSmartPointer<vtkVertexGlyphFilter> mobile_vfilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
    mobile_vfilter->SetInputData(mobile_polydata);

    // mapper
    vtkSmartPointer<vtkPolyDataMapper> mobile_mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mobile_mapper->SetInputConnection(mobile_vfilter->GetOutputPort());
    mobile_mapper->SetColorModeToMapScalars();
    mobile_mapper->SetScalarModeToUsePointData();
    mobile_mapper->SetScalarVisibility(ui->mobileScalars_checkBox->isChecked());

    mobile_actor = vtkSmartPointer<vtkActor>::New();
    mobile_actor->GetProperty()->SetColor(0.0, 0.0, 0.0); // Set color to black
    mobile_actor->GetProperty()->SetOpacity(ui->mobileAlpha_slider->value() / (double)ui->mobileAlpha_slider->maximum());
    mobile_actor->GetProperty()->SetPointSize(ui->particleSize_slider->value());
    mobile_actor->SetVisibility(ui->showMobile_checkBox->isChecked());
    mobile_actor->SetMapper(mobile_mapper);

    renderer->AddActor(mobile_actor);

    // scalar bar ======================================================

    // add a scalar bar
    scalarBar = vtkSmartPointer<vtkScalarBarActor>::New();
    scalarBar->SetTitle("Pressure");
    scalarBar->SetNumberOfLabels(5);
    scalarBar->SetMaximumWidthInPixels(100);
    scalarBar->SetMaximumHeightInPixels(500);
    // scalarBar->AnnotationTextScalingOff();
    scalarBar->UnconstrainedFontSizeOn();
    scalarBar->SetMaximumNumberOfColors(16);
    scalarBar->GetLabelTextProperty()->SetFontSize(15);
    scalarBar->GetLabelTextProperty()->SetColor(0.0, 0.0, 0.0);
    scalarBar->GetLabelTextProperty()->ShadowOff();
    scalarBar->GetTitleTextProperty()->SetFontSize(25);
    scalarBar->GetTitleTextProperty()->SetColor(0.0, 0.0, 0.0);
    scalarBar->GetTitleTextProperty()->ShadowOff();
    scalarBar->DrawAboveRangeSwatchOn();
    scalarBar->DrawBelowRangeSwatchOn();
    scalarBar->SetAboveRangeAnnotation("");
    scalarBar->SetBelowRangeAnnotation("");
    scalarBar->SetVerticalTitleSeparation(10);
    renderer->AddActor2D(scalarBar);

    // see
    // https://www.compilatrix.com/docs/vtk-lookup-tables

    vtkSmartPointer<vtkLookupTable> lut = vtkSmartPointer<vtkLookupTable>::New();
    lut->SetHueRange(0.667, 0.0);
    lut->SetAboveRangeColor(1.0, 0.0, 1.0, 1.0); // purple
    lut->SetBelowRangeColor(0.1, 0.1, 0.1, 1.0); // almost black
    lut->UseAboveRangeColorOn();
    lut->UseBelowRangeColorOn();
    lut->SetNumberOfTableValues(256);
    lut->Build();

    // set lut to the mappers

    fixed_mapper->SetLookupTable(lut);
    mobile_mapper->SetLookupTable(lut);
    scalarBar->SetLookupTable(lut);
}

/// display a box representing the computational domain

void
DisplayWindow::addDomainBox()
{
    // Create a cube source
    vtkSmartPointer<vtkCubeSource> cubeSource = vtkSmartPointer<vtkCubeSource>::New();
    cubeSource->SetBounds(0.0, model.dom_dim,
                          0.0, model.dom_dim,
                          0.0, model.dom_dim);

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
    axes_marker->SetEnabled(1);
    axes_marker->InteractiveOff();
    // no actor to add to the renderer!
}

/// This function is called every time steps.
/// It should remain very fast.

void
DisplayWindow::light_update()
{
    ui->progressBar->setValue(model.currentTime / model.maxTime * 100);
}

/// This function is called every seconds in order to update the display
/// with the actual data in memory.
/// It can be slower than light_update.

void
DisplayWindow::heavy_update()
{
    // create a timer and measure time
    QElapsedTimer timer;
    timer.start();

    // update particles
    updateParticlePositions();

    // update infos:
    ui->infos_textEdit->clear();
    ui->infos_textEdit->append(QString("Time: %1 / %2 s").arg(model.currentTime).arg(model.maxTime));
    ui->infos_textEdit->append(QString("Time step: %1 s").arg(model.timeStep));
    ui->infos_textEdit->append(QString("Number of particles: %1").arg(model.numPart));
    ui->infos_textEdit->append(QString("Fixed particles: %1 (%2 %)").arg(model.numFP).arg(model.numFP * 100.0 / model.numPart));
    ui->infos_textEdit->append(QString("Mobile particles: %1 (%2 %)").arg(model.numMP).arg(model.numMP * 100.0 / model.numPart));

    // cpu times
    double cpu_total = g_timers["TOTAL"].elapsed() - g_timers["GUI"].elapsed();
    ui->infos_textEdit->append(QString("CPU sort: %1 %").arg( g_timers["sort"].elapsed() / cpu_total * 100));
    ui->infos_textEdit->append(QString("CPU update: %1 %").arg( g_timers["update_vars"].elapsed() / cpu_total * 100));

    // display time spent in this routine
    int elapsed = timer.elapsed();
    ui->infos_textEdit->append(QString("GUI update time: %1 ms").arg(elapsed));
}

/// Loop over particles and update their positions and scalar field

void
DisplayWindow::updateParticlePositions()
{
    if (!mobile_points)
        return;
    // -----------------------------------------------------------------
    // positions: fixed particles 

    //  (they can move thanks to prescribed displacements)
    for (int i = 0; i < model.numFP; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        fixed_points->SetPoint(i, pos(0), pos(1), pos(2));
    }
    fixed_points->Modified();

    // positions: mobile particles
    for (int i = model.numFP; i < model.numPart; i++)
    {
        auto const &pos = model.particles[i]->coord[0];
        mobile_points->SetPoint(i - model.numFP, pos(0), pos(1), pos(2));
    }
    mobile_points->Modified();


    // -----------------------------------------------------------------
    // Update scalar field

    auto mobile_scalars = mobile_polydata->GetPointData()->GetScalars();
    auto fixed_scalars = fixed_polydata->GetPointData()->GetScalars();

    ScalarField field = scalarFields[ui->scalars_comboBox->currentIndex()];


    double pmin = std::numeric_limits<double>::max();
    double pmax = -std::numeric_limits<double>::max();

    // fixed particles
    for (int i = 0; i < model.numFP; i++)
    {
        double pressure = 0.0;
        switch(field.code)
        {
            case ScalarCode::DENSITY:
                pressure = model.particles[i]->rho[0];
                break;
            case ScalarCode::PRESSURE:
                pressure = model.particles[i]->p[0];
                break;
            case ScalarCode::VELOCITY:
                pressure = model.particles[i]->speed[0].norm();
                break;
            case ScalarCode::MASS:
                pressure = model.particles[i]->m;
                break;
            case ScalarCode::SPEED_OF_SOUND:
                pressure = model.particles[i]->c[0];
                break;
            case ScalarCode::NB_NEIGHBOURS:
                pressure = model.particles[i]->neighbours.size();
                break;
            case ScalarCode::MAX_MU_AB:
                pressure = model.particles[i]->max_mu_ab;
                break;
            case ScalarCode::SMTH_LENGTH:
                pressure = model.particles[i]->h;
                break;
        }
        if (pressure < pmin)
            pmin = pressure;
        if (pressure > pmax)
            pmax = pressure;
        fixed_scalars->SetTuple1(i, pressure);
    }
    fixed_scalars->Modified();

    // mobile particles
    for (int i = model.numFP; i < model.numPart; i++)
    {
        double pressure = 0.0;
        switch(field.code)
        {
            case ScalarCode::DENSITY:
                pressure = model.particles[i]->rho[0];
                break;
            case ScalarCode::PRESSURE:
                pressure = model.particles[i]->p[0];
                break;
            case ScalarCode::VELOCITY:
                pressure = model.particles[i]->speed[0].norm();
                break;
            case ScalarCode::MASS:
                pressure = model.particles[i]->m;
                break;
            case ScalarCode::SPEED_OF_SOUND:
                pressure = model.particles[i]->c[0];
                break;
            case ScalarCode::NB_NEIGHBOURS:
                pressure = model.particles[i]->neighbours.size();
                break;
            case ScalarCode::MAX_MU_AB:
                pressure = model.particles[i]->max_mu_ab;
                break;
            case ScalarCode::SMTH_LENGTH:
                pressure = model.particles[i]->h;
                break;
        }
        if (pressure < pmin)
            pmin = pressure;
        if (pressure > pmax)
            pmax = pressure;
        mobile_scalars->SetTuple1(i - model.numFP, pressure);
    }
    mobile_scalars->Modified();

    // update scalarbar title
    scalarBar->SetTitle(field.name.c_str());

    // update min/max values of the mapper
    if (ui->minScalar_checkBox->isChecked())
        pmin = ui->minScalar_lineEdit->text().toDouble();
    if (ui->maxScalar_checkBox->isChecked())
        pmax = ui->maxScalar_lineEdit->text().toDouble();

    mobile_actor->GetMapper()->SetScalarRange(pmin, pmax);
    mobile_actor->GetMapper()->Modified();

    fixed_actor->GetMapper()->SetScalarRange(pmin, pmax);
    fixed_actor->GetMapper()->Modified();

    vtkwidget->renderWindow()->Render();
}
