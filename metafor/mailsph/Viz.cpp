#include "mailsph.h"
#include "Viz.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderWindow.h>
#include <vtkRenderer.h>
#include <vtkProperty.h>
#include <vtkDataSetMapper.h>
#include <vtkInteractorStyleTrackballCamera.h>

#include <vtkAxesActor.h>
#include <vtkTextProperty.h>
#include <vtkOrientationMarkerWidget.h>
#include <vtkCaptionActor2D.h>
#include <vtkOutlineFilter.h>
#include <vtkPolyDataMapper.h>

void
Viz::display()
{

    // renderer & co

    auto ren = vtkSmartPointer<vtkRenderer>::New();
    // ren->SetBackground(48./255,10./255,36./255); // unity terminal
    ren->SetBackground(0.1, 0.2, 0.4);

    for (auto ugrid : grids)
    {
        std::cout << "\n===============\n";
        std::cout << ugrid->GetNumberOfPoints() << " points and "
                  << ugrid->GetNumberOfCells() << " cells created\n";
        ugrid->GetPoints()->ComputeBounds();
        double bounds[6];
        ugrid->GetPoints()->GetBounds(bounds);
        std::cout << "bounds=" << bounds[0] << "," << bounds[1] << ","
                  << bounds[2] << "," << bounds[3] << "," << bounds[4] << ","
                  << bounds[5] << "\n";

        // ugrid->GetPoints()->Print(std::cout);

        // mesh
        auto meshMapper = vtkSmartPointer<vtkDataSetMapper>::New();
        meshMapper->SetInputData(ugrid);
        vtkSmartPointer<vtkActor> meshActor = vtkSmartPointer<vtkActor>::New();
        meshActor->SetMapper(meshMapper);
        meshActor->GetProperty()->SetOpacity(0.5);

        // grid
        auto gridMapper = vtkSmartPointer<vtkDataSetMapper>::New();
        gridMapper->SetResolveCoincidentTopologyToPolygonOffset();
        gridMapper->ScalarVisibilityOff();
        gridMapper->SetInputData(ugrid);
        auto gridActor = vtkSmartPointer<vtkActor>::New();
        gridActor->GetProperty()->SetRepresentationToWireframe();
        gridActor->GetProperty()->SetColor(0.1 * 2, 0.2 * 2, 0.4 * 2);
        gridActor->GetProperty()->SetAmbient(1.0);
        gridActor->GetProperty()->SetDiffuse(0.0);
        gridActor->GetProperty()->SetSpecular(0.0);
        gridActor->SetMapper(gridMapper);

        // bbox
        auto bboxfilter = vtkSmartPointer<vtkOutlineFilter>::New();
        bboxfilter->SetInputData(ugrid);
        auto bboxMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
        bboxMapper->SetInputConnection(bboxfilter->GetOutputPort());
        auto bboxActor = vtkSmartPointer<vtkActor>::New();
        bboxActor->SetMapper(bboxMapper);
        bboxActor->GetProperty()->SetColor(0.1 * 2, 0.2 * 2, 0.4 * 2);

        ren->AddActor(meshActor);
        ren->AddActor(gridActor);
        ren->AddActor(bboxActor);
    }

    ren->ResetCamera();

    auto renWin = vtkSmartPointer<vtkRenderWindow>::New();
    renWin->SetSize(640, 480);
    renWin->AddRenderer(ren);

    auto iren = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    iren->SetRenderWindow(renWin);

    auto style = vtkSmartPointer<vtkInteractorStyleTrackballCamera>::New();
    iren->SetInteractorStyle(style);

    // -- axes a la paraview
    auto axes = vtkSmartPointer<vtkAxesActor>::New();
    axes->SetShaftTypeToCylinder();
    axes->SetXAxisLabelText("x");
    axes->SetYAxisLabelText("y");
    axes->SetZAxisLabelText("z");
    axes->SetTotalLength(1, 1, 1);
    auto tprop = vtkSmartPointer<vtkTextProperty>::New();
    tprop->ItalicOn();
    axes->GetXAxisCaptionActor2D()->SetCaptionTextProperty(tprop);
    axes->GetYAxisCaptionActor2D()->SetCaptionTextProperty(tprop);
    axes->GetZAxisCaptionActor2D()->SetCaptionTextProperty(tprop);

    auto marker = vtkSmartPointer<vtkOrientationMarkerWidget>::New();
    marker->SetOrientationMarker(axes);
    marker->SetViewport(0.85, 0.8, 1.1, 1.1);

    marker->SetInteractor(iren);
    marker->SetEnabled(1);
    marker->InteractiveOff();
    // -- fin axes

    iren->Initialize();
    renWin->Render();
    iren->Start();
}