#include "cylindre.h"
#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkRenderWindow.h>
#include <vtkRenderer.h>
#include <vtkProperty.h>
#include <vtkDataSetMapper.h>
#include <vtkInteractorStyleTrackballCamera.h>

void displayugrid(vtkUnstructuredGrid *ugrid)
{
    auto meshMapper = vtkSmartPointer<vtkDataSetMapper>::New(); 
    meshMapper->SetInputData(ugrid);
    vtkSmartPointer<vtkActor> meshActor = vtkSmartPointer<vtkActor>::New();
    meshActor->SetMapper(meshMapper);

    auto gridMapper = vtkSmartPointer<vtkDataSetMapper>::New();  
    gridMapper->SetResolveCoincidentTopologyToPolygonOffset();
    gridMapper->ScalarVisibilityOff();
    gridMapper->SetInputData(ugrid);
    auto gridActor = vtkSmartPointer<vtkActor>::New();
    gridActor->GetProperty()->SetRepresentationToWireframe();
    gridActor->GetProperty()->SetColor(0.,0.,0.);
    gridActor->GetProperty()->SetAmbient(1.0);
    gridActor->GetProperty()->SetDiffuse(0.0);
    gridActor->GetProperty()->SetSpecular(0.0);
    gridActor->SetMapper(gridMapper);  

    auto ren = vtkSmartPointer<vtkRenderer>::New();
    ren->SetBackground(48./255,10./255,36./255); // unity terminal
    ren->AddActor(meshActor);
    ren->AddActor(gridActor);
    ren->ResetCamera();

    auto renWin = vtkSmartPointer<vtkRenderWindow>::New();
    renWin->SetSize(640, 480);    
    renWin->AddRenderer(ren);

    auto iren = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    iren->SetRenderWindow(renWin);

    auto style = vtkSmartPointer<vtkInteractorStyleTrackballCamera>::New();
    iren->SetInteractorStyle(style);
    iren->Initialize();
    //renWin->Render();
    iren->Start();
}