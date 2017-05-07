#include <string>
#include <vtkSmartPointer.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkVRMLImporter.h>
#include <vtk3DSImporter.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkInteractorStyleTrackballCamera.h>
#include <vtkCamera.h>

int main(int argc, char *argv[])
{
    if(argc!=2)
    {
        std::cout << argv[0] << " by RoBo: display 3DS & VRML2 files using vtkImporter classes\n";
        std::cout << "usage:\n";
        std::cout << "\t" << argv[0] << " file.[3ds|wrl]\n\n";
        return 1;
    }
    
    auto ren = vtkSmartPointer<vtkRenderer>::New();
    auto renWin = vtkSmartPointer<vtkRenderWindow>::New();
    renWin->AddRenderer(ren);

    std::string filename = argv[1];
    if (filename.find(".3ds")!=std::string::npos)
    {   
        auto importer = vtkSmartPointer<vtk3DSImporter>::New();
        importer->SetRenderWindow(renWin);
        importer->SetFileName(filename.c_str());
        importer->Read();
    } else if (filename.find(".wrl")!=std::string::npos)
    {
        auto importer = vtkSmartPointer<vtkVRMLImporter>::New();    
        importer->SetRenderWindow(renWin);
        importer->SetFileName(filename.c_str());
        importer->Read();
    }
    else
    {
        std::cout << "Unknown format\n\n";
        return 1;
    }

    auto iren = vtkSmartPointer<vtkRenderWindowInteractor>::New();
    iren->SetRenderWindow(renWin);
    auto style = vtkSmartPointer<vtkInteractorStyleTrackballCamera>::New();
    iren->SetInteractorStyle(style);

    ren->SetBackground(0.1, 0.2, 0.4);
    renWin->SetSize(500, 500);

    auto camera = ren->GetActiveCamera();
    camera->SetPosition(1, -2, 0.5);
    camera->SetFocalPoint(0, 0, 0);
    camera->SetViewUp(0, 0, 1); // up = +z

    // let the renderer compute good position and focal point
    ren->ResetCamera();
    camera->Dolly(1.2); // zoom
    ren->ResetCameraClippingRange();

    //iren->Initialize();  // useless
    //renWin->Render();    // useless
    iren->Start();
}
