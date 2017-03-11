#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkVRMLImporter.h>
#include <vtkRenderWindowInteractor.h>

void main(int argc, char *argv[])
{

    if(argc!=2)
    {
        std::cout << argv[0] << " by RoBo: display VRML 2 file using vtkVRMLImporter\n";
        std::cout << "usage:\n";
        std::cout << "\t" << argv[0] << " file.wrl\n\n";
        exit(1);
    }
    char *filename = argv[1];

    vtkRenderer *ren1 = vtkRenderer::New();
    vtkRenderWindow *renWin = vtkRenderWindow::New();
    renWin->AddRenderer(ren1);

    vtkVRMLImporter *importer = vtkVRMLImporter::New();
    importer->SetRenderWindow(renWin);
    importer->SetFileName(filename);
    importer->Read();

    vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
    iren->SetRenderWindow(renWin);

    importer->GetRenderer()->SetBackground(0.1, 0.2, 0.4);
    importer->GetRenderWindow()->SetSize(500, 500);

    renWin->Render();
    iren->Start();
}
