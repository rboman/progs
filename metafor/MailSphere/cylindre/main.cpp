#include "cylindre.h"

int main()
{
    try
    {
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = cylindre();
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphere();
        vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphere2();
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = spherepeau();

        std::cout << ugrid->GetNumberOfPoints() << " points and " << ugrid->GetNumberOfCells() << " cells created\n";

        exportvtu(ugrid, "mesh.vtu");
        displayugrid(ugrid);
    }
    catch(std::exception &e) 
    {
        std::cerr << e.what();
    }
    catch(...) 
    {
        std::cerr << "Unknown C++ Runtime Error";
    }  

    return 0;   
}