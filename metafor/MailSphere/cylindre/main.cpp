#include "cylindre.h"

int main()
{
    try
    {
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = cylindre();
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphereBAD();     // pas OK
        vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphere2(); // OK
        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = spherepeauBAD(); // pas OK

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