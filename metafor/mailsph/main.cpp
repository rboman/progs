#include "mailsph.h"
#include "Cylinder.h"
#include "Sphere.h"

int main()
{
    try
    {
        std::vector<vtkSmartPointer<vtkUnstructuredGrid>> grids;



        Cylinder cyl1; 
        cyl1.build();
        grids.push_back( cyl1.ugrid );

        Cylinder cyl2;
        cyl2.centre[0] = 100.0; 
        cyl2.cyl_creux = 0;
        cyl2.build();
        grids.push_back( cyl2.ugrid );


        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphereBAD();     // pas OK

        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = spherepeauBAD(); // pas OK


        Sphere sph1; 
        sph1.build();
        grids.push_back( sph1.ugrid );

        //std::cout << ugrid->GetNumberOfPoints() << " points and " << ugrid->GetNumberOfCells() << " cells created\n";

        //exportvtu(ugrid, "mesh.vtu");
        displayugrid(grids);
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