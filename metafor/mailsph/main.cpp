// mkdir build
// cd build
// cmake -G "Visual Studio 14 2015 Win64" ..
// cmake --build . --config Release


#include "mailsph.h"
#include "Cylinder.h"
#include "Sphere.h"
#include "Viz.h"

int main()
{
    try
    {
        //std::vector<vtkSmartPointer<vtkUnstructuredGrid>> grids;
        Viz viz;


        Cylinder cyl1; 
        cyl1.build();
        cyl1.exportvtu("cyl1.vtu");
        viz.grids.push_back( cyl1.ugrid );

        Cylinder cyl2;
        cyl2.centre[0] = 100.0; 
        cyl2.cyl_creux = 0;
        cyl2.build();
        viz.grids.push_back( cyl2.ugrid );


        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = sphereBAD();     // pas OK

        //vtkSmartPointer<vtkUnstructuredGrid> ugrid = spherepeauBAD(); // pas OK


        Sphere sph1; 
        sph1.build();
        viz.grids.push_back( sph1.ugrid );

        //std::cout << ugrid->GetNumberOfPoints() << " points and " << ugrid->GetNumberOfCells() << " cells created\n";

        //exportvtu(ugrid, "mesh.vtu");
        viz.display();
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