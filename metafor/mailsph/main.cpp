// mkdir build
// cd build
// cmake -G "Visual Studio 14 2015 Win64" ..
// cmake --build . --config Release

#include "mailsph.h"
#include "Cylinder.h"
#include "Sphere.h"
#include "Viz.h"

int
main()
{
    try
    {
        Viz viz;

        Cylinder cyl1;
        cyl1.build();
        cyl1.exportvtu("cyl1.vtu");
        viz.grids.push_back(cyl1.ugrid);

        Cylinder cyl2;
        cyl2.centre[0] = 120.0;
        cyl2.norm[1] = 1.0;
        cyl2.cyl_creux = 0;
        cyl2.rint = 50;
        cyl2.build();
        viz.grids.push_back(cyl2.ugrid);

        Cylinder cyl3;
        // cyl3.centre[0] = 120.0*2;
        // cyl3.norm[1] = 1.0;
        // cyl3.cyl_ouvert = 1;
        // cyl3.theta0 = 270.0;
        cyl3.build();
        viz.grids.push_back(cyl3.ugrid);

        Sphere sph1;
        sph1.build();
        viz.grids.push_back(sph1.ugrid);

        viz.display();
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << '\n';
        /*
        std::cout << "<enter> to quit\n";
        int a;
        std::cin >> a;
        */
    }
    catch (...)
    {
        std::cerr << "Unknown C++ Runtime Error";
    }

    return 0;
}