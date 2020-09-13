// build & run
// cmake -A x64 .. && cmake --build . --config Release && Release\cube3d.exe

#include "cube3d.h"

int
main()
{
    Cube3d demo;
    if (demo.Construct(500, 500, 2, 2))
        demo.Start();
    return 0;
}
