// from Code-It-Yourself! 3D Graphics Engine (javidx9)
// part #1: https://www.youtube.com/watch?v=ih20l3pJoeU
// part #2: https://www.youtube.com/watch?v=XgMWc6LumG4

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
