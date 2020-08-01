// build & run
// mkdir build && cd build
// cmake -A x64 .. && cmake --build . --config Release && Release\dungeon.exe

#include "Game.h"

int
main()
{
    try // does not work
    {
        Game demo;
        if (demo.Construct(800, 600, 2, 2))
            demo.Start();
    }
    catch (const std::exception &e)
    {
        std::cerr << e.what() << '\n';
    }

    return 0;
}
