//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\sweep_and_prune.exe

//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

struct Box
{
    Box(int i, olc::vf2d const &c, float Lx, float Ly)
    {
        pmin = c;
        pmax = c + olc::vf2d(Lx, Ly);
        id = i; 
    }
    olc::vf2d pmin;
    olc::vf2d pmax;
    int id;
};


class SweepAndPrune : public olc::PixelGameEngine
{
    std::vector<Box*> boxes;

public:
    SweepAndPrune() { sAppName = "Sweep and Prune"; }

public:
    bool OnUserCreate() override
    {
        int maxLx = 50;
        int maxLy = 50;
        int minLx = 20;
        int minLy = 20;


        for(int i=0; i<10; ++i)
            boxes.push_back(new Box(i, {float(rand() % (ScreenWidth()-maxLx)), float(rand() % (ScreenHeight()-maxLy))}, 
            rand() % (maxLx-minLx)+minLx, rand() % (maxLy-minLy)+minLy ) );
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        Clear(olc::DARK_BLUE);

        for(auto b : boxes)
        {
            DrawRect(b->pmin, b->pmax-b->pmin, olc::WHITE);
            DrawString( (b->pmin+b->pmax)/2-olc::vi2d(3,3), std::to_string(b->id), olc::YELLOW );
        }

        return true;
    }
};

int
main()
{
    SweepAndPrune demo;
    if (demo.Construct(256, 240, 2, 2))
        demo.Start();

    return 0;
}
