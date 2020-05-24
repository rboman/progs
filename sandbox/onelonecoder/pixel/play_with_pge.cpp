// build & run
// cmake --build . --config Release && Release\play_with_pge.exe

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

class Example : public olc::PixelGameEngine
{
    float radius = 5.0f;
    olc::vf2d centre = {100, 200};
public:
    Example()
    {
        sAppName = "Example";
    }

public:
    bool OnUserCreate() override
    {
        // Called once at the start, so create things here
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::DARK_BLUE);



        // Handle User Input
        if (GetKey(olc::Key::LEFT).bHeld) radius -= 100 * fElapsedTime;
        if (GetKey(olc::Key::RIGHT).bHeld) radius += 100 * fElapsedTime;
        if (radius<5.0f) radius=5.0f;
        if (radius>100.0f) radius=100.0f;
        
        if (GetMouse(0).bHeld)
        {
            centre = { float(GetMouseX()), float(GetMouseY()) };
        }


        int width = ScreenWidth();
        int height = ScreenHeight();
        // Draw line
        DrawLine(10, 10, width, height, olc::YELLOW);

        FillRect(20, 10, 20+30, 10+30, olc::GREEN);

        FillCircle(centre, radius, olc::CYAN);
        return true;
    }
};

int main()
{
    Example demo;
    if (demo.Construct(512, 480, 2, 2))
        demo.Start();

    return 0;
}
