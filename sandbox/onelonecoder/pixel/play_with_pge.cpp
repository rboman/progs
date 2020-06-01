// build & run
// cmake --build . --config Release && Release\play_with_pge.exe

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

class Example : public olc::PixelGameEngine
{
    float radius = 30.0f;
    olc::vf2d centre = {100, 200};

    std::vector<std::pair<std::string, olc::Pixel>> colours;

public:
    Example()
    {
        sAppName = "Example";

        colours.push_back(std::make_pair("GREY", olc::GREY));
        colours.push_back(std::make_pair("DARK_GREY", olc::DARK_GREY));
        colours.push_back(std::make_pair("VERY_DARK_GREY", olc::VERY_DARK_GREY));
        colours.push_back(std::make_pair("RED", olc::RED));
        colours.push_back(std::make_pair("DARK_RED", olc::DARK_RED));
        colours.push_back(std::make_pair("VERY_DARK_RED", olc::VERY_DARK_RED));
        colours.push_back(std::make_pair("YELLOW", olc::YELLOW));
        colours.push_back(std::make_pair("DARK_YELLOW", olc::DARK_YELLOW));
        colours.push_back(std::make_pair("VERY_DARK_YELLOW", olc::VERY_DARK_YELLOW));
        colours.push_back(std::make_pair("GREEN", olc::GREEN));
        colours.push_back(std::make_pair("DARK_GREEN", olc::DARK_GREEN));
        colours.push_back(std::make_pair("VERY_DARK_GREEN", olc::VERY_DARK_GREEN));
        colours.push_back(std::make_pair("CYAN", olc::CYAN));
        colours.push_back(std::make_pair("DARK_CYAN", olc::DARK_CYAN));
        colours.push_back(std::make_pair("VERY_DARK_CYAN", olc::VERY_DARK_CYAN));
        colours.push_back(std::make_pair("BLUE", olc::BLUE));
        colours.push_back(std::make_pair("DARK_BLUE", olc::DARK_BLUE));
        colours.push_back(std::make_pair("VERY_DARK_BLUE", olc::VERY_DARK_BLUE));
        colours.push_back(std::make_pair("MAGENTA", olc::MAGENTA));
        colours.push_back(std::make_pair("DARK_MAGENTA", olc::DARK_MAGENTA));
        colours.push_back(std::make_pair("VERY_DARK_MAGENTA", olc::VERY_DARK_MAGENTA));
        colours.push_back(std::make_pair("WHITE", olc::WHITE));
        colours.push_back(std::make_pair("BLACK", olc::BLACK));
        colours.push_back(std::make_pair("BLANK", olc::BLANK));
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

        if (GetKey(olc::Key::SPACE).bHeld)
            return false; // quit

        // make the circle grow
        if (GetKey(olc::Key::LEFT).bHeld)
            radius -= 100 * fElapsedTime;
        if (GetKey(olc::Key::RIGHT).bHeld)
            radius += 100 * fElapsedTime;
        if (radius < 5.0f)
            radius = 5.0f;
        if (radius > 100.0f)
            radius = 100.0f;

        // move the centre of the circle
        if (GetMouse(0).bHeld)
        {
            centre = {float(GetMouseX()), float(GetMouseY())};
        }

        int width = ScreenWidth();
        int height = ScreenHeight();
        // Draw line
        DrawLine(10, 10, width, height, olc::YELLOW);

        FillRect(20, 10, 20 + 30, 10 + 30, olc::GREEN);

        SetPixelMode(olc::Pixel::ALPHA);
        FillCircle(centre, int32_t(radius), olc::Pixel(0, 255, 255, 150));
        SetPixelMode(olc::Pixel::NORMAL);
        DrawCircle(centre, int32_t(radius), olc::WHITE);

        // draw colour palette

        int ox = 10;
        int oy = 100;
        int w = 14;
        int h = 14;

        for (int i = 0; i < colours.size(); ++i)
        {
            int x1 = ox;
            int y1 = oy + i * h;

            FillRect(x1, y1, w, h, colours[i].second);
            DrawRect(x1, y1, w, h, olc::WHITE);

            DrawString(x1 + 2 * w + 1, y1 + (h - 8) / 2 + 1, colours[i].first, olc::BLACK); // 1char = 8x8
            DrawString(x1 + 2 * w, y1 + (h - 8) / 2, colours[i].first, olc::WHITE);         // 1char = 8x8
        }

        std::string str = "Double";
        uint32_t scale = 2;
        DrawString((width - int(str.size()) * 8 * scale) / 2, height - 20, str, olc::WHITE, scale);

        return true;
    }
};

int main()
{
    Example demo;
    if (demo.Construct(500, 500, 2, 2))
        demo.Start();

    return 0;
}
