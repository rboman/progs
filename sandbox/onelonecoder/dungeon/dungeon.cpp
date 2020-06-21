
//
// build & run
// mkdir build
// cmake -A x64 ..
// cmake --build . --config Release && Release\dungeon.exe

#include "config.h"
//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"
#include <sstream>
#include <iostream>
#include <map>

struct Spr
{
    int ox, oy, w, h, ni;
};

class Dungeon : public olc::PixelGameEngine
{
    std::map<std::string, Spr> sprs;
    std::unique_ptr<olc::Sprite> tiles;

public:
    Dungeon() { sAppName = "Dungeon"; }

private:
    bool OnUserCreate() override
    {
        // load
        std::cout << "loading tiles...\n";
        tiles = std::make_unique<olc::Sprite>(std::string(CMAKE_SOURCE_DIR) +
                                              "/tiles.png");
        std::cout << tiles.get()->width << 'x' << tiles.get()->height << '\n';

        // load assets
        std::cout << "loading asset positions...\n";
        std::ifstream infile(std::string(CMAKE_SOURCE_DIR) + "/tiles.txt");
        std::string line;
        while (std::getline(infile, line))
        {
            // std::cout << line << '\n';
            std::istringstream iss(line);
            std::string name;
            iss >> name;
            if (name.length())
            {
                int ox, oy, w, h;
                iss >> ox >> oy >> w >> h;
                int ni;
                iss >> ni;
                if (!iss)
                    ni = 1;
                // std::cout << "name=" << name << ", " << ox << ", " << oy <<
                // ", "
                //           << w << ", " << h << ", " << ni << '\n';
                sprs[name] = {ox, oy, w, h, ni};
            }
        }

        // print sprite map (c++17)
        // for (auto const &[key, val] : sprs)
        // {
        //     std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
        //               << ", " << val.h << ", " << val.ni << '\n';
        // }

        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::BLACK);

        // draw tiles
        DrawSprite(0, 0, tiles.get());

        // get mouse
        int mx = GetMouseX(); 
        int my = GetMouseY();

        // draw all the tiles data
        std::string const*in = nullptr;
        for (auto const &[key, val] : sprs)
        {
            DrawRect(val.ox, val.oy, val.w, val.h, olc::WHITE);
            // std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
            //           << ", " << val.h << ", " << val.ni << '\n';
            if (mx>val.ox && mx<val.ox+val.w && my>val.oy && my<val.oy+val.h)
            {
                in = &key;
            }
        }

        if(in)
        {
            // draw anim boxes
            Spr val = sprs[*in];
            for(int i=0; i<val.ni; ++i)
                DrawRect(val.ox+i*val.w, val.oy, val.w, val.h, olc::DARK_RED);

            // draw string box
            int charSz = 9;
            int bord = 2;
            int posx = mx-bord;
            int posy = my-bord+charSz;
            int w = in->length()*charSz+2*bord;
            int h = charSz+2*bord;
            if(posx+w>ScreenWidth())
                posx = ScreenWidth()-w;
            if(posy+h>ScreenHeight())
                posx = ScreenHeight()-h;
            SetPixelMode(olc::Pixel::ALPHA);                
            FillRect(posx, posy, w, h, olc::Pixel(0, 0, 0, 170));
            SetPixelMode(olc::Pixel::NORMAL);
            DrawString(posx+bord, posy+bord, *in, olc::WHITE);
        }


        return true;
    }
};

int
main()
{
    Dungeon demo;
    if (demo.Construct(512, 512, 2, 2))
        demo.Start();

    return 0;
}
