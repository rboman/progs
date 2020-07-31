#include "Tiles.h"

Tiles::Tiles() : atime(0.0f) {}

void
Tiles::load()
{
    // load
    std::cout << "loading tiles...\n";
    tileimg = std::make_unique<olc::Sprite>(std::string(CMAKE_SOURCE_DIR) +
                                            "/tiles.png");
    decal = std::make_unique<olc::Decal>(tileimg.get());
    std::cout << tileimg.get()->width << 'x' << tileimg.get()->height << '\n';

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
            tilemap[name] = {ox, oy, w, h, ni};
        }
    }

    // print sprite map (c++17)
    // for (auto const &[key, val] : tilemap)
    // {
    //     std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
    //               << ", " << val.h << ", " << val.ni << '\n';
    // }

    // return true;
}

void
Tiles::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // Erase previous frame
    pge.Clear(olc::BLACK);

    // draw the tile image
    pge.DrawSprite(0, 0, tileimg.get());

    // get mouse coordinates
    int mx = pge.GetMouseX();
    int my = pge.GetMouseY();

    // draw all the tiles data
    std::string const *in = nullptr;
    for (auto const &[key, val] : tilemap)
    {
        pge.DrawRect(val.ox, val.oy, val.w, val.h, olc::WHITE);
        // std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
        //           << ", " << val.h << ", " << val.ni << '\n';
        if (mx > val.ox && mx < val.ox + val.w && my > val.oy &&
            my < val.oy + val.h)
        {
            in = &key;
        }
    }

    if (in)
    {
        // draw anim boxes
        Tile &val = tilemap[*in];
        for (int i = 0; i < val.ni; ++i)
            pge.DrawRect(val.ox + i * val.w, val.oy, val.w, val.h,
                         olc::DARK_RED);

        // draw string box
        int charSz = 9;
        int bord = 2;
        int posx = mx - bord;
        int posy = my - bord + charSz;
        int w = (int)in->length() * charSz + 2 * bord;
        int h = charSz + 2 * bord;
        if (posx + w > pge.ScreenWidth())
            posx = pge.ScreenWidth() - w;
        if (posy + h > pge.ScreenHeight())
            posx = pge.ScreenHeight() - h;
        pge.SetPixelMode(olc::Pixel::ALPHA);
        pge.FillRect(posx, posy, w, h, olc::Pixel(0, 0, 0, 170));
        pge.SetPixelMode(olc::Pixel::NORMAL);
        pge.DrawString(posx + bord, posy + bord, *in, olc::WHITE);

        atime = atime + fElapsedTime;
        int ni = val.ni;
        int frame = int(atime * 10) % ni;

        // draw sprite
        int scale = 3;
        int px = pge.ScreenWidth() - scale * val.w - 10;
        int py = pge.ScreenHeight() - scale * val.h - 10;

        pge.DrawPartialSprite(px, py, tileimg.get(), val.ox + frame * val.w,
                              val.oy, val.w, val.h, scale);
    }
    else
    {
        atime = 0.0;
    }
}