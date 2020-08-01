#include "Tiles.h"
#include "TextWindow.h"

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
    pge.Clear(olc::BLUE);

    olc::vd2d origin = {5, 30};

    // draw the tile image
    pge.DrawSprite(origin.x, origin.y, tileimg.get());

    // get mouse coordinates
    int mx = pge.GetMouseX();
    int my = pge.GetMouseY();

    // draw all the tiles data
    std::string const *in_name = nullptr;
    for (auto const &[key, val] : tilemap)
    {
        pge.DrawRect(origin.x + val.ox, origin.y + val.oy, val.w, val.h, olc::WHITE);
        // std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
        //           << ", " << val.h << ", " << val.ni << '\n';
        if (mx > origin.x + val.ox && mx < origin.x + val.ox + val.w &&
            my > origin.y + val.oy && my < origin.y + val.oy + val.h)
        {
            in_name = &key;
        }
    }

    if (in_name)
    {
        // draw anim boxes
        Tile &val = tilemap[*in_name];
        for (int i = 0; i < val.ni; ++i)
            pge.DrawRect(origin.x + val.ox + i * val.w, origin.y + val.oy, val.w, val.h,
                         olc::DARK_RED);

        // draw floating string box
        std::vector<std::string> text;
        text.push_back(*in_name);
        text.push_back("origin="+std::to_string(val.ox)+"x"+std::to_string(val.oy));
        text.push_back("size="+std::to_string(val.h)+"x"+std::to_string(val.w));
        int charSz = 8;
        int bord = 2;
        int charsep = 2;
        int posx = mx - bord;
        int posy = my - bord + charSz;

        int wmax=0;
        for(auto &s : text)
            if (s.length() >wmax) wmax=s.length();

        int w = wmax * charSz + 2 * bord;
        int h = charSz * text.size() + 2 * bord + text.size()*charsep;
        if (posx + w > pge.ScreenWidth())
            posx = pge.ScreenWidth() - w;
        if (posy + h > pge.ScreenHeight())
            posx = pge.ScreenHeight() - h;
        pge.SetPixelMode(olc::Pixel::ALPHA);
        pge.FillRect(posx, posy, w, h, olc::Pixel(0, 0, 0, 170));
        pge.SetPixelMode(olc::Pixel::NORMAL);
        for(int i=0; i<text.size(); ++i)
            pge.DrawString(posx + bord, posy + bord + i*(charSz+charsep), text[i], olc::WHITE);

        atime = atime + 10 * fElapsedTime;
        int ni = val.ni;
        int frame = int(atime) % ni;
        if (atime > ni)
            atime -= ni;

        // draw animated sprite
        int scale = 3;
        int px = pge.ScreenWidth() - scale * val.w - 10;
        int py = pge.ScreenHeight() - scale * val.h - 50;

        pge.DrawPartialSprite(px, py, tileimg.get(), val.ox + frame * val.w,
                              val.oy, val.w, val.h, scale);
    }
    else
    {
        atime = 0.0;
    }

    int rmx = mx - origin.x;
    int rmy = my - origin.y;
    if (rmx >= 0 && rmx < tileimg.get()->width && rmy >= 0 && rmy < tileimg.get()->height)
        message = std::to_string(rmx) + "," + std::to_string(rmy);
    else
        message = "";

    // DEBUG

    // info on top of screen
    TextWindow twin(pge);
    twin.print("TILE INSPECTOR", HJustify::CENTRE);
    twin.print("<ESC> to quit", HJustify::CENTRE);

    // debug msg
    TextWindow win2 = twin.subwin(2, 500, HJustify::RIGHT, VJustify::BOTTOM);
    win2.clear(olc::VERY_DARK_BLUE);
    win2.print(message, HJustify::CENTRE);
}