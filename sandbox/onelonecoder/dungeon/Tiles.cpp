#include "Tiles.h"
#include "TextWindow.h"

Tiles::Tiles() : atime(0.0f) {}

void
Tiles::load()
{
    // load all the titles from "tiles.png"
    std::cout << "loading tiles.png...\n";
    tileimg = std::make_unique<olc::Sprite>(std::string(CMAKE_SOURCE_DIR) +
                                            "/tiles.png");
    decal = std::make_unique<olc::Decal>(tileimg.get());
    std::cout << tileimg.get()->width << 'x' << tileimg.get()->height << '\n';

    // load asset positions
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
}

/// print the sprite map to a text stream
void
Tiles::print(std::ostream &out)
{
    for (auto const &[key, val] : tilemap) // c++17
    {
        out << key << ':' << val.ox << ", " << val.oy << ", " << val.w
            << ", " << val.h << ", " << val.ni << '\n';
    }
}

void
Tiles::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // Erase previous frame
    pge.Clear(olc::BLUE);   // background colour

    olc::vd2d origin = {5, 30}; // position of the top left corner

    // ...draw the tile image over a grid

    // draw the black/grey grid 
    int gridsz = 8;    
    pge.FillRect(origin.x, origin.y, tileimg.get()->width, tileimg.get()->height, olc::BLACK);
    for (int i = 0; i < tileimg.get()->width / gridsz; ++i)
        for (int j = 0; j < tileimg.get()->height / gridsz; ++j)
        {
            olc::Pixel col;
            if ((i + j) % 2)
                pge.FillRect(origin.x + j * gridsz, origin.y + i * gridsz, gridsz, gridsz, olc::VERY_DARK_GREY);
        }

    // draw the tile image with transparent background
    pge.SetPixelMode(olc::Pixel::ALPHA);
    pge.DrawSprite(origin.x, origin.y, tileimg.get());
    pge.SetPixelMode(olc::Pixel::NORMAL);

    // get mouse coordinates
    int mx = pge.GetMouseX();
    int my = pge.GetMouseY();

    std::string const *in_name = nullptr; // asset currently pointed by the mouse

    // draw all the asset frames 
    for (auto const &[key, val] : tilemap)
    {
        pge.DrawRect(origin.x + val.ox, origin.y + val.oy, val.w, val.h, olc::WHITE);
        // std::cout << key << ':' << val.ox << ", " << val.oy << ", " << val.w
        //           << ", " << val.h << ", " << val.ni << '\n';
        // test which asset is pointed by the mouse
        if (mx > origin.x + val.ox && mx < origin.x + val.ox + val.w &&
            my > origin.y + val.oy && my < origin.y + val.oy + val.h)
        {
            in_name = &key;
        }
    }

    if (in_name) // if an asset is selected
    {
        // draw anim boxes
        Tile &val = tilemap[*in_name];
        for (int i = 0; i < val.ni; ++i)
            pge.DrawRect(origin.x + val.ox + i * val.w, origin.y + val.oy, val.w, val.h,
                         olc::DARK_RED);

        // draw a floating string box with the asset info
        std::vector<std::string> text; // lines of text to be displayed
        text.push_back(*in_name);
        text.push_back("origin=" + std::to_string(val.ox) + "x" + std::to_string(val.oy));
        text.push_back("size=" + std::to_string(val.h) + "x" + std::to_string(val.w));
        
        int charSz = 8; // character width/height (this is a constant from PGE)
        int bord = 2;   // top/bottom/left/right margin
        int charsep = 2; // additional line sep
        int posx = mx - bord;
        int posy = my - bord + charSz;

        // compute wmax - the max width among lines of text
        int wmax = 0;
        for (auto &s : text)
            if (s.length() > wmax)
                wmax = s.length();

        // compute box size from text
        int w = wmax * charSz + 2 * bord; // final width of the box
        int h = charSz * text.size() + 2 * bord + text.size() * charsep; // final height of the box
        if (posx + w > pge.ScreenWidth())
            posx = pge.ScreenWidth() - w;
        if (posy + h > pge.ScreenHeight())
            posx = pge.ScreenHeight() - h;

        // draw the box & text
        pge.SetPixelMode(olc::Pixel::ALPHA);
        pge.FillRect(posx, posy, w, h, olc::Pixel(0, 0, 0, 170));
        pge.SetPixelMode(olc::Pixel::NORMAL);
        for (int i = 0; i < text.size(); ++i)
            pge.DrawString(posx + bord, posy + bord + i * (charSz + charsep), text[i], olc::WHITE);


        // animation timer
        atime = atime + 10 * fElapsedTime;
        int ni = val.ni;
        int frame = int(atime) % ni;
        if (atime > ni)
            atime -= ni;

        // draw the animated sprite
        int scale = 3;
        int px = pge.ScreenWidth() - scale * val.w - 10;
        int py = pge.ScreenHeight() - scale * val.h - 50;

        pge.DrawPartialSprite(px, py, tileimg.get(), val.ox + frame * val.w,
                              val.oy, val.w, val.h, scale);
    }
    else
    {
        atime = 0.0; // reset the animation timer
    }

    // draw a small yellow crosshair

    // relative mouse position in the tile map
    int rmx = mx - origin.x;
    int rmy = my - origin.y;
    // nearest grid point
    int gx = std::round((float)rmx / gridsz) * gridsz;
    int gy = std::round((float)rmy / gridsz) * gridsz;
    int crosswidth = 1;

    if (rmx >= 0 && rmx < tileimg.get()->width && rmy >= 0 && rmy < tileimg.get()->height)
    {
        pge.DrawLine({(int)origin.x + gx - crosswidth, (int)origin.y + gy}, {(int)origin.x + gx + crosswidth, (int)origin.y + gy}, olc::YELLOW);
        pge.DrawLine({(int)origin.x + gx, (int)origin.y + gy - crosswidth}, {(int)origin.x + gx, (int)origin.y + gy + crosswidth}, olc::YELLOW);

        message = "position = " + std::to_string(rmx) + "," + std::to_string(rmy) + " - nearest grid point = " + std::to_string(gx) + "," + std::to_string(gy);
    }
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

