#include "MapEditor.h"
#include "Tiles.h"
#include "Tile.h"
#include "TextWindow.h"

MapEditor::MapEditor(Tiles *_tiles)
{
    tiles = _tiles;
    mapsize = {120, 80};
    offset = {20.0f, 20.0f};

    floor_tiles["floor_1"] = &(tiles->tilemap["floor_1"]);
    floor_tiles["floor_2"] = &(tiles->tilemap["floor_2"]);
    floor_tiles["floor_3"] = &(tiles->tilemap["floor_3"]);
    floor_tiles["floor_4"] = &(tiles->tilemap["floor_4"]);
    floor_tiles["floor_5"] = &(tiles->tilemap["floor_5"]);
    floor_tiles["floor_6"] = &(tiles->tilemap["floor_6"]);
    floor_tiles["floor_7"] = &(tiles->tilemap["floor_7"]);
    floor_tiles["floor_8"] = &(tiles->tilemap["floor_8"]);
    floor_tiles["floor_ladder"] = &(tiles->tilemap["floor_ladder"]);
    floor_tiles["wall_left"] = &(tiles->tilemap["wall_left"]);
    floor_tiles["wall_mid"] = &(tiles->tilemap["wall_mid"]);
    floor_tiles["wall_right"] = &(tiles->tilemap["wall_right"]);
    floor_tiles["wall_banner_red"] = &(tiles->tilemap["wall_banner_red"]);
    floor_tiles["wall_banner_blue"] = &(tiles->tilemap["wall_banner_blue"]);
    floor_tiles["wall_banner_green"] = &(tiles->tilemap["wall_banner_green"]);
    floor_tiles["wall_banner_yellow"] = &(tiles->tilemap["wall_banner_yellow"]);
    floor_tiles["wall_hole_1"] = &(tiles->tilemap["wall_hole_1"]);
    floor_tiles["wall_hole_2"] = &(tiles->tilemap["wall_hole_2"]);
    floor_tiles["wall_goo"] = &(tiles->tilemap["wall_goo"]);
    floor_tiles["wall_goo_base"] = &(tiles->tilemap["wall_goo_base"]);
    floor_tiles["wall_hole"] = &(tiles->tilemap["wall_hole"]);


    overlay_tiles["wall_top_left"] = &(tiles->tilemap["wall_top_left"]);
    overlay_tiles["wall_top_mid"] = &(tiles->tilemap["wall_top_mid"]);
    overlay_tiles["wall_top_right"] = &(tiles->tilemap["wall_top_right"]);
    overlay_tiles["wall_fountain_top"] = &(tiles->tilemap["wall_fountain_top"]);
    overlay_tiles["column_mid"] = &(tiles->tilemap["column_mid"]);
    overlay_tiles["column_top"] = &(tiles->tilemap["column_mid"]);
    overlay_tiles["column_base"] = &(tiles->tilemap["column_base"]);
    overlay_tiles["wall_side_top_left"] = &(tiles->tilemap["wall_side_top_left"]);
    overlay_tiles["wall_side_mid_left"] = &(tiles->tilemap["wall_side_mid_left"]);
    overlay_tiles["wall_side_front_left"] = &(tiles->tilemap["wall_side_front_left"]);
    overlay_tiles["wall_side_top_right"] = &(tiles->tilemap["wall_side_top_right"]);
    overlay_tiles["wall_side_mid_right"] = &(tiles->tilemap["wall_side_mid_right"]);
    overlay_tiles["wall_side_front_right"] = &(tiles->tilemap["wall_side_front_right"]);


    // INITIAL MAP
    floor.resize(mapsize.x * mapsize.y);
    for (auto &t : floor)
        t = floor_tiles["floor_1"];
    // put random floor tiles
    for (int i = 0; i < floor.size() / 10; ++i)
    {
        int posx = rand() % mapsize.x;
        int posy = rand() % mapsize.y;
        int type = rand() % 8;

        auto it = floor_tiles.begin();
        for (int j = 0; j < type; ++j)
            ++it;
        floor[posy * mapsize.x + posx] = it->second;
    }
    // initialize the foreground layer (overlay)
    overlay.resize(mapsize.x * mapsize.y);
    for (auto &t : overlay)
        t = nullptr;

    brush = nullptr; // current brush is void
}

MapEditor::~MapEditor()
{
}

void
MapEditor::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    pge.Clear(olc::BLACK);
    message = "";

    // scrolling
    if (pge.GetKey(olc::Key::LEFT).bHeld)
        offset.x += scrollspeed * fElapsedTime;
    if (pge.GetKey(olc::Key::RIGHT).bHeld)
        offset.x -= scrollspeed * fElapsedTime;
    if (pge.GetKey(olc::Key::UP).bHeld)
        offset.y += scrollspeed * fElapsedTime;
    if (pge.GetKey(olc::Key::DOWN).bHeld)
        offset.y -= scrollspeed * fElapsedTime;

    // DRAW THE MAP

    int w = floor_tiles.begin()->second->w;
    int h = floor_tiles.begin()->second->h;

    int j1 = (int)(-offset.x / w);
    if (j1 < 0)
        j1 = 0;
    int j2 = 1 + (int)((pge.ScreenWidth() - offset.x) / w);
    if (j2 > mapsize.x)
        j2 = mapsize.x;
    int i1 = (int)(-offset.y / h);
    if (i1 < 0)
        i1 = 0;
    int i2 = 1 + (int)((pge.ScreenHeight() - offset.y) / h);
    if (i2 > mapsize.y)
        i2 = mapsize.y;

    // draw a grid
    for (int i = i1; i < i2; ++i)
        for (int j = j1; j < j2; ++j)
        {
            olc::vi2d pos = {(int)(offset.x + w * j), (int)(offset.y + h * i)};
            pge.DrawRect( pos, {w,h}, olc::VERY_DARK_GREY);
        }

    // draw the floor
    for (int i = i1; i < i2; ++i)
        for (int j = j1; j < j2; ++j)
        {
            Tile *t = floor[i * mapsize.x + j];
            if (t)
            {
                olc::vi2d pos = {(int)(offset.x + t->w * j), (int)(offset.y + t->h * i)};
                pge.DrawPartialSprite(pos, tiles->tileimg.get(), {t->ox, t->oy}, {t->w, t->h});
            }
        }

    // draw the overlay
    // for (int i = i1; i < i2; ++i)
    //     for (int j = j1; j < j2; ++j)
    //     {
    //         Tile *t = overlay[i * mapsize.x + j];
    //         if (t)
    //         {
    //             olc::vi2d pos = {(int)(offset.x + t->w * j), (int)(offset.y + t->h * i)};
    //             pge.DrawPartialSprite(pos, tiles->tileimg.get(), {t->ox, t->oy}, {t->w, t->h});
    //         }
    //     }



    // DRAW TILESET WINDOW
    olc::vi2d winsz = {2*16 + 20, pge.ScreenHeight()}; // tileset window
    pge.FillRect({0, 0}, winsz, olc::BLACK);         // window background

    olc::vi2d tilpos = {10, 20};

    int k = 0;
    for (auto &[name, til] : floor_tiles)
    {
        olc::vi2d pos = {tilpos.x, tilpos.y + k * til->h};
        pge.DrawPartialSprite(pos, tiles->tileimg.get(),
                              {til->ox, til->oy}, {til->w, til->h});
        ++k;
    }

    pge.DrawLine( {winsz.x, 0}, {winsz.x, winsz.y}, olc::VERY_DARK_YELLOW);

    // MOUSE CURSOR

    // get mouse coordinates
    int mx = pge.GetMouseX();
    int my = pge.GetMouseY();

    if (mx < winsz.x && my < winsz.y) // the mouse is over the tileset zone
    {
        if (pge.GetMouse(0).bHeld)
            brush = nullptr;

        k = 0;
        for (auto &[name, til] : floor_tiles)
        {
            if (mx > tilpos.x && mx < tilpos.x + w &&
                my > tilpos.y + k * h && my < tilpos.y + (k + 1) * h)
            {
                pge.DrawRect({tilpos.x, tilpos.y + k * h}, {w, h});
                message = name;
                // select
                if (pge.GetMouse(0).bHeld)
                {
                    brush = til;
                }
                break;
            }
            ++k;
        }
    }
    else // the mouse is over the map
    {
        // transform to map coordinates
        int jm = (mx - offset.x) / w;
        int im = (my - offset.y) / h;
        int idx = im * mapsize.x + jm;

        // get the tile under the mouse
        Tile *t = nullptr;
        if (jm >= 0 && jm < mapsize.x && im >= 0 && im < mapsize.y)
        {
            t = floor[im * mapsize.x + jm];
            message = std::to_string(jm) + "," + std::to_string(im);

            if (pge.GetMouse(0).bHeld)
            {
                floor[im * mapsize.x + jm] = brush;
            }
            // draw a selection box
            pge.DrawRect({(int)(offset.x + w * jm), (int)(offset.y + h * im)}, {w, h});
        }
    }

    // hightlight selected brush
    k = 0;
    for (auto &[name, til] : floor_tiles)
    {
        if (til == brush)
        {
            pge.DrawRect({tilpos.x, tilpos.y + k * h}, {w, h}, olc::GREEN);
            break;
        }
        ++k;
    }

    // TEXT

    // info on top of screen
    TextWindow twin(pge);
    twin.print("MAP EDITOR", HJustify::CENTRE);
    twin.print("<ESC> to quit", HJustify::CENTRE);

    // debug msg
    TextWindow win2 = twin.subwin(2, 500, HJustify::RIGHT, VJustify::BOTTOM);
    win2.clear(olc::VERY_DARK_YELLOW);
    win2.print(message, HJustify::CENTRE);
}
