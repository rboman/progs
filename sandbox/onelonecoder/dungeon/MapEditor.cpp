#include "MapEditor.h"
#include "Tiles.h"
#include "Tile.h"

MapEditor::MapEditor(Tiles *_tiles)
{
    tiles = _tiles;
    mapsize = { 120, 80 };
    offset = { 20.0f, 20.0f };

    floor.resize(mapsize.x*mapsize.y);

    floor_tiles["floor_1"] = &(tiles->tilemap["floor_1"]);
    floor_tiles["floor_2"] = &(tiles->tilemap["floor_2"]);
    floor_tiles["floor_3"] = &(tiles->tilemap["floor_3"]);
    floor_tiles["floor_4"] = &(tiles->tilemap["floor_4"]);
    floor_tiles["floor_5"] = &(tiles->tilemap["floor_5"]);
    floor_tiles["floor_6"] = &(tiles->tilemap["floor_6"]);
    floor_tiles["floor_7"] = &(tiles->tilemap["floor_7"]);
    floor_tiles["floor_8"] = &(tiles->tilemap["floor_8"]);

    for(auto &t : floor)
        t = floor_tiles["floor_1"];

    // put random floor tiles

    for(int i=0; i<floor.size()/10; ++i)
    {
        int posx = rand() % mapsize.x;
        int posy = rand() % mapsize.y;
        int type = rand() % 5;

        auto it = floor_tiles.begin();
        for(int j=0; j<type; ++j) ++it;

        //std::cout <<posx << "," << posy << ", " << posx*mapsize.x + posy << "<" <<  mapsize.x*mapsize.y    << std::endl;

        floor[posy*mapsize.x + posx] = it->second;
    }
    //std::cout << "done." << std::endl;
}

MapEditor::~MapEditor() 
{

}

void
MapEditor::update(olc::PixelGameEngine &pge, float fElapsedTime)
{

    pge.Clear(olc::BLACK);

    int scrollspeed = 300;

    // scrolling
    if (pge.GetKey(olc::Key::LEFT).bHeld)
        offset.x -= scrollspeed*fElapsedTime;
    if (pge.GetKey(olc::Key::RIGHT).bHeld)
        offset.x += scrollspeed*fElapsedTime;
    if (pge.GetKey(olc::Key::UP).bHeld)
        offset.y -= scrollspeed*fElapsedTime;
    if (pge.GetKey(olc::Key::DOWN).bHeld)
        offset.y += scrollspeed*fElapsedTime;

    // draw the map
    int w = floor_tiles.begin()->second->w;
    int h = floor_tiles.begin()->second->h;


    int j1 = -offset.x / w; if(j1<0) j1 = 0;
    int j2 = 1+ (pge.ScreenWidth()-offset.x) / w; if(j2>mapsize.x) j2 = mapsize.x;
    int i1 = -offset.y / h; if(i1<0) i1 = 0;
    int i2 = 1+ (pge.ScreenHeight()-offset.y) / h; if(i2>mapsize.y) i2 = mapsize.y;


    for(int i=i1; i<i2; ++i)    
        for(int j=j1; j<j2; ++j) 
        {
            Tile *t = floor[i*mapsize.x+j];
            if(t)
            {
                olc::vi2d pos =  { (int)(offset.x + t->w *j), (int)(offset.y  + t->h *i)};
                pge.DrawPartialSprite(pos, tiles->tileimg.get(), {t->ox, t->oy}, {t->h, t->w});
            }
        }   

    // draw the mouse cursor

    // get mouse coordinates
    int mx = pge.GetMouseX();
    int my = pge.GetMouseY();





}
