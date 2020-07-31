#include "MapEditor.h"
#include "Tiles.h"
#include "Tile.h"

MapEditor::MapEditor(Tiles *_tiles)
{
    tiles = _tiles;
    mapsize = { 15, 10 };


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

}


MapEditor::~MapEditor() 
{

}

void
MapEditor::update(olc::PixelGameEngine &pge, float fElapsedTime)
{

    pge.Clear(olc::BLACK);


    for(int i=0; i<mapsize.y; ++i)    
        for(int j=0; j<mapsize.x; ++j) 
        {
            Tile *t = floor[i*mapsize.x+j];
            if(t)
            {
                olc::vi2d pos = { (int)(20 + t->w *j), (int)(20+ t->h *i)};
                pge.DrawPartialSprite(pos, tiles->tileimg.get(), {t->ox, t->oy}, {t->h, t->w});
            }

        }   

}
