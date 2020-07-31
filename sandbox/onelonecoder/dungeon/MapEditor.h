#ifndef MAPEDITOR_H
#define MAPEDITOR_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

class MapEditor
{
    Tiles *tiles;

    olc::vi2d mapsize;
    std::vector<Tile *> floor;


    std::map<std::string, Tile *> floor_tiles;

public:
    MapEditor(Tiles *_tiles);
    ~MapEditor();

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // MAPEDITOR_H