#ifndef MAPEDITOR_H
#define MAPEDITOR_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

class MapEditor
{
    Tiles *tiles; ///< a ptr to tile data

    olc::vi2d mapsize;         ///< size of the map
    std::vector<Tile *> floor; ///< the map
    olc::vf2d offset;          ///< current display offset

    std::map<std::string, Tile *> floor_tiles; ///< tiles for the floor

    int scrollspeed = 300;
    Tile *brush;         ///< current selected brush
    std::string message; ///< message to be displayed in the bottom window

public:
    MapEditor(Tiles *_tiles);
    ~MapEditor();

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // MAPEDITOR_H