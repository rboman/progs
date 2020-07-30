#ifndef TILES_H
#define TILES_H

#include "dungeon.h"
#include "Game.h"
#include "Tile.h"

class Tiles
{
    float atime;

public:
    std::map<std::string, Tile> tilemap;  ///< map of tiles
    std::unique_ptr<olc::Sprite> tileimg; ///< sprite/tile image
    std::unique_ptr<olc::Decal> decal;    ///< sprite/tile decal

    Tiles();

    void load();
    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // TILES_H
