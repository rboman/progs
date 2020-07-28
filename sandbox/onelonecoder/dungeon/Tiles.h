#ifndef TILES_H
#define TILES_H

#include "Game.h"

struct Tile
{
    int ox, oy, w, h, ni;
};

class Tiles
{
    std::map<std::string, Tile> tilemap;        ///< map of tiles    - rename tiles
    std::unique_ptr<olc::Sprite> tileimg;     ///< sprite/tile image - rename tilemap or tileimg

    float atime;

public:
    Tiles();

    void load();
    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // TILES_H
