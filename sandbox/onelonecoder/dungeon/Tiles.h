#ifndef TILES_H
#define TILES_H

#include "Game.h"

struct Spr
{
    int ox, oy, w, h, ni;
};

class Tiles
{
    std::map<std::string, Spr> sprs;        ///< map of tiles    - rename tiles
    std::unique_ptr<olc::Sprite> tiles;     ///< sprite/tile image - rename tilemap or tileimg

    float atime;

public:
    Tiles();

    void load();
    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // TILES_H
