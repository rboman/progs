#ifndef WEAPON_H
#define WEAPON_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
#include "Tile.h"

class Weapon
{
    Tiles *tiles; ///< tiles data
public:
    Tile *tile;      ///< tile image
    olc::vf2d pos;   ///< reference position upper-left corner
    olc::vf2d scale; ///< drawing scale (negative if x-flipped)

    float angle;

    enum class State
    {
        IDLE,
        HELD,
        THROWN,
        SWUNG
    };

    Weapon(Tiles *_tiles, std::string const &_name);

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
    void set_tile(std::string const &name);
};

#endif // WEAPON_H