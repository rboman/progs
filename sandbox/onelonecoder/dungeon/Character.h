#ifndef CHARACTER_H
#define CHARACTER_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
#include "Tile.h"
class Tiles;

enum class State
{
    IDLE,
    RUNNING,
    HIT
};

class Character
{
    Tiles *tiles;
    Tile *tl = nullptr;
public:
    std::string idlename;
    std::string runname;
    std::string hitname;

    olc::vf2d pos;
    olc::vf2d scale;
    float basespeed;        ///< walking basespeed
    float atime;            ///< animation time
    State state;           ///< current state
    olc::vf2d velocity;     ///< current velocity

    Character(Tiles *_tiles, std::string const &_idlename,
              std::string const &_runname, std::string const &_hitname);

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
    void userKeys(olc::PixelGameEngine &pge, float fElapsedTime);

    olc::vf2d opposite() const { return pos + olc::vf2d{tl->w * std::abs(scale.x), tl->h * scale.y}; }

};

#endif // CHARACTER_H