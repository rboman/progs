#ifndef CHARACTER_H
#define CHARACTER_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
class Tiles;

enum class Action
{
    IDLE,
    RUNNING,
    HIT
};

class Character
{
public:
    std::string idlename;
    std::string runname;
    std::string hitname;

    olc::vf2d pos;
    olc::vf2d scale;
    float atime;
    Action action;

    Character(std::string const &_idlename, std::string const &_runname,
              std::string const &_hitname);

    void update(olc::PixelGameEngine &pge, Tiles *tiles, float fElapsedTime);
};

#endif // CHARACTER_H