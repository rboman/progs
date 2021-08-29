#ifndef CHARACTER_H
#define CHARACTER_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
#include "Tile.h"
class Tiles;

class Character
{
    Tiles *tiles;       ///< tiles data
    Tile *tl = nullptr; ///< current tile
public:
    Tile *tile_idle;
    Tile *tile_run;
    Tile *tile_hit;

    enum class State
    {
        IDLE,
        RUNNING,
        HIT
    };

public:
    olc::vf2d pos;      ///< reference position upper-left corner
    olc::vf2d scale;    ///< drawing scale (negative if x-flipped)
    float basespeed;    ///< walking basespeed
    float atime;        ///< animation time
    State state;        ///< current state
    olc::vf2d velocity; ///< current velocity

    Character(Tiles *_tiles, std::string const &_idlename,
              std::string const &_runname, std::string const &_hitname);

    void update(olc::PixelGameEngine &pge, float fElapsedTime);

    // velocity management
    void userKeys(olc::PixelGameEngine &pge, float fElapsedTime);
    void bounce(olc::PixelGameEngine &pge, float fElapsedTime);

    // animation
    void set_state(State newstate);
    void set_tiles(std::string const &_idlename,
                   std::string const &_runname, std::string const &_hitname);

    // positions
    olc::vf2d lowerright() const { return pos + size(); }
    olc::vf2d size() const { return olc::vf2d{width(), height()}; }
    float width() const { return tl->w * std::abs(scale.x); }
    float height() const { return tl->h * scale.y; }

    // debug
    void change(Character const &chr);
};

#endif // CHARACTER_H