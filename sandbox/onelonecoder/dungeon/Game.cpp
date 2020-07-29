
//
// build & run
// mkdir build
// cmake -A x64 ..
// cmake --build . --config Release && Release\dungeon.exe

#include "Game.h"
#include "Tiles.h"

class Character
{
public:
    std::string idlename;
    std::string runname;
    std::string hitname;

    Character(std::string const &_idlename, std::string const &_runname,
              std::string const &_hitname)
        : idlename(_idlename), runname(_runname), hitname(_hitname)
    {
    }
};

Game::Game() : tiles(nullptr), state(State::TEST)
{
    sAppName = "Dungeon";
    state = State::TEST;
}

Game::~Game()
{
    if (tiles)
        delete tiles;
}

bool
Game::OnUserCreate()
{
    tiles = new Tiles();
    tiles->load();

    hero = new Character("knight_f_idle_anim", "knight_f_run_anim",
                         "knight_f_hit_anim");

    return true;
}

bool
Game::OnUserUpdate(float fElapsedTime)
{
    if (GetKey(olc::ESCAPE).bPressed)
        state = State::MENU;

    switch (state)
    {
    case State::TEST:
        testSprites(fElapsedTime);
        break;
    case State::MENU:
    case State::TILEMAP:
    default:
        tiles->update(*this, fElapsedTime);
    }

    return true;
}

void
Game::testSprites(float fElapsedTime)
{
    // Erase previous frame
    Clear(olc::BLACK);

    Tile &tl = tiles->tilemap[hero->runname];

    // void DrawPartialDecal(const olc::vf2d &pos, olc::Decal *decal,
    //                       const olc::vf2d &source_pos,
    //                       const olc::vf2d &source_size,
    //                       const olc::vf2d &scale = {1.0f, 1.0f},
    //                       const olc::Pixel &tint = olc::WHITE);

    DrawPartialDecal({100.f, 100.f}, tiles->decal.get(),
                     {(float)tl.ox, (float)tl.oy}, 
                     {(float)tl.w, (float)tl.h},
                     {-3.f, -3.f}, olc::WHITE);
}