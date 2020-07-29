
//
// build & run
// mkdir build
// cmake -A x64 ..
// cmake --build . --config Release && Release\dungeon.exe

#include "Game.h"
#include "Tiles.h"

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
              std::string const &_hitname)
        : idlename(_idlename), runname(_runname), hitname(_hitname)
    {
        pos = {100.0f, 100.0f};
        scale = {3.0f, 3.0f};
        atime = 0.0f;
        action = Action::IDLE;
    }

    void update(olc::PixelGameEngine &pge, Tiles *tiles, float fElapsedTime);

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
    if (hero)
        delete hero;
}

bool
Game::OnUserCreate()
{
    tiles = new Tiles();
    tiles->load();

    hero = new Character("knight_f_idle_anim", "knight_f_run_anim",
                         "knight_f_hit_anim");
    // hero = new Character("wizzard_f_idle_anim", "wizzard_f_run_anim",
    //                      "wizzard_f_hit_anim");
    // hero = new Character("big_zombie_idle_anim", "big_zombie_run_anim",
    //                      "big_zombie_idle_anim");

    return true;
}

bool
Game::OnUserUpdate(float fElapsedTime)
{
    if (GetKey(olc::ESCAPE).bPressed)
        return false;


    if (GetKey(olc::F1).bPressed)
        state = State::MENU;
    if (GetKey(olc::F2).bPressed)
        state = State::TILEMAP;
    if (GetKey(olc::F3).bPressed)
        state = State::TEST;

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

    hero->update(*this, tiles, fElapsedTime);
}

void 
Character::update(olc::PixelGameEngine &pge, Tiles *tiles, float fElapsedTime)
{
    float speed = 200.0f;

    bool move = false;

    // moving left
    if (pge.GetKey(olc::Key::LEFT).bHeld || pge.GetKey(olc::Key::Q).bHeld)
    {
        move = true;
        scale.x = -abs(scale.x);
        pos += { -speed * fElapsedTime, 0.0f }; 
        if (pos.x < 0.0f) pos.x = 0.0f;
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving right
    if (pge.GetKey(olc::Key::RIGHT).bHeld || pge.GetKey(olc::Key::D).bHeld)
    {
        move = true;
        scale.x = abs(scale.x);
        pos += { speed * fElapsedTime, 0.0f };
        if(pos.x > pge.ScreenWidth()) pos.x = (float)pge.ScreenWidth();
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving up
    if (pge.GetKey(olc::Key::UP).bHeld || pge.GetKey(olc::Key::Z).bHeld)
    {
        move = true;
        pos += { 0.0f, -speed * fElapsedTime };
        if(pos.y < 0.0f) pos.y = 0.0f;
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving down
    if (pge.GetKey(olc::Key::DOWN).bHeld || pge.GetKey(olc::Key::S).bHeld)
    {
        move = true;
        pos += { 0.0f, speed * fElapsedTime };
        if(pos.y > pge.ScreenHeight()) pos.y = (float)pge.ScreenHeight();
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // hit
    if (pge.GetKey(olc::Key::SPACE).bHeld)
    {
        move = true;
        if (action != Action::HIT)
        {
            action = Action::HIT;
            atime = 0.0f;
        }
    }

    // idle mode
    if (!move && action != Action::IDLE)
    {
        action = Action::IDLE;
        atime = 0.0f;
    }        

    // choose tile according to action
    Tile *tl = nullptr;
    switch(action)
    {
        case Action::RUNNING:
            tl = &(tiles->tilemap[runname]); break;
        case Action::HIT:
            tl = &(tiles->tilemap[hitname]); break;
        case Action::IDLE:
        default:
            tl = &(tiles->tilemap[idlename]); break;
    }

    // if decal is x-inverted, the ref point is the right corner
    olc::vf2d dpos = pos;
    if(scale.x<0.0f) dpos.x -= tl->w * scale.x;

    // choose which frame to display
    atime = atime + fElapsedTime;
    int ni = tl->ni;
    int frame = int(atime * 10) % ni;

    // draw decal
    pge.DrawPartialDecal(dpos, tiles->decal.get(),
                     {(float)(tl->ox + frame * tl->w), (float)tl->oy}, 
                     {(float)tl->w, (float)tl->h},
                     scale, olc::WHITE);


}