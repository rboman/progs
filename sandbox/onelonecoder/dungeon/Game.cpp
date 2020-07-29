
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

    float speed = 200.0f;

    bool move = false;

    // moving left
    if (GetKey(olc::Key::LEFT).bHeld || GetKey(olc::Key::Q).bHeld)
    {
        move = true;
        hero->scale.x = -abs(hero->scale.x);
        hero->pos += { -speed * fElapsedTime, 0.0f }; 
        if (hero->pos.x < 0.0f) hero->pos.x = 0.0f;
        if (hero->action != Action::RUNNING)
        {
            hero->action = Action::RUNNING;
            hero->atime = 0.0f;
        }
    }

    // moving right
    if (GetKey(olc::Key::RIGHT).bHeld || GetKey(olc::Key::D).bHeld)
    {
        move = true;
        hero->scale.x = abs(hero->scale.x);
        hero->pos += { speed * fElapsedTime, 0.0f };
        if(hero->pos.x > ScreenWidth()) hero->pos.x = (float)ScreenWidth();
        if (hero->action != Action::RUNNING)
        {
            hero->action = Action::RUNNING;
            hero->atime = 0.0f;
        }
    }

    // moving up
    if (GetKey(olc::Key::UP).bHeld || GetKey(olc::Key::Z).bHeld)
    {
        move = true;
        hero->pos += { 0.0f, -speed * fElapsedTime };
        if(hero->pos.y < 0.0f) hero->pos.y = 0.0f;
        if (hero->action != Action::RUNNING)
        {
            hero->action = Action::RUNNING;
            hero->atime = 0.0f;
        }
    }

    // moving down
    if (GetKey(olc::Key::DOWN).bHeld || GetKey(olc::Key::S).bHeld)
    {
        move = true;
        hero->pos += { 0.0f, speed * fElapsedTime };
        if(hero->pos.y > ScreenHeight()) hero->pos.y = (float)ScreenHeight();
        if (hero->action != Action::RUNNING)
        {
            hero->action = Action::RUNNING;
            hero->atime = 0.0f;
        }
    }

    // hit
    if (GetKey(olc::Key::SPACE).bHeld)
    {
        move = true;
        if (hero->action != Action::HIT)
        {
            hero->action = Action::HIT;
            hero->atime = 0.0f;
        }
    }

    // idle mode
    if (!move && hero->action != Action::IDLE)
    {
        hero->action = Action::IDLE;
        hero->atime = 0.0f;
    }        

    // choose tile according to action
    Tile *tl = nullptr;
    switch(hero->action)
    {
        case Action::RUNNING:
            tl = &(tiles->tilemap[hero->runname]); break;
        case Action::HIT:
            tl = &(tiles->tilemap[hero->hitname]); break;
        case Action::IDLE:
        default:
            tl = &(tiles->tilemap[hero->idlename]); break;
    }

    // if decal is x-inverted, the ref point is the right corner
    olc::vf2d pos = hero->pos;
    if(hero->scale.x<0.0f) pos.x -= tl->w * hero->scale.x;

    // choose which frame to display
    hero->atime = hero->atime + fElapsedTime;
    int ni = tl->ni;
    int frame = int(hero->atime * 10) % ni;

    // draw decal
    DrawPartialDecal(pos, tiles->decal.get(),
                     {(float)(tl->ox + frame * tl->w), (float)tl->oy}, 
                     {(float)tl->w, (float)tl->h},
                     hero->scale, olc::WHITE);


}