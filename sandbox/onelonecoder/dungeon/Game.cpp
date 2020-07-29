
#include "Game.h"
#include "Tiles.h"
#include "Character.h"

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

    // hero = new Character("knight_f_idle_anim", "knight_f_run_anim",
    //                      "knight_f_hit_anim");
    // hero = new Character("wizzard_f_idle_anim", "wizzard_f_run_anim",
    //                      "wizzard_f_hit_anim");
    hero = new Character("big_zombie_idle_anim", "big_zombie_run_anim",
                         "big_zombie_idle_anim");

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
