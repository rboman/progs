
#include "Game.h"
#include "Tiles.h"
#include "CharacterScene.h"

Game::Game() : tiles(nullptr), state(State::TEST)
{
    sAppName = "Dungeon";
    charscene = nullptr;
    state = State::TEST;
}

Game::~Game()
{
    if (tiles)
        delete tiles;
    if(charscene)delete charscene;
}

bool
Game::OnUserCreate()
{
    tiles = new Tiles();
    tiles->load();

    charscene = new CharacterScene(tiles);

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
        charscene->update(*this, fElapsedTime);
        break;
    case State::MENU:
    case State::TILEMAP:
    default:
        tiles->update(*this, fElapsedTime);
    }

    return true;
}

