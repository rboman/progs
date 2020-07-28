
//
// build & run
// mkdir build
// cmake -A x64 ..
// cmake --build . --config Release && Release\dungeon.exe

#include "Game.h"
#include "Tiles.h"

Game::Game() : tiles(nullptr)
{ 
    sAppName = "Dungeon"; 
}

Game::~Game() 
{ 
    if(tiles) delete tiles; 
}

bool Game::OnUserCreate()
{
    tiles = new Tiles();
    tiles->load();
    return true;
}

bool Game::OnUserUpdate(float fElapsedTime)
{
    tiles->update(*this, fElapsedTime);
    return true;
}
