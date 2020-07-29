#ifndef GAME_H
#define GAME_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
class Tiles;
class Character;

enum class State
{
    MENU,
    TILEMAP,
    TEST
};

class Game : public olc::PixelGameEngine
{
    Tiles *tiles;
    State state;

    Character *hero;

public:
    Game();
    ~Game();

private:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;

    void testSprites(float fElapsedTime);
};

#endif // GAME_H