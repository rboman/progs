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

    CharacterScene *charscene;

public:
    Game();
    ~Game();

private:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif // GAME_H