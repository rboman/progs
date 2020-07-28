#ifndef GAME_H
#define GAME_H


#include "config.h"
#include "olcPixelGameEngine.h"
class Tiles;

enum class State
{
    MENU,
    TILEMAP
};


class Game : public olc::PixelGameEngine
{
    Tiles *tiles;
    State state;

public:
    Game();
    ~Game();

private:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif //GAME_H