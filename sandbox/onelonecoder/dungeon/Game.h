#ifndef GAME_H
#define GAME_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"
class Tiles;
class Character;

enum class Scene
{
    CHARACTER,
    TILES,
    MAPEDITOR
};

class Game : public olc::PixelGameEngine
{
    Tiles *tiles;
    Scene scene;

    CharacterScene *charscene;
    MapEditor *mapeditor;

public:
    Game();
    ~Game();

private:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif // GAME_H