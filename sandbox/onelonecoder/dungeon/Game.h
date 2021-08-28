#ifndef GAME_H
#define GAME_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

/// This "game" contains several programs (scenes) tha can be displayed 
/// by pressing <F1>, <F2>, etc.
enum class Scene
{
    CHARACTER,      ///< Character scene <F1>
    TILES,          ///< Tile Inspector <F2>
    MAPEDITOR,      ///< Map Editor <F3>
    TEXTWINDOW      ///< TextWindow demo <F4>
};

class Game : public olc::PixelGameEngine
{
    Tiles *tiles;   ///< assets as well as TileScene
    Scene scene;    ///< current scene to be displayed

    // scenes
    CharacterScene *charscene;
    MapEditor *mapeditor;
    TextWindowScene *twinscene;

public:
    Game();
    ~Game();

private:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif // GAME_H