
#include "Game.h"
#include "Tiles.h"
#include "CharacterScene.h"
#include "MapEditor.h"

Game::Game()
{
    sAppName = "Dungeon";
    tiles = nullptr;
    charscene = nullptr;
    mapeditor = nullptr;
    scene = Scene::MAPEDITOR;
}

Game::~Game()
{
    std::cout << "~Game()\n";
    if (tiles)
        delete tiles;
    if (charscene)
        delete charscene;
    if (mapeditor)
        delete mapeditor;
}

bool
Game::OnUserCreate()
{
    // init random numbers
    srand (static_cast <unsigned> (time(0)));

    // load tiles
    tiles = new Tiles();
    tiles->load();

    // create the scenes
    charscene = new CharacterScene(tiles);
    mapeditor = new MapEditor(tiles);
    return true;
}

/// main update function
/// management of scenes

bool
Game::OnUserUpdate(float fElapsedTime)
{
    // quit if ESC is pressed
    if (GetKey(olc::ESCAPE).bPressed)
        return false;

    // switch between scenes with F* keys
    if (GetKey(olc::F1).bPressed)
        scene = Scene::CHARACTER;
    if (GetKey(olc::F2).bPressed)
        scene = Scene::TILES;
    if (GetKey(olc::F3).bPressed)
        scene = Scene::MAPEDITOR;

    switch (scene)
    {
    case Scene::CHARACTER:
        charscene->update(*this, fElapsedTime);
        break;
    case Scene::MAPEDITOR:
        mapeditor->update(*this, fElapsedTime);
        break;
    case Scene::TILES:
    default:
        tiles->update(*this, fElapsedTime);
    }

    return true;
}
