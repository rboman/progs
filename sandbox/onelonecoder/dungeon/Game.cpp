
#include "Game.h"
#include "Tiles.h"
#include "CharacterScene.h"
#include "MapEditor.h"
#include "TextWindowScene.h"

Game::Game()
{
    sAppName = "Dungeon";
    tiles = nullptr;
    charscene = nullptr;
    mapeditor = nullptr;
    twinscene = nullptr;
    scene = Scene::CHARACTER;
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
    if (twinscene)
        delete twinscene;
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
    twinscene = new TextWindowScene();
    return true;
}

/// main update function
/// management of scenes

bool
Game::OnUserUpdate(float fElapsedTime)
{
    // quit if ESC is pressed
    if (GetKey(olc::Key::ESCAPE).bPressed)
        return false;

    // switch between scenes with <Fx> keys
    if (GetKey(olc::Key::F1).bPressed)
        scene = Scene::CHARACTER;
    if (GetKey(olc::Key::F2).bPressed)
        scene = Scene::TILES;
    if (GetKey(olc::Key::F3).bPressed)
        scene = Scene::MAPEDITOR;
    if (GetKey(olc::Key::F4).bPressed)
        scene = Scene::TEXTWINDOW;

    switch (scene)
    {
    case Scene::CHARACTER:
        charscene->update(*this, fElapsedTime);
        break;
    case Scene::MAPEDITOR:
        mapeditor->update(*this, fElapsedTime);
        break;
    case Scene::TEXTWINDOW:
        twinscene->update(*this, fElapsedTime);
        break;
    case Scene::TILES:
    default:
        tiles->update(*this, fElapsedTime);
    }

    return true;
}
