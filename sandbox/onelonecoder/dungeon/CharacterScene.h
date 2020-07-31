#ifndef CHARACTERSCENE_H
#define CHARACTERSCENE_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

class CharacterScene
{
    Character *hero;
    Character *monster;

    std::map<std::string, Character*> characters;
    std::map<std::string, Character*>::iterator heroit;

    std::string message;

public:
    CharacterScene(Tiles *tiles);
    ~CharacterScene();

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // CHARACTERSCENE_H