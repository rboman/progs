#ifndef TEXTWINDOWSCENE_H
#define TEXTWINDOWSCENE_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

class TextWindowScene
{
public:
    TextWindowScene();
    ~TextWindowScene();

    void update(olc::PixelGameEngine &pge, float fElapsedTime);
};

#endif // TEXTWINDOWSCENE_H
