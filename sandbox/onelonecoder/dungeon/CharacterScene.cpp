
#include "CharacterScene.h"
#include "Character.h"
#include "TextWindow.h"

CharacterScene::CharacterScene(Tiles *tiles)
{
    // hero = new Character(tiles, "knight_f_idle_anim", "knight_f_run_anim",
    //                      "knight_f_hit_anim");
    // hero = new Character(tiles, "wizzard_f_idle_anim", "wizzard_f_run_anim",
    //                      "wizzard_f_hit_anim");
    hero = new Character(tiles, "big_zombie_idle_anim", "big_zombie_run_anim",
                         "big_zombie_idle_anim");
}

CharacterScene::~CharacterScene()
{
    std::cout << "~CharacterScene()\n";
    if (hero)
        delete hero;
}

void
CharacterScene::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    pge.Clear(olc::BLACK);
    hero->update(pge, fElapsedTime);

    // debug
    TextWindow twin(pge);
    //twin.clear();
    twin.print("<ESC> to quit", HJustify::CENTRE);

    TextWindow win2 = twin.subwin(2, 500, HJustify::RIGHT, VJustify::BOTTOM);
    win2.clear(olc::VERY_DARK_YELLOW);
    win2.print("fElapsedTime = " + std::to_string(fElapsedTime), HJustify::CENTRE);
}

