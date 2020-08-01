
#include "CharacterScene.h"
#include "Character.h"
#include "TextWindow.h"





CharacterScene::CharacterScene(Tiles *tiles)
{
    message = "press <A> to change the hero";

    // build character map
    characters["elf_f"] = new Character(tiles, "elf_f_idle_anim", "elf_f_run_anim", "elf_f_hit_anim");
    characters["elf_m"] = new Character(tiles, "elf_m_idle_anim", "elf_m_run_anim", "elf_m_hit_anim");
    characters["knight_f"] = new Character(tiles, "knight_f_idle_anim", "knight_f_run_anim", "knight_f_hit_anim");
    characters["knight_m"] = new Character(tiles, "knight_m_idle_anim", "knight_m_run_anim", "knight_m_hit_anim");
    characters["wizard_f"] = new Character(tiles, "wizard_f_idle_anim", "wizard_f_run_anim", "wizard_f_hit_anim");
    characters["wizard_m"] = new Character(tiles, "wizard_m_idle_anim", "wizard_m_run_anim", "wizard_m_hit_anim");
    characters["lizard_f"] = new Character(tiles, "lizard_f_idle_anim", "lizard_f_run_anim", "lizard_f_hit_anim");
    characters["lizard_m"] = new Character(tiles, "lizard_m_idle_anim", "lizard_m_run_anim", "lizard_m_hit_anim");
    characters["big_zombie"] = new Character(tiles, "big_zombie_idle_anim", "big_zombie_run_anim", "big_zombie_idle_anim");
    characters["ogre"] = new Character(tiles, "ogre_idle_anim", "ogre_run_anim", "ogre_idle_anim");
    characters["big_demon"] = new Character(tiles, "big_demon_idle_anim", "big_demon_run_anim", "big_demon_idle_anim");
    characters["tiny_zombie_demon"] = new Character(tiles, "tiny_zombie_idle_anim", "tiny_zombie_run_anim", "tiny_zombie_idle_anim");
    characters["goblin"] = new Character(tiles, "goblin_idle_anim", "goblin_run_anim", "goblin_idle_anim");
    characters["imp"] = new Character(tiles, "imp_idle_anim", "imp_run_anim", "imp_idle_anim");
    characters["skelet"] = new Character(tiles, "skelet_idle_anim", "skelet_run_anim", "skelet_idle_anim");
    characters["muddy"] = new Character(tiles, "muddy_run_anim", "muddy_run_anim", "muddy_run_anim");
    characters["swampy"] = new Character(tiles, "swampy_run_anim", "swampy_run_anim", "swampy_run_anim");
    characters["zombie"] = new Character(tiles, "zombie_run_anim", "zombie_run_anim", "zombie_run_anim");
    characters["ice_zombie"] = new Character(tiles, "ice_zombie_run_anim", "ice_zombie_run_anim", "ice_zombie_run_anim");
    characters["masked_orc"] = new Character(tiles, "masked_orc_idle_anim", "masked_orc_run_anim", "masked_orc_idle_anim");
    characters["orc_warrior"] = new Character(tiles, "orc_warrior_idle_anim", "orc_warrior_run_anim", "orc_warrior_idle_anim");
    characters["orc_shaman"] = new Character(tiles, "orc_shaman_idle_anim", "orc_shaman_run_anim", "orc_shaman_idle_anim");
    characters["necromancer"] = new Character(tiles, "necromancer_run_anim", "necromancer_run_anim", "necromancer_run_anim");
    characters["wogol"] = new Character(tiles, "wogol_idle_anim", "wogol_run_anim", "wogol_idle_anim");
    characters["chort"] = new Character(tiles, "chort_idle_anim", "chort_run_anim", "chort_idle_anim");

    // choose 1 hero and 1 monster

    heroit = characters.find("elf_f");
    hero = new Character(*heroit->second);

    monster = new Character(*characters.at("big_demon"));
    monster->pos = {200.0f, 200.0f};
    monster->basespeed = 150.f;

    // behaviour

    monster->velocity = {monster->basespeed, monster->basespeed};
    btimet = 0.f;
    btime = 0.f;
}

CharacterScene::~CharacterScene()
{
    std::cout << "~CharacterScene()\n";
    if (hero)
        delete hero;
    if (monster)
        delete monster;
    for (auto const &[key, val] : characters)
        delete val;
}

void
CharacterScene::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // change hero if A is pressed
    if (pge.GetKey(olc::A).bPressed)
    {
        ++heroit;
        if (heroit == characters.end())
            heroit = characters.begin();
        hero->change(*heroit->second);
        message = "hero changed to " + heroit->first;
    }

    // MONSTER BEHAVIOUR
    btime += fElapsedTime;

    if (btime > btimet) // change behaviour
    {
        int vx=0, vy=0;
        if (monster->velocity.x == 0.0 && monster->velocity.x == 0.0)
        {
            vx = rand() % 3 - 1;
            vy = rand() % 3 - 1;
        }
        monster->velocity = {monster->basespeed * vx, monster->basespeed * vy};

        btimet = 1 + rand() % 5;
        btime = 0.f;
        message = std::to_string(vx) + "," + std::to_string(vy) + " for " + std::to_string(btimet);
    }

    // MANGEMENT OF USER ACTIONS
    hero->userKeys(pge, fElapsedTime);
    monster->bounce(pge, fElapsedTime);

    // DRAW
    pge.Clear(olc::BLACK);
    hero->update(pge, fElapsedTime);
    monster->update(pge, fElapsedTime);

    // TEXT

    // info on top of screen
    TextWindow twin(pge);
    twin.print("<ESC> to quit", HJustify::CENTRE);
    twin.print("use Z,Q,S,D to move the hero", HJustify::CENTRE);

    // debug msg
    TextWindow win2 = twin.subwin(2, 500, HJustify::RIGHT, VJustify::BOTTOM);
    win2.clear(olc::VERY_DARK_YELLOW);
    win2.print(message, HJustify::CENTRE);
}
