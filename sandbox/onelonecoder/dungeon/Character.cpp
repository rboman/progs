
#include "Character.h"
#include "Tiles.h"
#include "TextWindow.h"

Character::Character(std::string const &_idlename, std::string const &_runname,
                     std::string const &_hitname)
    : idlename(_idlename), runname(_runname), hitname(_hitname)
{
    pos = {100.0f, 100.0f};
    scale = {3.0f, 3.0f};
    atime = 0.0f;
    action = Action::IDLE;
}

void
Character::update(olc::PixelGameEngine &pge, Tiles *tiles, float fElapsedTime)
{

    float speed = 200.0f;

    bool move = false;

    // moving left
    if (pge.GetKey(olc::Key::LEFT).bHeld || pge.GetKey(olc::Key::Q).bHeld)
    {
        move = true;
        scale.x = -abs(scale.x);
        pos += {-speed * fElapsedTime, 0.0f};
        if (pos.x < 0.0f)
            pos.x = 0.0f;
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving right
    if (pge.GetKey(olc::Key::RIGHT).bHeld || pge.GetKey(olc::Key::D).bHeld)
    {
        move = true;
        scale.x = abs(scale.x);
        pos += {speed * fElapsedTime, 0.0f};
        if (pos.x > pge.ScreenWidth())
            pos.x = (float)pge.ScreenWidth();
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving up
    if (pge.GetKey(olc::Key::UP).bHeld || pge.GetKey(olc::Key::Z).bHeld)
    {
        move = true;
        pos += {0.0f, -speed * fElapsedTime};
        if (pos.y < 0.0f)
            pos.y = 0.0f;
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // moving down
    if (pge.GetKey(olc::Key::DOWN).bHeld || pge.GetKey(olc::Key::S).bHeld)
    {
        move = true;
        pos += {0.0f, speed * fElapsedTime};
        if (pos.y > pge.ScreenHeight())
            pos.y = (float)pge.ScreenHeight();
        if (action != Action::RUNNING)
        {
            action = Action::RUNNING;
            atime = 0.0f;
        }
    }

    // hit
    if (pge.GetKey(olc::Key::SPACE).bHeld)
    {
        move = true;
        if (action != Action::HIT)
        {
            action = Action::HIT;
            atime = 0.0f;
        }
    }

    // idle mode
    if (!move && action != Action::IDLE)
    {
        action = Action::IDLE;
        atime = 0.0f;
    }

    // choose tile according to action
    Tile *tl = nullptr;
    switch (action)
    {
    case Action::RUNNING:
        tl = &(tiles->tilemap[runname]);
        break;
    case Action::HIT:
        tl = &(tiles->tilemap[hitname]);
        break;
    case Action::IDLE:
    default:
        tl = &(tiles->tilemap[idlename]);
        break;
    }

    // if decal is x-inverted, the ref point is the right corner
    olc::vf2d dpos = pos;
    if (scale.x < 0.0f)
        dpos.x -= tl->w * scale.x;

    // choose which frame to display
    atime = atime + fElapsedTime;
    int ni = tl->ni;
    int frame = int(atime * 10) % ni;

    // draw decal
    pge.DrawPartialDecal(dpos, tiles->decal.get(),
                         {(float)(tl->ox + frame * tl->w), (float)tl->oy},
                         {(float)tl->w, (float)tl->h}, scale, olc::WHITE);

    // debug
    TextWindow twin(pge);
    //twin.clear();
    twin.print("<ESC> to quit", HJustify::CENTRE);
    //twin.print("1234567890123456789012345678901234567890123456789012345678901234567890");
    //twin.frame(olc::GREEN);

    // twin.print("how are you?", HJustify::CENTRE);

    // twin.print("nbrows = " + std::to_string(twin.nbrows), HJustify::CENTRE);
    // twin.print("nbcols = " + std::to_string(twin.nbcols), HJustify::CENTRE);

    // for(int i=0; i<100; ++i)
    //     twin.print(std::to_string(i));

    // twin.print("hello\nmyfriend!\nABCDEFG\n12345");

    // note
    // pge.DrawRect(0, 0, 2, 2, olc::GREEN); // fait 3 de large si on inclut la
    // bordure



    TextWindow win2 = twin.subwin(2, 500, HJustify::RIGHT, VJustify::BOTTOM);
    win2.clear(olc::VERY_DARK_YELLOW);
    win2.print("frame = " + std::to_string(frame), HJustify::CENTRE);

    // win2.colour = olc::YELLOW;
    // win2.print("COUCOU!", HJustify::CENTRE);
    // win2.print("ox="+std::to_string(win2.ox));
    // win2.print("oy="+std::to_string(win2.oy));
    // win2.print("nbrows="+std::to_string(win2.nbrows));
    // win2.print("nbcols="+std::to_string(win2.nbcols));
    // win2.print("nbrows2="+std::to_string(twin.nbrows));
    // win2.print("nbcols2="+std::to_string(twin.nbcols));
    // win2.print("tox="+std::to_string(twin.ox));
    // win2.print("toy="+std::to_string(twin.oy));
    // win2.print("test");

}
