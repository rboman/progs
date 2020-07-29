
#include "Character.h"
#include "Tiles.h"


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
}
