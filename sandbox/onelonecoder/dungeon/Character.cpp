
#include "Character.h"
#include "Tiles.h"
#include "TextWindow.h"

Character::Character(Tiles *_tiles, std::string const &_idlename, std::string const &_runname,
                     std::string const &_hitname)
    : tiles(_tiles), idlename(_idlename), runname(_runname), hitname(_hitname)
{
    pos = {100.0f, 100.0f};
    scale = {3.0f, 3.0f};
    atime = 0.0f;
    action = State::IDLE;
    speed = 200.0f;
}

/// modify the state of the character according to user keys

void
Character::userKeys(olc::PixelGameEngine &pge, float fElapsedTime)
{

    bool move = false;

    // moving left
    if (pge.GetKey(olc::Key::LEFT).bHeld || pge.GetKey(olc::Key::Q).bHeld)
    {
        move = true;
        scale.x = -abs(scale.x);
        pos += {-speed * fElapsedTime, 0.0f};
        if (pos.x < 0.0f)
            pos.x = 0.0f;
        if (action != State::RUNNING)
        {
            action = State::RUNNING;
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
        if (action != State::RUNNING)
        {
            action = State::RUNNING;
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
        if (action != State::RUNNING)
        {
            action = State::RUNNING;
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
        if (action != State::RUNNING)
        {
            action = State::RUNNING;
            atime = 0.0f;
        }
    }

    // hit
    if (pge.GetKey(olc::Key::SPACE).bHeld)
    {
        move = true;
        if (action != State::HIT)
        {
            action = State::HIT;
            atime = 0.0f;
        }
    }

    // idle mode
    if (!move && action != State::IDLE)
    {
        action = State::IDLE;
        atime = 0.0f;
    }

}

void
Character::update(olc::PixelGameEngine &pge, float fElapsedTime)
{

    // MANAGEMENT OF USER ACTIONS
    userKeys(pge, fElapsedTime);


    // choose tile according to action
    Tile *tl = nullptr;
    switch (action)
    {
    case State::RUNNING:
        tl = &(tiles->tilemap[runname]);
        break;
    case State::HIT:
        tl = &(tiles->tilemap[hitname]);
        break;
    case State::IDLE:
    default:
        tl = &(tiles->tilemap[idlename]);
        break;
    }


    // DRAWING PART

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
