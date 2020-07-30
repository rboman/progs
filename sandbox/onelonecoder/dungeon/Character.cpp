
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
    state = State::IDLE;
    basespeed = 200.0f;
    tl = &(tiles->tilemap[idlename]);
}

/// modify the state of the character according to user keys

void
Character::userKeys(olc::PixelGameEngine &pge, float fElapsedTime)
{
    if (pge.GetKey(olc::Key::LEFT).bHeld || pge.GetKey(olc::Key::Q).bHeld)
        velocity.x = -basespeed; // moving left
    else if (pge.GetKey(olc::Key::RIGHT).bHeld || pge.GetKey(olc::Key::D).bHeld)
        velocity.x = basespeed; // moving right
    else
        velocity.x = 0.0f;

    if (pge.GetKey(olc::Key::UP).bHeld || pge.GetKey(olc::Key::Z).bHeld)
        velocity.y = -basespeed; // moving up
    else if (pge.GetKey(olc::Key::DOWN).bHeld || pge.GetKey(olc::Key::S).bHeld)
        velocity.y = basespeed; // moving down
    else
        velocity.y = 0.0f;
}

void
Character::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // MANGEMENT OF USER ACTIONS
    userKeys(pge, fElapsedTime);

    // HANDLE VELOCITY

    // move the reference point
    pos += velocity * fElapsedTime;

    // flip image according to velocity
    if(velocity.x<0.0f) scale.x = -abs(scale.x);
    if(velocity.x>0.0f) scale.x = abs(scale.x);

    // position of the opposite corner
    olc::vf2d pos2 = opposite();

    // collisions with borders.+/

    if (pos.x < 0.0f)
        pos.x = 0.0f;
    if (pos2.x > pge.ScreenWidth())
        pos.x -= pos2.x-(float)pge.ScreenWidth();
    if (pos.y < 0.0f)
        pos.y = 0.0f;
    if (pos2.y > pge.ScreenHeight())
        pos.y -= pos2.y-(float)pge.ScreenHeight();
           
    // new state
    State newstate = State::IDLE;

    if (velocity.x != 0.0 || velocity.x != 0.0 )
        newstate = State::RUNNING;

    if(state != newstate)
    {
        atime = 0.0f;
        state = newstate;
    }

    // DRAWING PART

    // choose tile according to state
    
    switch (state)
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

    // if decal is x-inverted, the ref point is the right corner
    olc::vf2d dpos = pos;
    if (scale.x < 0.0f)
        dpos.x -= tl->w * scale.x;

    // choose which frame to display
    atime = atime + 10 * fElapsedTime;
    int ni = tl->ni;
    int frame = int(atime) % ni;
    if (atime > ni)
        atime -= ni;

    // draw decal
    pge.DrawPartialDecal(dpos, tiles->decal.get(),
                         {(float)(tl->ox + frame * tl->w), (float)tl->oy},
                         {(float)tl->w, (float)tl->h}, scale, olc::WHITE);

    // DEBUG: draw reference positions
    pge.Draw(pos, olc::YELLOW);
    pge.Draw(opposite(), olc::RED);
    pge.DrawString(olc::vf2d{pos.x, pos2.y + 1.0f}, "atime=" + std::to_string(atime));
}
