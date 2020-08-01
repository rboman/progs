
#include "Character.h"
#include "Tiles.h"
#include "TextWindow.h"

Character::Character(Tiles *_tiles, std::string const &idlename, std::string const &runname,
                     std::string const &hitname)
    : tiles(_tiles)
{
    pos = {100.0f, 100.0f};
    scale = {3.0f, 3.0f};
    basespeed = 200.0f;
    set_tiles(idlename, runname, hitname);
    set_state(State::IDLE);
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
Character::set_state(State newstate)
{
    state = newstate;
    // choose current tile according to state
    switch (newstate)
    {
    case State::RUNNING:
        tl = tile_run;
        break;
    case State::HIT:
        tl = tile_hit;
        break;
    case State::IDLE:
    default:
        tl = tile_idle;
        break;
    }
    // reset anim counter
    atime = 0.0f;
}

void
Character::set_tiles(std::string const &idlename, std::string const &runname,
                     std::string const &hitname)
{
    try
    {    
        tile_idle = &(tiles->tilemap.at(idlename));
        tile_run = &(tiles->tilemap.at(runname));
        tile_hit = &(tiles->tilemap.at(hitname));
    }
    catch(const std::exception& e)
    {
        std::cerr << "EXCEPTION: " << e.what() << std::endl;
    }
}

void
Character::change(Character const &chr)
{
    tile_idle = chr.tile_idle;
    tile_run = chr.tile_run;
    tile_hit = chr.tile_hit;
    set_state(state);
}

void
Character::bounce(olc::PixelGameEngine &pge, float fElapsedTime)
{
    olc::vf2d oldpos = pos;

    // predictor
    pos += velocity * fElapsedTime;

    // position of the lowerright corner
    olc::vf2d pos2 = lowerright();

    // test collisions with borders and swap velocity is required
    if (pos.x < 0.0f)
        velocity.x = -velocity.x;
    if (pos2.x > pge.ScreenWidth())
        velocity.x = -velocity.x;
    if (pos.y < 0.0f)
        velocity.y = -velocity.y;
    if (pos2.y > pge.ScreenHeight())
        velocity.y = -velocity.y;

    // reset current pos
    pos = oldpos;
}


void
Character::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // HANDLE VELOCITY

    // move the reference point
    pos += velocity * fElapsedTime;

    // position of the lowerright corner
    olc::vf2d pos2 = lowerright();

    // collisions with borders.
    if (pos.x < 0.0f)
        pos.x = 0.0f;
    if (pos2.x > pge.ScreenWidth())
        pos.x = std::round(pos.x - pos2.x + (float)pge.ScreenWidth());
    if (pos.y < 0.0f)
        pos.y = 0.0f;
    if (pos2.y > pge.ScreenHeight())
        pos.y = std::round(pos.y - pos2.y + (float)pge.ScreenHeight());

    // new state
    State newstate = State::IDLE;

    if (velocity.x != 0.0 || velocity.x != 0.0)
        newstate = State::RUNNING;

    if (state != newstate)
        set_state(newstate);

    // DRAWING PART

    // flip image according to velocity
    if (velocity.x < 0.0f)
        scale.x = -abs(scale.x);
    if (velocity.x > 0.0f)
        scale.x = abs(scale.x);

    // if decal is x-inverted, the drawing ref point is the right corner
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
    pge.Draw(lowerright(), olc::RED);
    pge.DrawString(olc::vf2d{pos.x - 50.f, pos2.y + 1.0f}, "atime=" + std::to_string(atime));
    pge.DrawString(olc::vf2d{pos.x - 50.f, pos2.y + 10.0f}, "pos2=" + std::to_string(pos2.x));
}
