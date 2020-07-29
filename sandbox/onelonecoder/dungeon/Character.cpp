
#include "Character.h"
#include "Tiles.h"


enum class Justify
{
    LEFT,
    RIGHT,
    CENTRE
};

class TextWindow
{
    olc::PixelGameEngine &pge;
    int32_t charHeight = 9;
    int32_t charWidth = 9;
public:    
    int32_t nbrows;
    int32_t nbcols;
    int32_t row;   // current row

    olc::Pixel colour = olc::WHITE;

    TextWindow(olc::PixelGameEngine &_pge) : pge(_pge)
    {
        nbrows = pge.ScreenHeight() / charHeight;
        nbcols = pge.ScreenWidth() / charWidth;
        row = 0;
    }

    void print(std::string const &text, Justify justify = Justify::LEFT)
    {
        int len = text.length();

        int col = 0;

        if (justify==Justify::RIGHT)
            col = nbcols-len+1;
        if (justify==Justify::CENTRE)
            col = (nbcols-len)/2;

        // draw string as sprite (below decals)
        // pge.DrawString(col * charWidth + 1,
        //                 row * charHeight + 1, 
        //                 text, colour);
        // draw string as decal   
        pge.DrawStringDecal({(float) (col * charWidth + 1),
                        (float) (row * charHeight + 1) }, 
                        text, colour);
        row++;
    }

    void clear(olc::Pixel c = olc::RED)
    {
        pge.FillRect( {0,0}, {nbrows*charWidth, nbcols*charHeight}, c);
    }
};


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
    twin.clear(olc::Pixel(100,0,0));
    twin.print("hello");
    twin.print("how are you?", Justify::CENTRE);

    twin.print("frame = " + std::to_string(frame), Justify::RIGHT);
    twin.print("nbrows = " + std::to_string(twin.nbrows), Justify::CENTRE);
    twin.print("nbcols = " + std::to_string(twin.nbcols), Justify::CENTRE);

    // for(int i=0; i<100; ++i)
    //     twin.print(std::to_string(i));

    twin.print("hello\nmyfriend!\nABCDEFG\n12345");

    // note
    //pge.DrawRect(0, 0, 2, 2, olc::GREEN); // fait 3 de large si on inclut la bordure

    twin.print("<ESC> to quit", Justify::CENTRE);

}
