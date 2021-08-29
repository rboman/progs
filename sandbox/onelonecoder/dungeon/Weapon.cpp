
#include "Weapon.h"
#include "Tiles.h"
#include "TextWindow.h"

Weapon::Weapon(Tiles *_tiles, std::string const &name)
    : tiles(_tiles)
{
    pos = {200.0f, 100.0f};
    scale = {3.0f, 3.0f};
    angle = 0.0f;
    set_tile(name);
}

void
Weapon::set_tile(std::string const &name)
{
    try
    {
        tile = &(tiles->tilemap.at(name));
    }
    catch (const std::exception &e)
    {
        std::cerr << "EXCEPTION: " << e.what() << std::endl;
    }
}

void
Weapon::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // HANDLE ROTATION

    // DRAWING PART

    olc::vd2d centre = {(float)tile->w/2.0f, (float)tile->h/2.0f};
    angle += 2.0f*fElapsedTime;
    if(angle>=2.0f*M_PI)  angle -= 2.0f*M_PI;
    if(angle<=-2.0f*M_PI)  angle += 2.0f*M_PI;

    // draw decal
    // pge.DrawPartialDecal(pos, tiles->decal.get(),
    //                      {(float)tile->ox, (float)tile->oy},
    //                      {(float)tile->w, (float)tile->h}, 
    //                      scale, olc::WHITE);

	pge.DrawPartialRotatedDecal(pos, tiles->decal.get(),
                        angle, centre,
                         {(float)tile->ox, (float)tile->oy},
                         {(float)tile->w, (float)tile->h}, 
                         scale, olc::WHITE);

}
