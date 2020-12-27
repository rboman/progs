/***********************************************************************
 *                      Starfield 1  (C++ & PGE)                        *
 *                                                                      *
 * . Déplacement latéral des étoiles                                    *
 *                                                             12.08.96 *
 ************************************************************************/

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

struct etoiles
{
    float x;
    int y;
    int plan;
};
etoiles et[500];

class Example : public olc::PixelGameEngine
{
public:
    bool OnUserCreate() override
    {
        for (int nb_et = 0; nb_et < 500; nb_et++)
        {
            et[nb_et].x = rand() % 320;
            et[nb_et].y = rand() % 200;
            et[nb_et].plan = rand() % 256;
        }
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        if (GetKey(olc::Key::SPACE).bHeld)
            return false; // quit

        for (int nb_et = 0; nb_et < 500; nb_et++)
        {
            // Eteint l'étoile
            Draw(et[nb_et].x, et[nb_et].y, olc::BLACK);

            // Calcule la vitesse
            et[nb_et].x -= ((et[nb_et].plan >> 5) + 1) * fElapsedTime * 30;

            // Teste la sortie du champ de vision
            if (et[nb_et].x <= 0)
            {
                et[nb_et].x = 319;
                et[nb_et].y = rand() % 200;
                et[nb_et].plan = rand() % 256;
            }

            // Dessine l'étoile
            Draw(et[nb_et].x, et[nb_et].y, olc::WHITE);
            uint8_t g = et[nb_et].plan;
            Draw(et[nb_et].x, et[nb_et].y, olc::Pixel(g, g, g));
        }

        return true;
    }
};

int
main()
{
    Example demo;
    if (demo.Construct(320, 200, 4, 4))
        demo.Start();
    return 0;
}
