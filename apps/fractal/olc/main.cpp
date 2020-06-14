#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

class Example : public olc::PixelGameEngine
{
    double x1 = -2.0;
    double y1 = 1.2;
    double x2 = 1.5;
    double y2 = -1.2;
    int nb_coul = 50;

    std::vector<olc::Pixel> colours;

public:
    Example() { sAppName = "Example"; }

public:
    bool OnUserCreate() override
    {
        // Called once at the start, so create things here

        // setup palette
        for (int n = 0; n < nb_coul; ++n)
            colours.push_back(olc::Pixel((255 * n) / nb_coul, 0, 0));
        colours.push_back(olc::Pixel(0, 0, 0));

        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {

        double a1 = x2 - x1;
        double a2 = y2 - y1;
        int sl = ScreenWidth();
        int sh = ScreenHeight();

        for (int xe = 0; xe < sl; ++xe)
            for (int ye = 0; ye < sh; ++ye)
            {
                double xc = (a1 * xe) / sl + x1;
                double yc = (a2 * ye) / sh + y1;

                int n = 0;
                double xn = 0.0;
                double yn = 0.0;

                while (n < nb_coul && yn * yn + xn * xn < 4.0)
                {
                    double xn2 = xn * xn - yn * yn + xc;
                    double yn2 = 2 * xn * yn + yc;
                    xn = xn2;
                    yn = yn2;
                    n++;
                }
                Draw(xe, ye, colours[n]);
            }
        return true;
    }
};

int
main()
{
    Example demo;
    if (demo.Construct(640, 480, 1, 1))
        demo.Start();
    return 0;
}
