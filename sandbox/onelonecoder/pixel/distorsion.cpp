//
// ██████  ██ ███████ ████████  ██████  ██████  ███████ ██  ██████  ███    ██ 
// ██   ██ ██ ██         ██    ██    ██ ██   ██ ██      ██ ██    ██ ████   ██ 
// ██   ██ ██ ███████    ██    ██    ██ ██████  ███████ ██ ██    ██ ██ ██  ██ 
// ██   ██ ██      ██    ██    ██    ██ ██   ██      ██ ██ ██    ██ ██  ██ ██ 
// ██████  ██ ███████    ██     ██████  ██   ██ ███████ ██  ██████  ██   ████ 
// 
//
// build & run
// cmake --build . --config Release && Release\distorsion.exe

#include "config.h"
#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

class Distorsion : public olc::PixelGameEngine
{
    olc::vi2d centre = {100, 200};
    float N = 0.5f;     // distorsion factor (-1 <= N <= 1)
    float alp = 10.f;   // depth of the image
    float zoom = 100.f; // zoom factor

    float (*fn_f)(float, float);
    float (*fn_dfdx)(float, float);
    float (*fn_dfdy)(float, float);

    std::unique_ptr<olc::Sprite> source;
    // std::unique_ptr<olc::Sprite> target;

    bool usethreads = true;
    bool displayHelp = true;
    bool displayPars = true;
    bool displayAxes = true;

public:
    Distorsion() { sAppName = "Distorsion"; }

private:
    void fct_sin()
    {
        static auto sfn_f = [](float x, float y) { return sin(x + y); };
        static auto sfn_dfdx = [](float x, float y) { return cos(x + y); };
        static auto sfn_dfdy = [](float x, float y) { return cos(x + y); };
        fn_f = sfn_f;
        fn_dfdx = sfn_dfdx;
        fn_dfdy = sfn_dfdy;
    }

    void fct_sin2()
    {
        static auto sfn_f = [](float x, float y) { return sin(x * x + y * y); };
        static auto sfn_dfdx = [](float x, float y) {
            return 2 * x * cos(x * x + y * y);
        };
        static auto sfn_dfdy = [](float x, float y) {
            return 2 * y * cos(x * x + y * y);
        };
        fn_f = sfn_f;
        fn_dfdx = sfn_dfdx;
        fn_dfdy = sfn_dfdy;
    }

    void fct_gauss()
    {
        static auto sfn_f = [](float x, float y) {
            return exp(-x * x - y * y);
        };
        static auto sfn_dfdx = [](float x, float y) {
            return -2 * x * exp(-x * x - y * y);
        };
        static auto sfn_dfdy = [](float x, float y) {
            return -2 * y * exp(-x * x - y * y);
        };
        fn_f = sfn_f;
        fn_dfdx = sfn_dfdx;
        fn_dfdy = sfn_dfdy;
    }

    void src_text1()
    {
        SetDrawTarget(source.get());
        Clear(olc::BLACK);
        int scl = 4; // font scale
        for (int j = 0; j < ScreenHeight() / (9 * scl); ++j)
            DrawString(j * 9 * scl, j * 9 * scl, "Distorsion", olc::YELLOW,
                       scl);
        SetDrawTarget(nullptr);
    }

    void src_grid()
    {
        SetDrawTarget(source.get());
        Clear(olc::BLACK);
        int spacing = 20;
        int offset = spacing / 2;
        for (int i = 0; i < ScreenHeight() / spacing; ++i)
            DrawLine(0, i * spacing + offset, ScreenWidth(),
                     i * spacing + offset, olc::YELLOW);
        for (int i = 0; i < ScreenWidth() / spacing; ++i)
            DrawLine(i * spacing + offset, 0, i * spacing + offset,
                     ScreenHeight(), olc::YELLOW);
        SetDrawTarget(nullptr);
    }

    void src_img()
    {
        SetDrawTarget(source.get());
        Clear(olc::BLACK);
        olc::Sprite img(std::string(CMAKE_SOURCE_DIR) + "/bouteille.png");
        DrawSprite(0, 0, &img);
        SetDrawTarget(nullptr);
    }

    bool OnUserCreate() override
    {
        fct_sin();
        source = std::make_unique<olc::Sprite>(ScreenWidth(), ScreenHeight());
        src_img();
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::BLACK);

        // Handle User Input ---------------------------------------------------

        if (GetKey(olc::Key::SPACE).bPressed)
            return false; // quit

        // chg source image
        if (GetKey(olc::Key::F1).bPressed)
            src_grid();
        if (GetKey(olc::Key::F2).bPressed)
            src_text1();
        if (GetKey(olc::Key::F3).bPressed)
            src_img();

        // chg fct
        if (GetKey(olc::Key::K1).bPressed)
            fct_sin();
        if (GetKey(olc::Key::K2).bPressed)
            fct_sin2();
        if (GetKey(olc::Key::K3).bPressed)
            fct_gauss();

        // Help
        if (GetKey(olc::Key::H).bPressed)
            displayHelp = !displayHelp;
        // Parameters
        if (GetKey(olc::Key::P).bPressed)
            displayPars = !displayPars;
        // Parameters
        if (GetKey(olc::Key::X).bPressed)
            displayAxes = !displayAxes;
        // Parameters
        if (GetKey(olc::Key::T).bPressed)
            usethreads = !usethreads;

        // chg zoom
        if (GetKey(olc::Key::LEFT).bHeld || GetMouseWheel() > 0)
            zoom += 100.f * fElapsedTime;
        if (GetKey(olc::Key::RIGHT).bHeld || GetMouseWheel() < 0)
            zoom -= 100.f * fElapsedTime;
        if (zoom < 1.f)
            zoom = 1.f;
        if (zoom > 200.0f)
            zoom = 200.0f;

        // chg depth of the image
        if (GetKey(olc::Key::UP).bHeld)
            alp += 50.f * fElapsedTime;
        if (GetKey(olc::Key::DOWN).bHeld)
            alp -= 50.f * fElapsedTime;
        if (alp < 1.f)
            alp = 1.f;
        if (alp > 100.0f)
            alp = 100.0f;

        // chg distorsion index
        if (GetKey(olc::Key::A).bHeld)
            N += 1.f * fElapsedTime;
        if (GetKey(olc::Key::Z).bHeld)
            N -= 1.f * fElapsedTime;
        if (N < -1.f)
            N = -1.f;
        if (N > 1.0f)
            N = 1.0f;

        // move the centre of the circle
        if (GetMouse(0).bHeld)
            centre = {GetMouseX(), GetMouseY()};

        int32_t ox = centre.x;
        int32_t oy = centre.y;

        int32_t x1 = 0;
        int32_t x2 = ScreenWidth();
        int32_t y1 = 0;
        int32_t y2 = ScreenHeight();


        if (usethreads)
        {
            // with several threads
            constexpr int nMaxThreads = 6;
            int nSectionWidth = (x2 - x1) / nMaxThreads;

            std::thread t[nMaxThreads];

            for (int i = 0; i < nMaxThreads; i++)
            {
                int xmin = x1 + i * nSectionWidth;
                int xmax = xmin + nSectionWidth;
                if (i == nMaxThreads - 1)
                    xmax = x2;
                t[i] = std::thread(&Distorsion::distort, this, ox, oy, xmin, y1,
                                   xmax, y2);
            }

            for (int i = 0; i < nMaxThreads; i++)
                t[i].join();
        }
        else
        {        
            // 1 process
            distort(ox, oy, x1, y1, x2, y2);
        }

        // draw axes
        if (displayAxes)
        {
            DrawLine(0, oy, ScreenWidth(), oy, olc::WHITE);
            DrawLine(ox, 0, ox, ScreenHeight(), olc::WHITE);
            DrawLine(ox + int32_t(zoom), oy - 3, ox + int32_t(zoom), oy + 3,
                     olc::WHITE);
            DrawLine(ox - 3, oy + int32_t(zoom), ox + 3, oy + int32_t(zoom),
                     olc::WHITE);
        }

        // print parameters
        if (displayPars)
            drawParams();

        if (displayHelp)
            drawHelp();

        return true;
    }

    void distort(int32_t ox, int32_t oy, int32_t x1, int32_t y1, int32_t x2,
                 int32_t y2)
    {
        for (int32_t i = x1; i < x2; ++i)
            for (int32_t j = y1; j < y2; ++j)
            {
                float a = float(i - ox) / zoom;
                float b = float(j - oy) / zoom;
                float nx = fn_dfdx(a, b);
                float ny = fn_dfdy(a, b);
                float z = fn_f(a, b) * zoom;
                float i1 = atan(nx);
                float i2 = atan(ny);

                float r1, r2;
                if (N >= 0.0f)
                {
                    r1 = asin((1.f - N) * sin(i1));
                    r2 = asin((1.f - N) * sin(i2));
                }
                else
                {
                    r1 = asin(sin(i1) / (N + 1.0f));
                    r2 = asin(sin(i2) / (N + 1.0f));
                }

                float xd = (alp + z) * tan(i1 - r1);
                float yd = (alp + z) * tan(i2 - r2);
                olc::Pixel C =
                    source->GetPixel(i - int32_t(xd), j - int32_t(yd));
                Draw(i, j, C);
            }
    }

    void drawParams()
    {
        int32_t charHeight = 9;
        int32_t nbrows = ScreenHeight() / charHeight;
        int32_t nbcols = ScreenWidth() / charHeight;

        SetPixelMode(olc::Pixel::ALPHA);
        FillRect(0, (nbrows - 4) * charHeight, ScreenWidth(),
                 (4 * nbrows) * charHeight, olc::Pixel(0, 0, 0, 128));
        SetPixelMode(olc::Pixel::NORMAL);

        DrawString(charHeight, (nbrows - 3) * charHeight,
                   "zoom = " + std::to_string(zoom) + " [left-right]",
                   olc::WHITE);
        DrawString(charHeight, (nbrows - 2) * charHeight,
                   "alp  = " + std::to_string(alp) + " [up-down]", olc::WHITE);
        DrawString(charHeight, (nbrows - 1) * charHeight,
                   "N    = " + std::to_string(N) + " [A-Z]", olc::WHITE);

        std::string txt =
            std::string("threading:") +
            ((usethreads) ? std::string("on") : std::string("off"));
        DrawString((nbcols - int32_t(txt.length()) - 1) * charHeight,
                   (nbrows - 1) * charHeight, txt, olc::WHITE);
    }

    void drawHelp()
    {
        std::string text = "Distorison\n"
                           "----------\n\n"
                           " Keys:\n\n"
                           " - F1, F2, F3     : background image\n"
                           " - 1, 2, 3        : distorsion function\n"
                           " - A-Z            : distorsion index\n"
                           " - <left>-<right> : zoom\n"
                           " - <up>-<down>    : depth\n"
                           " - A-Z            : distorsion index\n"
                           " - H              : display help\n"
                           " - X              : display axes\n"
                           " - P              : display parameters\n"
                           " - <space>        : quit";

        int charHeight = 9;
        int nbrows = ScreenHeight() / charHeight;

        int bord = 6;
        SetPixelMode(olc::Pixel::ALPHA);
        FillRect(bord * charHeight, bord * charHeight,
                 ScreenWidth() - bord * 2 * charHeight,
                 ScreenHeight() - bord * 2 * charHeight,
                 olc::Pixel(0, 0, 0, 128));
        SetPixelMode(olc::Pixel::NORMAL);

        DrawString((bord + 1) * charHeight, (bord + 1) * charHeight, text,
                   olc::WHITE);
    }
};

int
main()
{
    Distorsion demo;
    if (demo.Construct(500, 500, 2, 2))
        demo.Start();

    return 0;
}
