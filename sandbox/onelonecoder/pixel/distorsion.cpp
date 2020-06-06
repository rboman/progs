// build & run
// cmake --build . --config Release && Release\play_with_pge.exe

#include "config.h"
#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"


class Example : public olc::PixelGameEngine
{
    float radius = 30.0f;
    olc::vf2d centre = {100, 200};



public:
    Example()
    {
        sAppName = "Example";
    }

    std::unique_ptr<olc::Sprite> source;
    //std::unique_ptr<olc::Sprite> target;

    // parameters
    float N=0.5f;   // -1 <= N <= 1
    int ox = 150;
    int oy = 100;
    float alp = 10.f;
    float zoom = 20.f;

public:
    
    // float fn_f(float x, float y) { return sin(x+y); }
    // float fn_dfdx(float x, float y) { return cos(x+y); }
    // float fn_dfdy(float x, float y) { return cos(x+y); }

    float fn_f(float x, float y) { return sin(x*x+y*y); }
    float fn_dfdx(float x, float y) { return 2*x*cos(x*x+y*y); }
    float fn_dfdy(float x, float y) { return 2*y*cos(x*x+y*y); }

    bool OnUserCreate() override
    {
        std::string srcDir = CMAKE_SOURCE_DIR;


        // create an image
        source = std::make_unique<olc::Sprite>(ScreenWidth(), ScreenHeight());
        SetDrawTarget(source.get());
        Clear(olc::BLACK);

        int scl = 4;

        for(int j=0; j<ScreenHeight()/(9*scl); ++j)
            DrawString(j*9*scl, j*9*scl, "Distorsion", olc::YELLOW, scl);


        SetDrawTarget(nullptr);

        // deform image
        /*
        target = std::make_unique<olc::Sprite>(320, 200);
        SetDrawTarget(target.get());

        int x1 = -100;
        int x2 = 100;
        int y1 = -100;
        int y2 = 100;
        
        float pi = 4.f*atan(1.f);

        for(int i = x1; i<x2; ++i)
            for(int j = y1; j<y2; ++j)
            {
                float a = float(i)/zoom; 
                float b = float(j)/zoom; 
                float nx = fn_dfdx(a,b);
                float ny = fn_dfdy(a,b);
                float z = fn_f(a,b)*zoom;
                float i1 = acos(1.f/sqrt(nx*nx+1.f));
                float i2 = acos(1.f/sqrt(ny*ny+1.f));
                float r1 = pi/2.f-acos(N*sin(i1));
                float r2 = pi/2.f-acos(N*sin(i2));
                float xd = (alp+z)*tan(i1-r1);
                float yd = (alp+z)*tan(i2-r2);
                olc::Pixel C = source->GetPixel(i+ox-xd,j+oy-yd);
                Draw(i+ox, j+oy, C);
            }

        SetDrawTarget(nullptr);
        */
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::DARK_BLUE);

        // Handle User Input

        if (GetKey(olc::Key::SPACE).bHeld)
            return false; // quit


        int width = ScreenWidth();
        int height = ScreenHeight();

        // sprite
        //SetPixelMode(olc::Pixel::ALPHA);
        //DrawSprite(0, 0, source.get());
        //SetPixelMode(olc::Pixel::NORMAL);
        //DrawSprite(0, 210, target.get());


        // chg zoom
        if (GetKey(olc::Key::LEFT).bHeld)
            zoom += 10. * fElapsedTime;
        if (GetKey(olc::Key::RIGHT).bHeld)
            zoom -= 10. * fElapsedTime;
        if (zoom < 1.f)
            zoom = 1.f;
        if (zoom > 100.0f)
            zoom = 100.0f;        

        if (GetKey(olc::Key::UP).bHeld)
            alp += 10. * fElapsedTime;
        if (GetKey(olc::Key::DOWN).bHeld)
            alp -= 10. * fElapsedTime;
        if (alp < 1.f)
            alp = 1.f;
        if (alp > 100.0f)
            alp = 100.0f;

        if (GetKey(olc::Key::A).bHeld)
            N += 1. * fElapsedTime;
        if (GetKey(olc::Key::Z).bHeld)
            N -= 1. * fElapsedTime;
        if (N < -1.f)
            N = -1.f;
        if (N > 1.0f)
            N = 1.0f;

        // move the centre of the circle
        if (GetMouse(0).bHeld)
        {
            centre = {float(GetMouseX()), float(GetMouseY())};
        }        
        ox = centre.x;
        oy = centre.y;

        int x1 = -ox;
        int x2 = width-ox;
        int y1 = -oy;
        int y2 = height-oy;
        
        float pi = 4.f*atan(1.f);

        for(int i = x1; i<x2; ++i)
            for(int j = y1; j<y2; ++j)
            {
                float a = float(i)/zoom; 
                float b = float(j)/zoom; 
                float nx = fn_dfdx(a,b);
                float ny = fn_dfdy(a,b);
                float z = fn_f(a,b)*zoom;
                float i1 = acos(1.f/sqrt(nx*nx+1.f));
                float i2 = acos(1.f/sqrt(ny*ny+1.f));
                float r1 = pi/2.f-acos(N*sin(i1));
                float r2 = pi/2.f-acos(N*sin(i2));
                float xd = (alp+z)*tan(i1-r1);
                float yd = (alp+z)*tan(i2-r2);
                olc::Pixel C = source->GetPixel(i+ox - xd,j+oy - yd);
                Draw(i+ox, j+oy, C);
            }


        // draw axes
        DrawLine(0, oy, width, oy, olc::WHITE);
        DrawLine(ox, 0, ox, height, olc::WHITE);
        DrawLine(ox+zoom, oy-3, ox+zoom, oy+3, olc::WHITE);
        DrawLine(ox-3, oy+zoom, ox+3, oy+zoom, olc::WHITE);

        DrawString(5,5+40*9, "zoom="+std::to_string(zoom)+" [left-right]", olc::WHITE);
        DrawString(5,5+41*9, "alp="+std::to_string(alp)+" [up-down]", olc::WHITE);
        DrawString(5,5+42*9, "N="+std::to_string(N)+" [A-Z]", olc::WHITE);

        return true;
    }
};

int main()
{
    Example demo;
    if (demo.Construct(500, 500, 2, 2))
        demo.Start();

    return 0;
}
