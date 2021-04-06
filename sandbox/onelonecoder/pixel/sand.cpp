// Sand simulation (a la Noita)
// 
// references:
//  Recreating Noita's Sand Simulation in C and OpenGL
//      https://www.youtube.com/watch?v=VLZjd_Y1gJ8&ab_channel=JohnJackson
//  Exploring the Tech and Design of Noita 
//      https://www.youtube.com/watch?v=prXuyMCgbTc&ab_channel=GDC
//
//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\sand.exe

#include "olcPixelGameEngine.h"

enum class Type
{
    EMPTY,
    SAND
};

class Pixel
{
public:
    Type type;
    Pixel(Type t=Type::EMPTY) : type(t) {}
};


class SandSim : public olc::PixelGameEngine
{
    std::vector<Pixel> world;

    float timesum = 0.0;            ///< internal variable which stores the accumulted time between 2 updates
    int fps = 60;                    ///< update frequency


public:
    SandSim() 
    { 
        sAppName = "Sand simulation";
    }

public:
    /// convert indices i,j to a 1D array index
    int idx(int i, int j) const
    {
        return i*this->ScreenWidth()+j;
    }

    bool OnUserCreate() override
    {
        // allocate world array
        world.resize(this->ScreenWidth()*this->ScreenHeight());

        return true;
    }

    void draw_world()
    {
        for (int i = 0; i <this->ScreenHeight(); i++)
            for (int j = 0; j < this->ScreenWidth(); j++)
            {
                Pixel &pix = world[idx(i,j)];
                if (pix.type==Type::SAND)
                    Draw(j, i, olc::Pixel(255, 255, 0));  
            }
    }

    bool try_move(Pixel &current, int i, int j)
    {
        auto &target = world[idx(i,j)];
        if (target.type==Type::EMPTY)
        {
            target = current;
            current.type = Type::EMPTY;
            return true;
        }
        return false;
    }

    void update_world()
    {
        for (int i = this->ScreenHeight()-2; i >=0; i--)
            for (int j = 0; j < this->ScreenWidth(); j++)
            {
                auto &current = world[idx(i,j)];
                if(try_move(current, i+1,j)) 
                    continue;

                if (j-1>=0)
                {
                    if(try_move(current, i+1,j-1)) continue;
                }
                if (j+1<this->ScreenWidth())
                {
                    if(try_move(current, i+1,j+1)) continue;         
                }                    
                
            }
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::BLACK);

        // get mouse pos
        olc::vi2d mpos = {GetMouseX(), GetMouseY()};        

        if (GetMouse(0).bHeld) // left click - add sand
        {
            int x = mpos.x + rand() % 10 - 5;
            int y = mpos.y + rand() % 10 - 5;
            if (x>0 && x<this->ScreenWidth() && 
                y>0 && y<this->ScreenHeight() )
                world[idx(y,x)] = Pixel(Type::SAND);
        }

        if (GetMouse(1).bHeld) // right click - remove material
        {
            int x = mpos.x + rand() % 10 - 5;
            int y = mpos.y + rand() % 10 - 5;
            if (x>0 && x<this->ScreenWidth() && 
                y>0 && y<this->ScreenHeight() )
                world[idx(y,x)] = Pixel(Type::EMPTY);
        }

        // accumulate time
        timesum = timesum + fElapsedTime;
        float frametime = 1.0/fps;

        if (timesum>frametime)
        {
            while(timesum>frametime)
                timesum -= frametime;
            update_world();
        }

        draw_world();

        return true;
    }
};

int
main()
{
    SandSim demo;
    if (demo.Construct(300, 200, 4, 4))
        demo.Start();

    return 0;
}
