// Conway's Game of Life
//      https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\gameoflife.exe

#include "olcPixelGameEngine.h"
#include <chrono>
#include <thread>

class GameOfLife : public olc::PixelGameEngine
{
    std::vector<bool> worldA;       ///< state A of the world
    std::vector<bool> worldB;       ///< state B of the world
    std::vector<bool> *world0;      ///< pointer to the old state
    std::vector<bool> *world1;      ///< pointer to the new state

    float timesum = 0.0;            ///< internal variable which stores the accumulted time between 2 updates
    int fps = 5;                    ///< update frequency

    int ox = 20;                ///< x origin of the world
    int oy = 20;                ///< y origin of the world
    int width;             ///< horizontal dimension of the world    
    int height;           ///< vertical dimension of the world 
    int scale = 3;

public:
    GameOfLife() 
    { 
        sAppName = "Game of life"; 
    }

public:
    /// convert indices i,j to a 1D array index
    int idx(int i, int j) const
    {
        return i*width+j;
    }

    void set_random(std::vector<bool> &world)
    {
        // random array
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)       
                world[idx(i,j)] = !static_cast<bool> (rand() % 5);      
    }

    void set_empty(std::vector<bool> &world)
    {
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)
                world[idx(i,j)] = false;
    }


    bool OnUserCreate() override
    {
        // compute size of the world from margins and scale
        width = (this->ScreenWidth()-2*ox)/scale;
        height = (this->ScreenHeight()-2*oy)/scale;

        // allocate world arrays
        worldA.resize(width*height);
        worldB.resize(width*height);

        set_random(worldA);

        world0 = &worldA;
        world1 = &worldB;

        return true;
    }

    void calculate_new_world()
    {
        // calculate the new world as a function of the old one

        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)
            {
                // count neighbours
                int neigh = - static_cast<int>( (*world0)[idx(i,j)] );
                for(int ii=-1; ii<=1; ii++)
                    for(int jj=-1; jj<=1; jj++)
                        if (i+ii>0 && i+ii<height &&
                            j+jj>0 && jj+jj<width )
                            neigh += static_cast<int>( (*world0)[idx(i+ii,j+jj)] );
                
                if(neigh<2) // underpopulation
                    (*world1)[idx(i,j)] = false;
                else if(neigh>3) // overpopulation
                    (*world1)[idx(i,j)] = false;
                else if(neigh==3) // reproduction
                    (*world1)[idx(i,j)] = true;
                else
                    (*world1)[idx(i,j)] = (*world0)[idx(i,j)];
            }
    }

    void draw_world()
    {
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)
            {
                int colour = 0;
                if ((*world0)[idx(i,j)])
                    colour = 255;
                FillRect (ox+(j*scale), oy+(i*scale), 
                            scale, scale, 
                            olc::Pixel(colour, colour, colour));
            }                        
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::BLACK);

        if (GetKey(olc::Key::C).bHeld)
            set_empty(*world0);
        if (GetKey(olc::Key::R).bHeld)
            set_random(*world0);

        // get mouse pos
        olc::vi2d mpos = {(GetMouseX() - ox)/scale, 
                          (GetMouseY() - oy)/scale};

        // accumulate time
        timesum = timesum + fElapsedTime;
        float frametime = 1.0/fps;

        // check that it whether it is time to create a new generation
        if (timesum>frametime)
        {
            while(timesum>frametime)
                timesum -= frametime;

            calculate_new_world();
            // swap world
            auto w = world0;
            world0 = world1;
            world1 = w;            
        }

        draw_world();
        // draw borders
        DrawRect(ox-1, oy-1, scale*width+1, scale*height+1, olc::GREEN);

        // draw mouse pos in red
        if (mpos.x>=0 && mpos.x<width && mpos.y>=0 && mpos.y<height)
            FillRect (ox+(mpos.x*scale), oy+(mpos.y*scale), 
                        scale, scale, 
                        olc::RED);        

        this->DrawString({5,5}, "Game of Life");
        // using namespace std::chrono_literals;
        // std::this_thread::sleep_for(50ms);
        return true;
    }
};

int
main()
{
    GameOfLife demo;
    if (demo.Construct(600, 300, 2, 2))
        demo.Start();

    return 0;
}
