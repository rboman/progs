// Conway's Game of Life
//      using "Pixel Game Engine" from Javidx9
//      https://community.onelonecoder.com/
//
// 
// Wikipedia
//      https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
// It From Bit: Is The Universe A Cellular Automaton?
//      https://medium.com/starts-with-a-bang/it-from-bit-is-the-universe-a-cellular-automaton-4a5b1426ba6d
// Cellular Automata FAQ
//      http://cafaq.com/lifefaq/index.php
// best 152 patterns from the first quarter-century of Conway's Game of Life (Alan W. Hensel)
//      http://www.ibiblio.org/lifepatterns/lifep.zip
//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\gameoflife.exe

#include "config.h"
#include "olcPixelGameEngine.h"
#include <chrono>
#include <thread>
#include <fstream>
#include <algorithm>

std::vector<std::string> glider = {
    "  o",
    "o o",
    " oo"
};

std::vector<std::string> lwss = { // lightweight spaceship
    " oooo",
    "o   o",
    "    o",
    "o  o"
};

std::vector<std::string> mwss = { // middleweight spaceship
    " ooo",
    "ooooo",
    "ooo oo",
    "   oo"
};

std::vector<std::string> pulsar = { // middleweight spaceship
    "                 ",
    "     o     o     ",
    "     o     o     ",
    "     oo   oo     ",
    "                 ",
    " ooo  oo oo  ooo ",
    "   o o o o o o   ",
    "     oo   oo     ",
    "                 ",
    "     oo   oo     ",
    "   o o o o o o   ",
    " ooo  oo oo  ooo ",
    "                 ",
    "     oo   oo     ",
    "     o     o     ",
    "     o     o     ",
    "                 ",
};

std::vector<std::string> glider_gun = {
    "                                      ",
    "                         o            ",
    "                       o o            ",
    "             oo      oo            oo ",
    "            o   o    oo            oo ",
    " oo        o     o   oo               ",
    " oo        o   o oo    o o            ",
    "           o     o       o            ",
    "            o   o                     ",
    "             oo                       ",
    "                                      ",
};


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
    int width;                  ///< horizontal dimension of the world    
    int height;                 ///< vertical dimension of the world 
    int scale = 3;

    std::vector<std::vector<std::string> > objects;     ///< array storing interesting objects
    int object = 0;             ///< current object 

public:
    GameOfLife() 
    { 
        sAppName = "Game of life";
        // fill "objects" with interesting patterns
        objects.push_back(glider); 
        objects.push_back(lwss); 
        objects.push_back(mwss); 
        objects.push_back(pulsar); 
        objects.push_back(glider_gun);
        objects.push_back(load_lif(std::string(CMAKE_SOURCE_DIR) + "/life/RABBITS.LIF"));
    }

private:
    /// split a line of text into an array of tokens (words)
    ///     "a few words" => {"a", "few", "words"}
    std::vector<std::string> tokenize(std::string const &line)
    {
        std::vector<std::string> tokens;
        std::string token;
        std::stringstream iss(line);
        while (std::getline(iss, token,' '))
            tokens.push_back(token);

        // for(auto &t : tokens)
        //     std::cout << "token: " << t << '\n';
        return tokens;
    }

    /// load a .LIF file from Alan W. Hensel's library
    ///     see 
    std::vector<std::string> load_lif(std::string const &filename)
    {
        std::cout << "loading " << filename << '\n';

        std::ifstream infile(filename);
        if (! infile.is_open())
        {
            std::cerr << "ERROR: "<< filename << " file not found!\n";
            return std::vector<std::string>();
        }

        std::vector<std::string> object;
        int ox = 0;
        int oy = 0;

        std::string line;
        while (std::getline(infile, line))
        {
            // std::cout << "next line...\n";
            if (line[0]=='#')
            {
                auto tokens = tokenize(line);
                if(tokens[0]=="#Life")
                {
                    // TODO: check version
                    // std::cout << "file version: " << tokens[1] << '\n';
                } 
                else if(tokens[0]=="#P")
                {
                    ox = std::stoi(tokens[1]);
                    oy = std::stoi(tokens[2]);
                    // std::cout << "ox=" << ox << "; ";
                    // std::cout << "oy=" << oy << '\n';
                }
            }
            else
            {
                std::replace( line.begin(), line.end(), '*', 'o');
                std::replace( line.begin(), line.end(), '.', ' ');
                object.push_back(line);
            }
        }
        return object;
    }

    /// convert i, j indices to a 1D array index
    int idx(int i, int j) const
    {
        return i*width+j;
    }

    /// fill the world with a random set of dots
    void set_random(std::vector<bool> &world)
    {
        // random array
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)       
                world[idx(i,j)] = !static_cast<bool> (rand() % 5);      
    }

    /// clear the world
    void set_empty(std::vector<bool> &world)
    {
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)
                world[idx(i,j)] = false;
    }

    /// PGE function called when aplication starts
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

    /// calculate the new world as a function of the old one
    ///     apply the rules of the game of life
    void calculate_new_world()
    {
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

    /// draw the current state of the world onto the screen
    void draw_world()
    {
        for (int i = 0; i < height; i++)
            for (int j = 0; j < width; j++)
            {
                int colour = 0;     // black
                if ((*world0)[idx(i,j)])
                    colour = 255;   // white
                FillRect (ox+(j*scale), oy+(i*scale), 
                            scale, scale, 
                            olc::Pixel(colour, colour, colour));
            }                        
    }

    /// PGE function called at every new frame
    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::BLACK);

        // User interaction management

        if (GetKey(olc::Key::C).bHeld)
            set_empty(*world0);

        if (GetKey(olc::Key::R).bHeld)
            set_random(*world0);
            
        if (GetKey(olc::Key::LEFT).bPressed || GetMouseWheel() > 0)
        {
            object+=1;
            if (object==objects.size()) object=0;
            std::cout << "setting object to " << object << std::endl;
        }
        if (GetKey(olc::Key::RIGHT).bPressed || GetMouseWheel() < 0)
        {
            object-=1;
            if (object<0) object=objects.size()-1;
            std::cout << "setting object to " << object << std::endl;
        }      

        if (GetKey(olc::Key::UP).bPressed)
        {
            fps*=2;
            if (fps>120) fps=120;
            std::cout << "setting fps to " << fps << std::endl;
        }
        if (GetKey(olc::Key::DOWN).bPressed)
        {
            fps/=2;
            if (fps<1) fps=1;
            std::cout << "setting fps to " << fps << std::endl;
        } 

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

        // draw current object at mouse pos in red
        auto &curobj = objects[object];
        for(int i=0; i<curobj.size(); ++i)
            for(int j=0; j<curobj[i].size(); ++j)
            {
                if(curobj[i][j]!=' ')
                {
                    int posx = mpos.x+j;
                    int posy = mpos.y+i;
                    if (posx>=0 && posx<width && posy>=0 && posy<height)
                        FillRect (ox+(posx*scale), oy+(posy*scale), 
                                    scale, scale, 
                                    olc::RED);                    
                }
            }

        if (GetMouse(0).bPressed)
        {
            std::cout << "creating object\n";
            for(int i=0; i<curobj.size(); ++i)
                for(int j=0; j<curobj[i].size(); ++j)
                {
                    bool value = false;
                    if(curobj[i][j]!=' ')
                        value=true;

                    int posx = mpos.x+j;
                    int posy = mpos.y+i;
                    if (posx>=0 && posx<width && posy>=0 && posy<height)
                        (*world0)[idx(posy,posx)] = value;                    

                }

        }

        // draw title
        this->DrawString({5,5}, "Game of Life");

        return true;
    }
};

int
main()
{


    GameOfLife demo;
    //demo.load_lif(std::string(CMAKE_SOURCE_DIR) + "/life/RABBITS.LIF");

    
        if (demo.Construct(600, 300, 2, 2))
            demo.Start();
    
    return 0;
}
