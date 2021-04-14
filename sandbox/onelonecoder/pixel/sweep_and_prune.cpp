//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\sweep_and_prune.exe

//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"
#include <algorithm>

struct Box
{
    Box(int i, olc::vf2d const &c, float Lx, float Ly)
    {
        pmin = c;
        pmax = c + olc::vf2d(Lx, Ly);
        id = i;
    }
    olc::vf2d pmin;
    olc::vf2d pmax;
    int id;
};

class SweepAndPrune : public olc::PixelGameEngine
{
    std::vector<Box *> boxes;

    int NBOXES = 10000;

public:
    SweepAndPrune() { sAppName = "Sweep and Prune"; }

public:
    bool OnUserCreate() override
    {
        int maxLx = 10;
        int maxLy = 10;
        int minLx = 5;
        int minLy = 5;

        for (int i = 0; i < NBOXES; ++i)
            boxes.push_back(new Box(i, {float(rand() % (ScreenWidth() - maxLx)), float(rand() % (ScreenHeight() - maxLy))},
                                    rand() % (maxLx - minLx) + minLx, rand() % (maxLy - minLy) + minLy));
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {

        // sort boxes
        std::vector<Box *> sBoxes = boxes;          // sorted list of boxes
        std::vector<std::pair<Box *, Box *>> pairs; // possible collisions

        if (true)
        {
            auto compFct = [](Box const *b1, Box const *b2) {
                return b1->pmin.x < b2->pmin.x;
            };

            std::sort(sBoxes.begin(), sBoxes.end(), compFct);

            if (GetKey(olc::Key::SPACE).bPressed && NBOXES < 100)
            {
                std::cout << "sorted list\n\t";
                for (auto b : sBoxes)
                {
                    std::cout << b->id << ", ";
                }
                std::cout << '\n';
            }

            auto it = sBoxes.begin();
            float xmin = (*it)->pmin.x;
            for (; it != sBoxes.end(); ++it)
            {
                auto it2 = it;
                while (++it2 != sBoxes.end())
                {
                    if ((*it2)->pmin.x <= (*it)->pmax.x)
                    {
                        // possible collision
                        pairs.push_back({*it, *it2});
                    }
                    else
                        break;
                    // it2++;
                }
            }
        }
        if (GetKey(olc::Key::SPACE).bPressed)
        {
            std::cout << pairs.size() << " pairs found\n";
            if (pairs.size() < 100)
                for (auto p : pairs)
                {
                    std::cout << "\t(" << p.first->id << ", " << p.second->id << ")\n";
                }
            std::cout << '\n';
            std::cout << pairs.size() << " tests to be done instead of " << NBOXES * (NBOXES - 1) << " (";
            std::cout << float(pairs.size()) / (NBOXES * (NBOXES - 1)) * 100 << " %)\n";
        }

        // draw

        Clear(olc::DARK_BLUE);

        for (auto b : boxes)
        {
            DrawRect(b->pmin, b->pmax - b->pmin, olc::WHITE);
            if (NBOXES < 100)
                DrawString((b->pmin + b->pmax) / 2 - olc::vi2d(3, 3), std::to_string(b->id), olc::YELLOW);
        }

        DrawString(olc::vi2d(10, 10), "Press <SPACE>", olc::YELLOW);

        return true;
    }
};

int
main()
{
    SweepAndPrune demo;
    if (demo.Construct(1000, 500, 1, 1))
        demo.Start();

    return 0;
}
