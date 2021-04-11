// collisions
//
// videos:
//  Programming Balls #1 Circle Vs Circle Collisions
//      https://www.youtube.com/watch?v=LPzyNOHY3A4&t=1s&ab_channel=javidx9
//  Convex Polygon Collisions #1
//      https://www.youtube.com/watch?v=7Ik2vowGcU0&ab_channel=javidx9
//  Circle Vs Rectangle Collisions
//      https://www.youtube.com/watch?v=D2a5fHX-Qrs&t=3s&ab_channel=javidx9
//
//
// build & run
//  cmake -A x64 .. && cmake --build . --config Release && Release\collisions.exe

#include "olcPixelGameEngine.h"

struct Ball
{
    olc::vf2d centre;
    olc::vf2d velocity;
    olc::vf2d accel;
    float radius;
    int id;
};

class Collisions : public olc::PixelGameEngine
{
    std::vector<Ball> balls;
    Ball *selectedBall = nullptr;

public:
    Collisions() { sAppName = "Collisions"; }

private:
    void addBall(float x, float y, float r = 5.0f)
    {
        Ball b;
        b.centre = {x, y};
        b.velocity = {0.0f, 0.0f};
        b.accel = {0.0f, 0.0f};
        b.radius = r;
        b.id = balls.size();
        balls.push_back(b);
    }

    bool OnUserCreate() override
    {
        float defRadius = 10.0f;
        addBall(ScreenWidth() * 0.25f, ScreenHeight() * 0.5f, defRadius);
        addBall(ScreenWidth() * 0.75f, ScreenHeight() * 0.5f, defRadius);

        for (int i = 0; i < 10; ++i)
            addBall(rand() % ScreenWidth(), rand() % ScreenHeight(), defRadius);

        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        auto doBallsOverlap = [](olc::vf2d c1, float r1, olc::vf2d c2, float r2) {
            // std::cout << (c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y) << " <= " << (r1 - r2) << "\n";
            return ((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y) <= (r1 + r2) * (r1 + r2));
        };
        auto isPointInBall = [](olc::vf2d c1, float r1, olc::vf2d c2) {
            return (c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y) < (r1) * (r1);
        };

        // mouse interaction
        if (GetMouse(0).bPressed)
        {
            selectedBall = nullptr;
            for (auto &ball : balls)
            {
                olc::vf2d pos = {float(GetMouseX()), float(GetMouseY())};
                if (isPointInBall(ball.centre, ball.radius, pos))
                {
                    selectedBall = &ball;
                    break;
                }
            }
        }
        if (GetMouse(0).bHeld)
        {
            if (selectedBall != nullptr)
            {
                olc::vf2d pos = {float(GetMouseX()), float(GetMouseY())};
                selectedBall->centre = pos;
            }
        }
        if (GetMouse(0).bReleased)
        {
            selectedBall = nullptr;
        }

        std::vector<std::pair<Ball *, Ball *>> collidedPairs;

        // naive approach
        for (auto &ball : balls)
        {
            for (auto &target : balls)
            {
                if (ball.id != target.id)
                {
                    // static resolution
                    if (doBallsOverlap(ball.centre, ball.radius, target.centre, target.radius))
                    {

                        // std::cout << "overlap!\n";
                        collidedPairs.push_back({&ball, &target});

                        // distance between balls
                        float D = sqrtf((ball.centre.x - target.centre.x) * (ball.centre.x - target.centre.x) + (ball.centre.y - target.centre.y) * (ball.centre.y - target.centre.y));
                        float overlap = 0.5f * (D - ball.radius - target.radius);

                        // displace current ball
                        ball.centre.x -= overlap * (ball.centre.x - target.centre.x) / D;
                        ball.centre.y -= overlap * (ball.centre.y - target.centre.y) / D;

                        // displace target ball
                        target.centre.x += overlap * (ball.centre.x - target.centre.x) / D;
                        target.centre.y += overlap * (ball.centre.y - target.centre.y) / D;
                    }
                }
            }
        }

        Clear(olc::DARK_BLUE);

        for (auto &b : balls)
        {
            DrawCircle(b.centre, int32_t(b.radius), olc::WHITE);
            float theta = atan2f(b.velocity.y, b.velocity.x);
            DrawLine(b.centre, olc::vi2d(b.centre.x + b.radius * cosf(theta), b.centre.y + b.radius * sinf(theta)),
                     olc::WHITE);
        }

        for (auto p : collidedPairs)
        {
            Ball *b1 = p.first;
            Ball *b2 = p.second;
            DrawLine(b1->centre, b2->centre, olc::RED);
        }

        return true;
    }
};

int
main()
{
    Collisions demo;
    if (demo.Construct(256, 100, 4, 4))
        demo.Start();

    return 0;
}
