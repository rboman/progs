// collisions
//
// videos:
//  Programming Balls #1 Circle Vs Circle Collisions
//      https://www.youtube.com/watch?v=LPzyNOHY3A4&t=1s&ab_channel=javidx9
//  Elastic collisions:
//      https://en.wikipedia.org/wiki/Elastic_collision
//
// Future:
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
    float mass;
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
        b.mass = r * 10.0f;
        b.id = balls.size();
        balls.push_back(b);
    }

    bool OnUserCreate() override
    {
        float defRadius = 10.0f;
        addBall(ScreenWidth() * 0.25f, ScreenHeight() * 0.5f, defRadius);
        addBall(ScreenWidth() * 0.75f, ScreenHeight() * 0.5f, defRadius);

        for (int i = 0; i < 10; ++i)
            addBall(rand() % ScreenWidth(), rand() % ScreenHeight(), rand() % 16 + 2);

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

        // mouse interaction - pick a ball with the mouse and move it
        if (GetMouse(0).bPressed || GetMouse(1).bPressed)
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
        if (GetMouse(1).bReleased)
        {
            if (selectedBall != nullptr)
            {
                selectedBall->velocity.x = -5.0f * (float(GetMouseX()) - selectedBall->centre.x);
                selectedBall->velocity.y = -5.0f * (float(GetMouseY()) - selectedBall->centre.y);
            }

            selectedBall = nullptr;
        }

        std::vector<std::pair<Ball *, Ball *>> collidedPairs; // remember pair of balls that have been collided

        // update velocity
        for (auto &ball : balls)
        {
            ball.accel.x = -ball.velocity.x * 0.8f;
            ball.accel.y = -ball.velocity.y * 0.8f;

            ball.velocity.x += ball.accel.x * fElapsedTime;
            ball.velocity.y += ball.accel.y * fElapsedTime;

            ball.centre.x += ball.velocity.x * fElapsedTime;
            ball.centre.y += ball.velocity.y * fElapsedTime;

            if (ball.centre.x < 0.0f)
                ball.centre.x += ScreenWidth();
            if (ball.centre.y < 0.0f)
                ball.centre.y += ScreenHeight();
            if (ball.centre.x > ScreenWidth())
                ball.centre.x -= ScreenWidth();
            if (ball.centre.y > ScreenHeight())
                ball.centre.y -= ScreenHeight();

            if ((ball.velocity.x * ball.velocity.x + ball.velocity.y * ball.velocity.y) < 0.01f)
            {
                ball.velocity.x = 0.0f;
                ball.velocity.y = 0.0f;
            }
        }

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

        // dynamic resolution (takes place after the static one)
        for (auto p : collidedPairs)
        {
            Ball *b1 = p.first;
            Ball *b2 = p.second;
            float D = sqrtf((b1->centre.x - b2->centre.x) * (b1->centre.x - b2->centre.x) + (b1->centre.y - b2->centre.y) * (b1->centre.y - b2->centre.y));

            olc::vf2d normal = {(b2->centre.x - b1->centre.x) / D, (b2->centre.y - b1->centre.y) / D};
            olc::vf2d tangent = {-normal.y, normal.x};

            // tangent response
            // float dpTan1 = b1->velocity.x * tangent.x + b1->velocity.y * tangent.y;
            // float dpTan2 = b2->velocity.x * tangent.x + b2->velocity.y * tangent.y;

            // normal response
            // float dpNor1 = b1->velocity.x * normal.x + b1->velocity.y * normal.y;
            // float dpNor2 = b2->velocity.x * normal.x + b2->velocity.y * normal.y;

            // 1D elastic collision : https://en.wikipedia.org/wiki/Elastic_collision
            // v_1 = ( u_1*(m_1-m_2) + 2*m_2*u_2 )/(m_1+m_2)
            // v_2 = ( u_2*(m_2-m_1) + 2*m_1*u_2 )/(m_1+m_2)

            // float m1 = (dpNor1 * (b1->mass - b2->mass) + 2 * b2->mass * dpNor2) / (b1->mass + b2->mass);
            // float m2 = (dpNor2 * (b2->mass - b1->mass) + 2 * b1->mass * dpNor1) / (b1->mass + b2->mass);

            // b1->velocity = {tangent.x * dpTan1 + m1 * normal.x, tangent.y * dpTan1 + m1 * normal.y};
            // b2->velocity = {tangent.x * dpTan2 + m2 * normal.x, tangent.y * dpTan2 + m2 * normal.y};

            // optimised wikipedia version
            float kx = (b1->velocity.x - b2->velocity.x);
            float ky = (b1->velocity.y - b2->velocity.y);
            float p = 2.0 * (normal.x * kx + normal.y * ky) / (b1->mass + b2->mass);
            b1->velocity = {b1->velocity.x - p * b2->mass * normal.x,
                            b1->velocity.y - p * b2->mass * normal.y};
            b2->velocity = {b2->velocity.x + p * b1->mass * normal.x,
                            b2->velocity.y + p * b1->mass * normal.y};
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

        if (selectedBall != nullptr)
        {
            olc::vf2d pos = {float(GetMouseX()), float(GetMouseY())};
            DrawLine(selectedBall->centre, pos, olc::BLUE);
        }

        return true;
    }
};

int
main()
{
    Collisions demo;
    if (demo.Construct(300, 200, 3, 3))
        demo.Start();

    return 0;
}
