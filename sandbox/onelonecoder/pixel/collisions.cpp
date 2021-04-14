// collisions
//
// videos:
//  Programming Balls #1 Circle Vs Circle Collisions
//      https://www.youtube.com/watch?v=LPzyNOHY3A4&t=1s&ab_channel=javidx9
//  Programming Balls #2 Circles V Edges Collisions
//      https://www.youtube.com/watch?v=ebq7L2Wtbl4&ab_channel=javidx9
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
#include <algorithm>

struct Ball
{
    olc::vf2d centre;
    olc::vf2d velocity;
    olc::vf2d accel;
    float radius;
    float mass;
    int id;

    olc::vf2d old_centre;
    float fSimTimeRemaing;
};

struct LineSegment
{
    olc::vf2d start;
    olc::vf2d end;
    float radius;
};

class Collisions : public olc::PixelGameEngine
{
    std::vector<Ball> balls;
    Ball *selectedBall = nullptr;

    std::vector<LineSegment> lines;
    LineSegment *selectedLine = nullptr;
    bool bSelectedLineStart = false;

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
        // addBall(ScreenWidth() * 0.25f, ScreenHeight() * 0.5f, defRadius);
        // addBall(ScreenWidth() * 0.75f, ScreenHeight() * 0.5f, defRadius);

        for (int i = 0; i < 1; ++i)
            addBall(rand() % ScreenWidth(), rand() % ScreenHeight(), rand() % 16 + 2);

        float fLineRadius = 5.0f;
        lines.push_back({{30.0f, 30.0f}, {100.0f, 30.0f}, fLineRadius});

        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {

        // convenient functions

        auto doBallsOverlap = [](olc::vf2d c1, float r1, olc::vf2d c2, float r2) {
            return ((c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y) <= (r1 + r2) * (r1 + r2));
        };
        auto isPointInBall = [](olc::vf2d c1, float r1, olc::vf2d c2) {
            return (c1.x - c2.x) * (c1.x - c2.x) + (c1.y - c2.y) * (c1.y - c2.y) < (r1) * (r1);
        };

        // mouse interaction - pick a ball with the mouse and move it

        if (GetMouse(0).bPressed || GetMouse(1).bPressed)
        {
            olc::vf2d pos = {float(GetMouseX()), float(GetMouseY())};
            selectedBall = nullptr;
            for (auto &ball : balls)
            {

                if (isPointInBall(ball.centre, ball.radius, pos))
                {
                    selectedBall = &ball;
                    break;
                }
            }

            selectedLine = nullptr;
            for (auto &line : lines)
            {
                if (isPointInBall(line.start, line.radius, pos))
                {
                    selectedLine = &line;
                    bSelectedLineStart = true;
                    break;
                }
                if (isPointInBall(line.end, line.radius, pos))
                {
                    selectedLine = &line;
                    bSelectedLineStart = false;
                    break;
                }
            }
        }
        if (GetMouse(0).bHeld)
        {
            olc::vf2d pos = {float(GetMouseX()), float(GetMouseY())};
            if (selectedBall != nullptr)
            {
                selectedBall->centre = pos;
            }

            if (selectedLine != nullptr)
            {
                if (bSelectedLineStart)
                {
                    selectedLine->start = pos;
                }
                else
                    selectedLine->end = pos;
            }
        }
        if (GetMouse(0).bReleased)
        {
            selectedBall = nullptr;
            selectedLine = nullptr;
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

        // collision detection and resolution ----------------------------------

        std::vector<std::pair<Ball *, Ball *>> collidedPairs; // remember pair of balls that have been collided
        std::vector<Ball *> fakeBalls;

        // we split fElapsedTime into a sub intervals (epochs)
        // in order to be more accurate and avoid balls going through each other
        int nSimulationUpdates = 4;
        float fSimElapsedTime = fElapsedTime / nSimulationUpdates; // epoch length

        int maxSimulationSteps = 15; // this is used to resolve more than 1 collision per ball per epoch.

        for (int i = 0; i < nSimulationUpdates; ++i) // loop over the epochs (avoids large displacements)
        {
            for (auto &ball : balls)
                ball.fSimTimeRemaing = fSimElapsedTime;

            for (int j = 0; j < maxSimulationSteps; ++j) // loop over simulation steps (manage complex motion during 1 epoch)
            {

                // update velocity
                for (auto &ball : balls)
                {
                    ball.old_centre = ball.centre;

                    if (ball.fSimTimeRemaing > 0.0f)
                    {

                        ball.accel.x = -ball.velocity.x * 0.8f;
                        ball.accel.y = -ball.velocity.y * 0.8f;

                        ball.velocity.x += ball.accel.x * ball.fSimTimeRemaing;
                        ball.velocity.y += ball.accel.y * ball.fSimTimeRemaing;

                        ball.centre.x += ball.velocity.x * ball.fSimTimeRemaing;
                        ball.centre.y += ball.velocity.y * ball.fSimTimeRemaing;

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
                }

                // naive approach
                for (auto &ball : balls)
                {

                    for (auto &line : lines)
                    {
                        olc::vf2d v1 = {line.end.x - line.start.x, line.end.y - line.start.y};
                        olc::vf2d v2 = {ball.centre.x - line.start.x, ball.centre.y - line.start.y};
                        float fLineLength2 = v1.x * v1.x + v1.y * v1.y;

                        float t = std::max<float>(0.0f, std::min<float>(fLineLength2, v2.x * v1.x + v2.y * v1.y)) / fLineLength2;

                        olc::vf2d closest = {line.start.x + t * v1.x, line.start.y + t * v1.y};

                        float D = sqrtf((closest.x - ball.centre.x) * (closest.x - ball.centre.x) +
                                        (closest.y - ball.centre.y) * (closest.y - ball.centre.y));

                        if (D <= line.radius + ball.radius)
                        {

                            Ball *fakeBall = new Ball();
                            fakeBall->centre = closest;
                            fakeBall->radius = line.radius;
                            fakeBall->mass = ball.mass;
                            fakeBall->velocity = {-ball.velocity.x, -ball.velocity.y};

                            collidedPairs.push_back({&ball, fakeBall});
                            fakeBalls.push_back(fakeBall);

                            float overlap = (D - ball.radius - fakeBall->radius);
                            ball.centre.x -= overlap * (ball.centre.x - fakeBall->centre.x) / D;
                            ball.centre.y -= overlap * (ball.centre.y - fakeBall->centre.y) / D;
                        }
                    }

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

                    // time displacement
                    float fIntendedSpeed = sqrtf(ball.velocity.x * ball.velocity.x + ball.velocity.y * ball.velocity.y);
                    float fIntendedDistance = fIntendedSpeed * ball.fSimTimeRemaing;
                    float fActualDistance = sqrt((ball.centre.x - ball.old_centre.x) * (ball.centre.x - ball.old_centre.x) +
                                                 (ball.centre.y - ball.old_centre.y) * (ball.centre.y - ball.old_centre.y));
                    float fActualTime = fActualDistance / fIntendedSpeed;
                    ball.fSimTimeRemaing -= fActualTime;
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
                } // dynamic resolution

                for (auto &b : fakeBalls)
                    delete b;
                fakeBalls.clear();

                collidedPairs.clear();

            } // j (simulation steps)
        }     // i (epochs)

        // Drawing code --------------------------------------------------------

        Clear(olc::DARK_BLUE);

        for (auto &b : balls)
        {
            DrawCircle(b.centre, int32_t(b.radius), olc::WHITE);
            float theta = atan2f(b.velocity.y, b.velocity.x);
            DrawLine(b.centre, olc::vi2d(b.centre.x + b.radius * cosf(theta), b.centre.y + b.radius * sinf(theta)),
                     olc::WHITE);
        }

        for (auto &line : lines)
        {
            FillCircle(line.start, line.radius, olc::GREY);
            FillCircle(line.end, line.radius, olc::GREY);
            olc::vf2d normal = {-(line.end.y - line.start.y), line.end.x - line.start.x};
            float d = sqrtf(normal.x * normal.x + normal.y * normal.y);
            normal.x /= d;
            normal.y /= d;
            DrawLine(olc::vi2d(line.start.x + line.radius * normal.x, line.start.y + line.radius * normal.y),
                     olc::vi2d(line.end.x + line.radius * normal.x, line.end.y + line.radius * normal.y), olc::GREY);
            DrawLine(olc::vi2d(line.start.x - line.radius * normal.x, line.start.y - line.radius * normal.y),
                     olc::vi2d(line.end.x - line.radius * normal.x, line.end.y - line.radius * normal.y), olc::GREY);
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
