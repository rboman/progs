// collisions
//
// sources:
//  Programming Balls #1 Circle Vs Circle Collisions
//      https://www.youtube.com/watch?v=LPzyNOHY3A4&t=1s&ab_channel=javidx9
//  Programming Balls #2 Circles V Edges Collisions
//      https://www.youtube.com/watch?v=ebq7L2Wtbl4&ab_channel=javidx9
//  Elastic collisions:
//      https://en.wikipedia.org/wiki/Elastic_collision
//  Building Collision Simulations: An Introduction to Computer Graphics
//      https://www.youtube.com/watch?v=eED4bSkYCB8&ab_channel=Reducible
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
#include <sstream>

struct Ball
{
    Ball(olc::vf2d const &centre, float r = 5.0f)
    {
        pos = centre;
        veloc = {0.0f, 0.0f};
        accel = {0.0f, 0.0f};
        radius = r;
        mass = 10.0f*r*r; // * 10.0f;
    }
    olc::vf2d pos;
    olc::vf2d veloc;
    olc::vf2d accel;
    float radius;
    float mass;

    olc::vf2d old_pos;     ///< position at the beginning of a sub-step
    float fSimTimeRemaing; ///< remaining time to be simulated
};

struct LineSegment
{
    olc::vf2d start; ///< starting point
    olc::vf2d end;   ///< ending point
    float radius;
};

class Collisions : public olc::PixelGameEngine
{
    // parameters
    float drag = 0.8f;
    olc::vf2d gravity = {0.0f, 90.0f};
    const int NBALLS = 1000;
    const float defBallRadius = 2.0f;
    const float defLineRadius = 4.0f;
    int nSimulationUpdates = 2; ///< split fElapsedTime into several epochs
    int maxSimulationSteps = 2; ///< split the epoch in several simulation steps

    // data structures
    std::vector<Ball> balls;      ///< array of balls
    Ball *selectedBall = nullptr; ///< current ball selected with the mouse

    Ball *mouseBall = nullptr; ///< current ball at mouse position

    std::vector<LineSegment> lines;      ///< array of lines
    LineSegment *selectedLine = nullptr; ///< current line selected with the mouse
    bool bSelectedLineStart = false;     ///< does the user selected the start or the end of the line?

public:
    Collisions() { sAppName = "Collisions"; }

private:
    bool OnUserCreate() override
    {
        // create balls
        for (int i = 0; i < NBALLS; ++i)
            balls.push_back(Ball({float(rand() % ScreenWidth()), float(rand() % ScreenHeight())}, rand() % 2 + defBallRadius));
        for (int i = 0; i < 5; ++i)
            balls.push_back(Ball({float(rand() % ScreenWidth()), float(rand() % ScreenHeight())}, rand() % 2 + 15.0f));
        for (int i = 0; i < 2; ++i)
            balls.push_back(Ball({float(rand() % ScreenWidth()), float(rand() % ScreenHeight())}, rand() % 2 + 25.0f));

        // create several lines
        lines.push_back({{18.0f, 327.0f}, {212.0f, 197.0f}, defLineRadius});
        lines.push_back({{131.0f, 28.0f}, {319.0f, 314.0f}, defLineRadius});
        lines.push_back({{302.0f, 41.0f}, {435.0f, 41.0f}, defLineRadius});
        lines.push_back({{347.0f, 208.0f}, {533.0f, 152.0f}, defLineRadius});

        return true;
    }

    bool doBallsOverlap(olc::vf2d const &c1, float r1, olc::vf2d const &c2, float r2) const
    {
        return (c2 - c1).mag2() <= (r1 + r2) * (r1 + r2);
    }
    bool isPointInBall(olc::vf2d const &c1, float r1, olc::vf2d const &c2) const
    {
        return (c2 - c1).mag2() <= r1 * r1;
    }

    void handleMouse()
    {
        // ---------------------------------------------------------------------
        // mouse interaction - pick a ball with the mouse and move it
        // ---------------------------------------------------------------------

        olc::vf2d mousePos = {float(GetMouseX()), float(GetMouseY())};

        // ball at mouse position (debug)
        mouseBall = nullptr;
        for (auto &ball : balls)
        {
            if (isPointInBall(ball.pos, ball.radius, mousePos))
            {
                mouseBall = &ball;
                break;
            }
        }

        // left or right mouse click => select a ball or a line extremity
        if (GetMouse(0).bPressed || GetMouse(1).bPressed)
        {
            // check balls
            //  loop over all the balls and test them
            selectedBall = nullptr;
            for (auto &ball : balls)
            {
                if (isPointInBall(ball.pos, ball.radius, mousePos))
                {
                    selectedBall = &ball;
                    break;
                }
            }

            // check lines
            //  loop over all the lines and test both extremities
            selectedLine = nullptr;
            for (auto &line : lines)
            {
                // test starting point
                if (isPointInBall(line.start, line.radius, mousePos))
                {
                    selectedLine = &line;
                    bSelectedLineStart = true;
                    break;
                }
                // test ending point
                if (isPointInBall(line.end, line.radius, mousePos))
                {
                    selectedLine = &line;
                    bSelectedLineStart = false;
                    break;
                }
            }
        }

        // right mouse button is held => move the ball or the line
        if (GetMouse(0).bHeld)
        {
            if (selectedBall != nullptr) // it could be held on nothing
                selectedBall->pos = mousePos;

            if (selectedLine != nullptr)
                if (bSelectedLineStart)
                    selectedLine->start = mousePos;
                else
                    selectedLine->end = mousePos;
        }
        // right mouse button is released => release selection
        if (GetMouse(0).bReleased)
        {
            selectedBall = nullptr;
            selectedLine = nullptr;
        }
        // left mouse button is released => set veloc for the selected ball
        if (GetMouse(1).bReleased)
        {
            if (selectedBall != nullptr)
            {
                selectedBall->veloc = -5.0f * (mousePos - selectedBall->pos);
                selectedBall = nullptr;
            }
        }
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        handleMouse();

        // ---------------------------------------------------------------------
        // collision detection and resolution
        // ---------------------------------------------------------------------

        std::vector<std::pair<Ball *, Ball *>> collidedPairs; // remember pair of balls that have been collided
        std::vector<Ball *> fakeBalls;

        // we split fElapsedTime into a sub intervals (called 'epochs' by javid)
        //  in order to be more accurate and avoid balls going through each other

        float fSimElapsedTime = fElapsedTime / nSimulationUpdates; // epoch duration
        int nSim = 0;                                              // keep track of max nb of simulations)

        // loop over the epochs (avoids large displacements)
        for (int i = 0; i < nSimulationUpdates; ++i)
        {
            // initialise the time remaining for each ball
            for (auto &ball : balls)
                ball.fSimTimeRemaing = fSimElapsedTime;

            // we also split the epoch in several simulation steps
            //  this is used to resolve more than 1 collision per ball per epoch.

            // loop over simulation steps
            //  (manage complex motion during 1 epoch)
            bool simDone = true;
            int j = 0;
            for (; j < maxSimulationSteps; ++j)
            {
                // update acceleration / veloc / position
                for (auto &ball : balls)
                {
                    // store the position before static collision test
                    ball.old_pos = ball.pos;

                    if (ball.fSimTimeRemaing > 0.0f)
                    {
                        // prescribed acceleration
                        ball.accel = -drag * ball.veloc + gravity;

                        // compute veloc from acceleration
                        ball.veloc += ball.accel * ball.fSimTimeRemaing;

                        // compute position from veloc
                        ball.pos += ball.veloc * ball.fSimTimeRemaing;

                        // make balls appear on the other side of the display
                        if (ball.pos.x < 0.0f)
                            ball.pos.x += ScreenWidth();
                        else if (ball.pos.x > ScreenWidth())
                            ball.pos.x -= ScreenWidth();
                        if (ball.pos.y < 0.0f)
                            ball.pos.y += ScreenHeight();
                        else if (ball.pos.y > ScreenHeight())
                            ball.pos.y -= ScreenHeight();

                        // if veloc is too low, set it to 0
                        if (ball.veloc.mag2() < 0.0001f)
                            ball.veloc = {0.0f, 0.0f};
                        // ! this could stop the ball in the middle of a jump
                    }
                }

                // -------------------------------------------------------------
                // STATIC COLLISION RESOLUTION
                //  check all the balls, one by one
                for (auto &ball : balls)
                {
                    bool collided = false;

                    // ... with all the lines
                    for (auto &line : lines)
                    {
                        // compute the closest point on line from ball
                        olc::vf2d v1 = line.end - line.start;
                        olc::vf2d v2 = ball.pos - line.start;
                        float fLineLength2 = v1.mag2();
                        float t = std::max<float>(0.0f, std::min<float>(fLineLength2, v1.dot(v2))) / fLineLength2;
                        olc::vf2d closest = line.start + t * v1;

                        // distance between line and ball
                        float D = (closest - ball.pos).mag();

                        // check collision & static resolution
                        if (D <= line.radius + ball.radius)
                        {
                            collided = true;
                            // create a fake ball with the same mass and opposite veloc
                            Ball *fakeBall = new Ball(closest, line.radius);
                            fakeBall->mass = ball.mass;
                            fakeBall->veloc = -ball.veloc;

                            collidedPairs.push_back({&ball, fakeBall});
                            fakeBalls.push_back(fakeBall);

                            float overlap = (D - ball.radius - fakeBall->radius);
                            ball.pos -= overlap * (ball.pos - fakeBall->pos) / D;
                        }
                    }

                    // ... with all the other balls
                    for (auto &target : balls)
                    {
                        if (&ball != &target)
                        {
                            // static resolution
                            if (doBallsOverlap(ball.pos, ball.radius, target.pos, target.radius))
                            {
                                collided = true;
                                // ball and target overlap each other
                                collidedPairs.push_back({&ball, &target});

                                // distance between the 2 balls
                                olc::vf2d distance = ball.pos - target.pos;
                                float D = distance.mag();
                                float overlap = 0.5f * (D - ball.radius - target.radius);

                                // not in javid's code:
                                //  take mass into consideration for the static resolution
                                float factor = ball.mass/(ball.mass+target.mass);

                                // displace the 2 balls
                                ball.pos -= (1.0f-factor) * overlap * distance / D;
                                target.pos += factor * overlap * distance / D;
                            }
                        }
                    }

                    // time displacement
                    if (collided)
                    {
                        float fIntendedSpeed = ball.veloc.mag();
                        float fIntendedDistance = fIntendedSpeed * ball.fSimTimeRemaing;
                        float fActualDistance = (ball.pos - ball.old_pos).mag();

                        if (fIntendedSpeed == 0.0f)
                        {
                            ball.fSimTimeRemaing = 0.0f;
                            // std::cout << "fIntendedSpeed=0.0f! fActualDistance=" << fActualDistance << "\n";
                        }
                        else
                        {
                            float fActualTime = fActualDistance / fIntendedSpeed;
                            ball.fSimTimeRemaing -= fActualTime;
                            // if(ball.fSimTimeRemaing<fSimElapsedTime/100.0f)
                            //     ball.fSimTimeRemaing = 0.0f;
                        }
                    }
                    else
                        ball.fSimTimeRemaing = 0.0f;

                    if (ball.fSimTimeRemaing > 0.0f) // if a ball still have some simTime 
                        simDone = false;
                }

                // dynamic resolution (takes place after the static one)
                for (auto p : collidedPairs)
                {
                    Ball *b1 = p.first;
                    Ball *b2 = p.second;

                    // optimised wikipedia version
                    //  (https://en.wikipedia.org/wiki/Elastic_collision)
                    olc::vf2d normal = (b2->pos - b1->pos).norm();
                    olc::vf2d k = b1->veloc - b2->veloc;
                    float p = 2.0f * normal.dot(k) / (b1->mass + b2->mass);
                    b1->veloc -= p * b2->mass * normal;
                    b2->veloc += p * b1->mass * normal;

                } // dynamic resolution

                // delete temporary fake balls
                for (auto &b : fakeBalls)
                    delete b;
                fakeBalls.clear();

                collidedPairs.clear();

                if (simDone)
                    break;

            } // j (simulation steps)

            nSim = std::max(j + 1, nSim);

        } // i (epochs)

        // Drawing code --------------------------------------------------------
        drawScene(nSim);

        return true;
    }

    void drawScene(int nSim)
    {
        Clear(olc::DARK_BLUE);

        for (auto &b : balls)
        {
            DrawCircle(b.pos, int32_t(b.radius), olc::WHITE);
            float theta = atan2f(b.veloc.y, b.veloc.x);
            DrawLine(b.pos, b.pos + b.radius * olc::vf2d(cosf(theta), sinf(theta)),
                     olc::WHITE);
        }

        for (auto &line : lines)
        {
            FillCircle(line.start, int(line.radius), olc::GREY);
            FillCircle(line.end, int(line.radius), olc::GREY);
            olc::vf2d unitn = (line.end - line.start).perp().norm();
            DrawLine(line.start + line.radius * unitn, line.end + line.radius * unitn, olc::GREY);
            DrawLine(line.start - line.radius * unitn, line.end - line.radius * unitn, olc::GREY);
        }

        // for (auto p : collidedPairs)
        // {
        //     Ball *b1 = p.first;
        //     Ball *b2 = p.second;
        //     DrawLine(b1->pos, b2->pos, olc::RED);
        // }
        olc::vi2d mousePos(GetMouseX(), GetMouseY());

        if (selectedBall != nullptr)
        {
            DrawLine(selectedBall->pos, mousePos, olc::BLUE);
        }

        // draw debug info for the ball at mouse position
        if (mouseBall != nullptr)
        {

            std::stringstream str;
            str << "pos = " << mouseBall->pos << '\n';
            str << "veloc = " << mouseBall->veloc << '\n';
            str << "accel = " << mouseBall->accel << '\n';
            DrawString(mousePos, str.str(), olc::YELLOW);
        }

        std::stringstream str;
        str << mousePos << '\n';
        str << "nSim=" << nSim;
        DrawString({10, 10}, str.str(), olc::YELLOW);
    }
};

int
main()
{
    Collisions demo;
    if (demo.Construct(600, 400, 2, 2))
        demo.Start();

    return 0;
}
