// this code comes from https://www.youtube.com/watch?v=alhpH6ECFvQ
// "Coding Challenge #132: Fluid Simulation"
// it has been adapted to the "Pixel Game Engine" from Javidx9
// https://community.onelonecoder.com/
//
// build & run
// cmake -A x64 .. && cmake --build . --config Release && Release\fluid.exe

#include "config.h"
//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"
#include "colour.h"
#include <vector>

int N = 200;
int iter = 4;
int SCALE = 5;
float t = 0;

// ███████ ██    ██ ███    ██  ██████ ████████ ██  ██████  ███    ██ ███████ 
// ██      ██    ██ ████   ██ ██         ██    ██ ██    ██ ████   ██ ██      
// █████   ██    ██ ██ ██  ██ ██         ██    ██ ██    ██ ██ ██  ██ ███████ 
// ██      ██    ██ ██  ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██ 
// ██       ██████  ██   ████  ██████    ██    ██  ██████  ██   ████ ███████ 

template <typename T>
T
constrain(T v, T vmin, T vmax)
{
    if (v < vmin)
        return vmin;
    if (v > vmax)
        return vmax;
    return v;
}

inline int
IX(int x, int y)
{
    x = constrain(x, 0, N - 1);
    y = constrain(y, 0, N - 1);
    return x + (y * N);
}

void
set_bnd(int b, std::vector<float> &x)
{
    for (int i = 1; i < N - 1; ++i)
    {
        x[IX(i, 0)] = b == 2 ? -x[IX(i, 1)] : x[IX(i, 1)];
        x[IX(i, N - 1)] = b == 2 ? -x[IX(i, N - 2)] : x[IX(i, N - 2)];
    }
    for (int j = 1; j < N - 1; ++j)
    {
        x[IX(0, j)] = b == 1 ? -x[IX(1, j)] : x[IX(1, j)];
        x[IX(N - 1, j)] = b == 1 ? -x[IX(N - 2, j)] : x[IX(N - 2, j)];
    }

    x[IX(0, 0)] = 0.5f * (x[IX(1, 0)] + x[IX(0, 1)]);
    x[IX(0, N - 1)] = 0.5f * (x[IX(1, N - 1)] + x[IX(0, N - 2)]);
    x[IX(N - 1, 0)] = 0.5f * (x[IX(N - 2, 0)] + x[IX(N - 1, 1)]);
    x[IX(N - 1, N - 1)] = 0.5f * (x[IX(N - 2, N - 1)] + x[IX(N - 1, N - 2)]);
}

void
lin_solve(int b, std::vector<float> &x,
          std::vector<float> const &x0,
          float a, float c)
{
    float cRecip = 1.0f / c;
    for (int k = 0; k < iter; k++)
    {
        for (int j = 1; j < N - 1; j++)
            for (int i = 1; i < N - 1; i++)
                x[IX(i, j)] =
                    (x0[IX(i, j)] + a * (x[IX(i + 1, j)] + x[IX(i - 1, j)] + x[IX(i, j + 1)] + x[IX(i, j - 1)])) * cRecip;
        set_bnd(b, x);
    }
}

void
diffuse(int b,
        std::vector<float> &x,
        std::vector<float> const &x0,
        float diff, float dt)
{
    float a = dt * diff * (N - 2) * (N - 2);
    lin_solve(b, x, x0, a, 1 + 4 * a);
}

void
project(std::vector<float> &velocX,
        std::vector<float> &velocY,
        std::vector<float> &p,
        std::vector<float> &div)
{
    for (int j = 1; j < N - 1; ++j)
        for (int i = 1; i < N - 1; ++i)
        {
            div[IX(i, j)] = -0.5f * (velocX[IX(i + 1, j)] - velocX[IX(i - 1, j)] + velocY[IX(i, j + 1)] - velocY[IX(i, j - 1)]) / N;
            p[IX(i, j)] = 0;
        }

    set_bnd(0, div);
    set_bnd(0, p);
    lin_solve(0, p, div, 1, 4);

    for (int j = 1; j < N - 1; ++j)
        for (int i = 1; i < N - 1; ++i)
        {
            velocX[IX(i, j)] -= 0.5f * (p[IX(i + 1, j)] - p[IX(i - 1, j)]) * N;
            velocY[IX(i, j)] -= 0.5f * (p[IX(i, j + 1)] - p[IX(i, j - 1)]) * N;
        }

    set_bnd(1, velocX);
    set_bnd(2, velocY);
}

void
advect(int b,
       std::vector<float> &d,
       std::vector<float> const &d0,
       std::vector<float> const &velocX,
       std::vector<float> const &velocY,
       float dt)
{
    float dtx = dt * (N - 2);
    float dty = dt * (N - 2);

    float Nfloat = (float)N;
    float ifloat, jfloat;
    int i, j;

    for (j = 1, jfloat = 1.0f; j < N - 1; j++, jfloat++)
    {
        for (i = 1, ifloat = 1.0f; i < N - 1; i++, ifloat++)
        {
            float tmp1 = dtx * velocX[IX(i, j)];
            float tmp2 = dty * velocY[IX(i, j)];
            float x = ifloat - tmp1;
            float y = jfloat - tmp2;

            if (x < 0.5f)
                x = 0.5f;
            if (x > Nfloat + 0.5f)
                x = Nfloat + 0.5f;
            float i0 = floor(x);
            float i1 = i0 + 1.0f;
            if (y < 0.5f)
                y = 0.5f;
            if (y > Nfloat + 0.5f)
                y = Nfloat + 0.5f;
            float j0 = floor(y);
            float j1 = j0 + 1.0f;

            float s1 = x - i0;
            float s0 = 1.0f - s1;
            float t1 = y - j0;
            float t0 = 1.0f - t1;

            int i0i = int(i0);
            int i1i = int(i1);
            int j0i = int(j0);
            int j1i = int(j1);

            // DOUBLE CHECK THIS!!!
            d[IX(i, j)] =
                s0 * (t0 * d0[IX(i0i, j0i)] + t1 * d0[IX(i0i, j1i)]) +
                s1 * (t0 * d0[IX(i1i, j0i)] + t1 * d0[IX(i1i, j1i)]);
        }
    }

    set_bnd(b, d);
}

// ███████ ██      ██    ██ ██ ██████ 
// ██      ██      ██    ██ ██ ██   ██ 
// █████   ██      ██    ██ ██ ██   ██ 
// ██      ██      ██    ██ ██ ██   ██ 
// ██      ███████  ██████  ██ ██████ 

class Fluid
{
    // int size;
    float diff;
    float visc;

    std::vector<float> s;
    std::vector<float> density;

    std::vector<float> Vx;
    std::vector<float> Vy;

    std::vector<float> Vx0;
    std::vector<float> Vy0;

public:
    Fluid(float diffusion, float viscosity)
    {
        // this->size = N;
        this->diff = diffusion;
        this->visc = viscosity;

        this->s.resize(N * N, 0.0);
        this->density.resize(N * N, 0.0);

        this->Vx.resize(N * N, 0.0);
        this->Vy.resize(N * N, 0.0);

        this->Vx0.resize(N * N, 0.0);
        this->Vy0.resize(N * N, 0.0);
    }

    void step(float dt)
    {
        // int N = this->size;
        float visc = this->visc;
        float diff = this->diff;
        std::vector<float> &Vx = this->Vx;
        std::vector<float> &Vy = this->Vy;
        std::vector<float> &Vx0 = this->Vx0;
        std::vector<float> &Vy0 = this->Vy0;
        std::vector<float> &s = this->s;
        std::vector<float> &density = this->density;

        diffuse(1, Vx0, Vx, visc, dt);
        diffuse(2, Vy0, Vy, visc, dt);

        project(Vx0, Vy0, Vx, Vy);

        advect(1, Vx, Vx0, Vx0, Vy0, dt);
        advect(2, Vy, Vy0, Vx0, Vy0, dt);

        project(Vx, Vy, Vx0, Vy0);

        diffuse(0, s, density, diff, dt);
        advect(0, density, s, Vx, Vy, dt);
    }

    void addDensity(int x, int y, float amount)
    {
        int index = IX(x, y);
        this->density[index] += amount;
    }

    void addVelocity(int x, int y, float amountX, float amountY)
    {
        int index = IX(x, y);
        this->Vx[index] += amountX;
        this->Vy[index] += amountY;
    }

    void renderD(olc::PixelGameEngine &pge)
    {
        for (int i = 0; i < N; ++i)
        {
            for (int j = 0; j < N; ++j)
            {
                // float x = i * SCALE;
                // float y = j * SCALE;
                // float d = this->density[IX(i, j)];
                // fill((d + 50) % 255, 200, d);
                // noStroke();
                // square(x, y, SCALE);

                // simple RGB
                // int d = this->density[IX(i, j)];
                // if (d > 255)
                //     d = 255;
                // pge.Draw(i, j, olc::Pixel(d, d, d));

                int d = (int)this->density[IX(i, j)];
                int v = d;
                if (v > 255)
                    v = 255;
                HsvColor hsv{unsigned char((d + 50) % 255), unsigned char(200), unsigned char(v)};
                RgbColor rgb = HsvToRgb(hsv);
                //pge.Draw(i, j, olc::Pixel(rgb.r, rgb.g, rgb.b));
                pge.FillRect(i*SCALE, j*SCALE, SCALE, SCALE, olc::Pixel(rgb.r, rgb.g, rgb.b));
            }
        }
    }

    void renderV(olc::PixelGameEngine &pge)
    {
        for (int i = 0; i < N; i+=5)
        {
            for (int j = 0; j < N; j+=5)
            {
                float x = i * SCALE;
                float y = j * SCALE;
                float vx = this->Vx[IX(i, j)];
                float vy = this->Vy[IX(i, j)];
                //stroke(255);

                // if (!(abs(vx) < 0.1 && abs(vy) <= 0.1))
                // {
                    //line(x, y, x + vx * SCALE, y + vy * SCALE);
                    pge.DrawLine(x, y, x + vx * SCALE * 10, y + vy * SCALE * 10);
                // }
            }
        }
    }

    void fadeD()
    {
        for (int i = 0; i < this->density.size(); i++)
        {
            float d = density[i];
            density[i] = (float)constrain(int(d - 0.02), 0, 255);
        }
    }
};

// ███████ ███    ██  ██████  ██ ███    ██ ███████ 
// ██      ████   ██ ██       ██ ████   ██ ██      
// █████   ██ ██  ██ ██   ███ ██ ██ ██  ██ █████ 
// ██      ██  ██ ██ ██    ██ ██ ██  ██ ██ ██    
// ███████ ██   ████  ██████  ██ ██   ████ ███████

class CFD : public olc::PixelGameEngine
{
    Fluid *fluid;
    int32_t pmx; ///< previous mouse position
    int32_t pmy;

public:
    CFD()
    {
        sAppName = "Fluid Simulation";
    }

public:
    bool OnUserCreate() override
    {
        fluid = new Fluid(1e-6f, 1e-8f);
        pmx = GetMouseX() / SCALE;
        pmy = GetMouseY() / SCALE;
        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // get mouse position
        int32_t mx = GetMouseX() / SCALE;
        int32_t my = GetMouseY() / SCALE;

        // add density and velocity at mouse position
        if (GetMouse(0).bHeld)
        {
            fluid->addDensity(mx, my, 500.);
        }
        if (GetMouse(1).bHeld)
        {
            fluid->addVelocity(mx, my, (float)(5*(mx - pmx)), (float)(5*(my - pmy)));
        }
        // update previous mouse position
        pmx = mx;
        pmy = my;

        fluid->step(fElapsedTime);
        fluid->renderD(*this);
        fluid->renderV(*this);

        return true;
    }
};

int
main()
{
    CFD demo;
    if (demo.Construct(N*SCALE, N*SCALE, 1, 1))
        demo.Start();
    return 0;
}
