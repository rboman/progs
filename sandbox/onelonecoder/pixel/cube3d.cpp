// build & run
// cmake -A x64 .. && cmake --build . --config Release && Release\cube3d.exe

#include "config.h"
//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

struct vec3
{
    float x, y, z;
};

struct triangle
{
    vec3 p[3];
};

struct mesh
{
    std::vector<triangle> tris;
};

struct mat4x4
{
    float m[4][4] = {0.0f};
};

class Cube3d : public olc::PixelGameEngine
{
    mesh meshCube;
    mat4x4 matProj;
    float fTheta = 0.0f;

    void MultiplyMatrixVector(vec3 &i, vec3 &o, mat4x4 &m)
    {
        o.x = i.x * m.m[0][0] + i.y * m.m[1][0] + i.z * m.m[2][0] + m.m[3][0];
        o.y = i.x * m.m[0][1] + i.y * m.m[1][1] + i.z * m.m[2][1] + m.m[3][1];
        o.z = i.x * m.m[0][2] + i.y * m.m[1][2] + i.z * m.m[2][2] + m.m[3][2];
        float w = i.x * m.m[0][3] + i.y * m.m[1][3] + i.z * m.m[2][3] + m.m[3][3];

        if (w != 0.0f)
        {
            o.x /= w;
            o.y /= w;
            o.z /= w;
        }
    }

public:
    bool OnUserCreate() override
    {

        meshCube.tris = {
            // south
            {
                0.0f,
                0.0f,
                0.0f,
                0.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                0.0f,
            },
            {
                0.0f,
                0.0f,
                0.0f,
                1.0f,
                1.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
            },

            // east
            {
                1.0f,
                0.0f,
                0.0f,
                1.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                1.0f,
            },
            {
                1.0f,
                0.0f,
                0.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
                0.0f,
                1.0f,
            },

            // north
            {
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
            },
            {
                1.0f,
                0.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                0.0f,
                0.0f,
                1.0f,
            },

            // west
            {
                0.0f,
                0.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                0.0f,
                1.0f,
                0.0f,
            },
            {
                0.0f,
                0.0f,
                1.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
                0.0f,
                0.0f,
            },

            // top
            {
                0.0f,
                1.0f,
                0.0f,
                0.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
            },
            {
                0.0f,
                1.0f,
                0.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
                1.0f,
                0.0f,
            },

            // bottom
            {
                1.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
                0.0f,
            },
            {
                1.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
                0.0f,
                1.0f,
                0.0f,
                0.0f,
            },
        };

        // projection matrix
        float fNear = 0.1f;   // z_near
        float fFar = 1000.0f; // z_far
        float fFov = 90.0f;   // field of view (degrees)
        float fAspectRatio = (float)ScreenHeight() / (float)ScreenWidth();
        float fFovRad = 1.0f / tanf(fFov * 0.5f / 180.0f * 3.14159f);

        matProj.m[0][0] = fAspectRatio * fFovRad;
        matProj.m[1][1] = fFovRad;
        matProj.m[2][2] = fFar / (fFar - fNear);
        matProj.m[3][2] = (-fFar * fNear) / (fFar - fNear);
        matProj.m[2][3] = 1.0f;
        matProj.m[3][3] = 0.0f;

        return true;
    }

    bool OnUserUpdate(float fElapsedTime) override
    {
        // Erase previous frame
        Clear(olc::DARK_BLUE);

        mat4x4 matRotZ, matRotX;
        fTheta += 1.0f * fElapsedTime;

        matRotZ.m[0][0] = cosf(fTheta);
        matRotZ.m[0][1] = sinf(fTheta);
        matRotZ.m[1][0] = -sinf(fTheta);
        matRotZ.m[1][1] = cosf(fTheta);
        matRotZ.m[2][2] = 1.0f;
        matRotZ.m[3][3] = 1.0f;

        matRotX.m[0][0] = 1.0f;
        matRotX.m[1][1] = cosf(fTheta * 0.5f);
        matRotX.m[1][2] = sinf(fTheta * 0.5f);
        matRotX.m[2][1] = -sinf(fTheta * 0.5f);
        matRotX.m[2][2] = cosf(fTheta * 0.5f);
        matRotX.m[3][3] = 1.0f;


        for (auto tri : meshCube.tris)
        {
            triangle triProjected, triTranslated, triRotatedZ, triRotatedZX;

            MultiplyMatrixVector(tri.p[0], triRotatedZ.p[0], matRotZ);
            MultiplyMatrixVector(tri.p[1], triRotatedZ.p[1], matRotZ);
            MultiplyMatrixVector(tri.p[2], triRotatedZ.p[2], matRotZ);

            MultiplyMatrixVector(triRotatedZ.p[0], triRotatedZX.p[0], matRotX);
            MultiplyMatrixVector(triRotatedZ.p[1], triRotatedZX.p[1], matRotX);
            MultiplyMatrixVector(triRotatedZ.p[2], triRotatedZX.p[2], matRotX);


            triTranslated = triRotatedZX;
            triTranslated.p[0].z = triRotatedZ.p[0].z + 3.0f;
            triTranslated.p[1].z = triRotatedZ.p[1].z + 3.0f;
            triTranslated.p[2].z = triRotatedZ.p[2].z + 3.0f;

            MultiplyMatrixVector(triTranslated.p[0], triProjected.p[0], matProj);
            MultiplyMatrixVector(triTranslated.p[1], triProjected.p[1], matProj);
            MultiplyMatrixVector(triTranslated.p[2], triProjected.p[2], matProj);

            // scale
            triProjected.p[0].x += 1.0f;
            triProjected.p[0].y += 1.0f;
            triProjected.p[1].x += 1.0f;
            triProjected.p[1].y += 1.0f;
            triProjected.p[2].x += 1.0f;
            triProjected.p[2].y += 1.0f;

            triProjected.p[0].x *= 0.5f * (float)ScreenWidth();
            triProjected.p[0].y *= 0.5f * (float)ScreenHeight();
            triProjected.p[1].x *= 0.5f * (float)ScreenWidth();
            triProjected.p[1].y *= 0.5f * (float)ScreenHeight();
            triProjected.p[2].x *= 0.5f * (float)ScreenWidth();
            triProjected.p[2].y *= 0.5f * (float)ScreenHeight();

            DrawTriangle(triProjected.p[0].x, triProjected.p[0].y,
                         triProjected.p[1].x, triProjected.p[1].y,
                         triProjected.p[2].x, triProjected.p[2].y);
        }

        return true;
    }
};

int
main()
{
    Cube3d demo;
    if (demo.Construct(500, 500, 2, 2))
        demo.Start();
    return 0;
}
