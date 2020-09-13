// build & run
// cmake -A x64 .. && cmake --build . --config Release && Release\cube3d.exe

#include "cube3d.h"

bool
Cube3d::OnUserCreate()
{
    createCube(meshCube);

    // projection matrix
    float fNear = 0.1f;   // z_near (for scaling z to -1,1)
    float fFar = 1000.0f; // z_far (for scaling z to -1,1)
    float fFov = 60.0f;   // field of view (degrees)
    float fAspectRatio = (float)ScreenHeight() / (float)ScreenWidth();
    float fFovRad = 1.0f / tanf(fFov * 0.5f / 180.0f * 3.14159f);

    matProj.m[0][0] = fAspectRatio * fFovRad;
    matProj.m[1][1] = fFovRad;
    matProj.m[2][2] = fFar / (fFar - fNear);
    matProj.m[3][2] = (-fFar * fNear) / (fFar - fNear);
    matProj.m[2][3] = 1.0f;
    //matProj.m[3][3] = 0.0f;

    return true;
}

bool
Cube3d::OnUserUpdate(float fElapsedTime)
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

        // rotate in Z-Axis
        MultiplyMatrixVector(tri.p[0], triRotatedZ.p[0], matRotZ);
        MultiplyMatrixVector(tri.p[1], triRotatedZ.p[1], matRotZ);
        MultiplyMatrixVector(tri.p[2], triRotatedZ.p[2], matRotZ);

        // rotate in X-Axis
        MultiplyMatrixVector(triRotatedZ.p[0], triRotatedZX.p[0], matRotX);
        MultiplyMatrixVector(triRotatedZ.p[1], triRotatedZX.p[1], matRotX);
        MultiplyMatrixVector(triRotatedZ.p[2], triRotatedZX.p[2], matRotX);

        // translate
        triTranslated = triRotatedZX;
        triTranslated.p[0].z = triRotatedZ.p[0].z + 3.0f;
        triTranslated.p[1].z = triRotatedZ.p[1].z + 3.0f;
        triTranslated.p[2].z = triRotatedZ.p[2].z + 3.0f;

        // project from 3D to 2D  = > x,y,z in [-1, 1]
        MultiplyMatrixVector(triTranslated.p[0], triProjected.p[0], matProj);
        MultiplyMatrixVector(triTranslated.p[1], triProjected.p[1], matProj);
        MultiplyMatrixVector(triTranslated.p[2], triProjected.p[2], matProj);

        // scale (to screen coordinates)
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


        // draw triangle
        DrawTriangle((int32_t)triProjected.p[0].x, (int32_t)triProjected.p[0].y,
                     (int32_t)triProjected.p[1].x, (int32_t)triProjected.p[1].y,
                     (int32_t)triProjected.p[2].x, (int32_t)triProjected.p[2].y);
    }

    return true;
}
