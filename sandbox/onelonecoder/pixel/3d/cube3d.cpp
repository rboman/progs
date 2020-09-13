#include "cube3d.h"
#include <algorithm>

bool
Cube3d::OnUserCreate()
{
    //createCube(meshCube);
    meshCube.LoadFromObjectFile(CMAKE_SOURCE_DIR "/VideoShip.obj");

    // projection matrix
    float fNear = 0.1f;   // z_near (for scaling z to -1,1)
    float fFar = 1000.0f; // z_far (for scaling z to -1,1)
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

bool
Cube3d::OnUserUpdate(float fElapsedTime)
{
    // Erase previous frame
    Clear(olc::DARK_BLUE);

    mat4x4 matRotZ, matRotX;
    fTheta += 1.0f * fElapsedTime;

    // rotation Z
    matRotZ.m[0][0] = cosf(fTheta);
    matRotZ.m[0][1] = sinf(fTheta);
    matRotZ.m[1][0] = -sinf(fTheta);
    matRotZ.m[1][1] = cosf(fTheta);
    matRotZ.m[2][2] = 1.0f;
    matRotZ.m[3][3] = 1.0f;

    // rotation X
    matRotX.m[0][0] = 1.0f;
    matRotX.m[1][1] = cosf(fTheta * 0.5f);
    matRotX.m[1][2] = sinf(fTheta * 0.5f);
    matRotX.m[2][1] = -sinf(fTheta * 0.5f);
    matRotX.m[2][2] = cosf(fTheta * 0.5f);
    matRotX.m[3][3] = 1.0f;

    std::vector<triangle> vecTrianglesToRaster;

    for (auto &tri : meshCube.tris)
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
        triTranslated.p[0].z = triRotatedZX.p[0].z + 8.0f;
        triTranslated.p[1].z = triRotatedZX.p[1].z + 8.0f;
        triTranslated.p[2].z = triRotatedZX.p[2].z + 8.0f;

        // normal
        vec3 normal, line1, line2;
        line1.x = triTranslated.p[1].x - triTranslated.p[0].x;
        line1.y = triTranslated.p[1].y - triTranslated.p[0].y;
        line1.z = triTranslated.p[1].z - triTranslated.p[0].z;

        line2.x = triTranslated.p[2].x - triTranslated.p[0].x;
        line2.y = triTranslated.p[2].y - triTranslated.p[0].y;
        line2.z = triTranslated.p[2].z - triTranslated.p[0].z;

        normal.x = line1.y * line2.z - line1.z * line2.y;
        normal.y = line1.z * line2.x - line1.x * line2.z;
        normal.z = line1.x * line2.y - line1.y * line2.x;

        float l = sqrtf(normal.x * normal.x + normal.y * normal.y + normal.z * normal.z);
        normal.x /= l;
        normal.y /= l;
        normal.z /= l;

        //if (normal.z < 0.0f)
        if (normal.x * (triTranslated.p[0].x - vCamera.x) +
                normal.y * (triTranslated.p[0].y - vCamera.y) +
                normal.z * (triTranslated.p[0].z - vCamera.z) <
            0.0f)
        {
            // illumination
            vec3 light_direction = {0.0f, 0.0f, -1.0f};
            float l = sqrtf(light_direction.x * light_direction.x + light_direction.y * light_direction.y + light_direction.z * light_direction.z);
            light_direction.x /= l;
            light_direction.y /= l;
            light_direction.z /= l;

            float dp = normal.x * light_direction.x + normal.y * light_direction.y + normal.z * light_direction.z;

            triTranslated.col = (dp > 0.0f) ? dp : 0.0f;

            // project from 3D to 2D  = > x,y,z in [-1, 1]
            MultiplyMatrixVector(triTranslated.p[0], triProjected.p[0], matProj);
            MultiplyMatrixVector(triTranslated.p[1], triProjected.p[1], matProj);
            MultiplyMatrixVector(triTranslated.p[2], triProjected.p[2], matProj);
            triProjected.col = triTranslated.col;

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

            // store triangles for sorting
            vecTrianglesToRaster.push_back(triProjected);
        }
    }

    // sort triangles

    std::sort(vecTrianglesToRaster.begin(), vecTrianglesToRaster.end(),
              [](triangle &t1, triangle &t2) {
                  float z1 = (t1.p[0].z + t1.p[1].z + t1.p[2].z) / 3.0f;
                  float z2 = (t2.p[0].z + t2.p[1].z + t2.p[2].z) / 3.0f;
                  return z1 > z2;
              });

    // draw triangles
    for (auto &triProjected : vecTrianglesToRaster)
    {

        FillTriangle((int32_t)triProjected.p[0].x, (int32_t)triProjected.p[0].y,
                     (int32_t)triProjected.p[1].x, (int32_t)triProjected.p[1].y,
                     (int32_t)triProjected.p[2].x, (int32_t)triProjected.p[2].y,
                     olc::PixelF(triProjected.col, triProjected.col, triProjected.col));

        DrawTriangle((int32_t)triProjected.p[0].x, (int32_t)triProjected.p[0].y,
                     (int32_t)triProjected.p[1].x, (int32_t)triProjected.p[1].y,
                     (int32_t)triProjected.p[2].x, (int32_t)triProjected.p[2].y, olc::BLACK);
    }

    return true;
}
