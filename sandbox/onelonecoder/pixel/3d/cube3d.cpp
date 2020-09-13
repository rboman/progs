#include "cube3d.h"
#include <algorithm>

bool
Cube3d::OnUserCreate()
{
    //createCube(meshCube);
    // meshCube.LoadFromObjectFile(CMAKE_SOURCE_DIR "/VideoShip.obj");
    // meshCube.LoadFromObjectFile(CMAKE_SOURCE_DIR "/teapot.obj");
    meshCube.LoadFromObjectFile(CMAKE_SOURCE_DIR "/axis.obj");

    // projection matrix
    float fNear = 0.1f;   // z_near (for scaling z to -1,1)
    float fFar = 1000.0f; // z_far (for scaling z to -1,1)
    float fFov = 90.0f;   // field of view (degrees)
    float fAspectRatio = (float)ScreenHeight() / (float)ScreenWidth();

    matProj = Matrix_MakeProjection(fFov, fAspectRatio, fNear, fFar);

    return true;
}

bool
Cube3d::OnUserUpdate(float fElapsedTime)
{

    if (GetKey(olc::UP).bHeld)
        vCamera.y += 8.0f * fElapsedTime;

    if (GetKey(olc::DOWN).bHeld)
        vCamera.y -= 8.0f * fElapsedTime;

    if (GetKey(olc::LEFT).bHeld)
        vCamera.x -= 8.0f * fElapsedTime;

    if (GetKey(olc::RIGHT).bHeld)
        vCamera.x += 8.0f * fElapsedTime;

    vec3 vForward = Vector_Mul(vLookDir, 8.0f * fElapsedTime);

    if (GetKey(olc::Z).bHeld)
        vCamera = Vector_Add(vCamera, vForward);
    if (GetKey(olc::S).bHeld)
        vCamera = Vector_Sub(vCamera, vForward);

    if (GetKey(olc::Q).bHeld)
        fYaw -= 2.0f * fElapsedTime;
    if (GetKey(olc::D).bHeld)
        fYaw += 2.0f * fElapsedTime;

    // Erase previous frame
    Clear(olc::DARK_BLUE);

    mat4x4 matRotZ, matRotX;
    // fTheta += 1.0f * fElapsedTime;

    // rotations
    matRotZ = Matrix_MakeRotationZ(fTheta);
    matRotX = Matrix_MakeRotationX(fTheta * 0.5f);

    mat4x4 matTrans;
    matTrans = Matrix_MakeTranslation(0.0f, 0.0f, 8.0f);

    mat4x4 matWorld;
    // matWorld = Matrix_Identity();
    matWorld = Matrix_MultiplyMatrix(matRotZ, matRotX);
    matWorld = Matrix_MultiplyMatrix(matWorld, matTrans);

    vec3 vUp = {0, 1, 0};
    vec3 vTarget = {0, 0, 1};
    mat4x4 matCameraRot = Matrix_MakeRotationY(fYaw);
    vLookDir = Matrix_MultipyVector(matCameraRot, vTarget);
    vTarget = Vector_Add(vCamera, vLookDir);

    mat4x4 matCamera = Matrix_PointAt(vCamera, vTarget, vUp);
    mat4x4 matView = Matrix_QuickInverse(matCamera);

    std::vector<triangle> vecTrianglesToRaster;

    for (auto &tri : meshCube.tris)
    {
        triangle triProjected, triTransformed, triViewed;

        triTransformed.p[0] = Matrix_MultipyVector(matWorld, tri.p[0]);
        triTransformed.p[1] = Matrix_MultipyVector(matWorld, tri.p[1]);
        triTransformed.p[2] = Matrix_MultipyVector(matWorld, tri.p[2]);

        // normal
        vec3 normal, line1, line2;
        line1 = Vector_Sub(triTransformed.p[1], triTransformed.p[0]);
        line2 = Vector_Sub(triTransformed.p[2], triTransformed.p[0]);
        normal = Vector_CrossProduct(line1, line2);
        normal = Vector_Normalise(normal);

        vec3 vCameraRay = Vector_Sub(triTransformed.p[0], vCamera);

        if (Vector_DotProduct(normal, vCameraRay) < 0.0f)
        {
            // illumination
            vec3 light_direction = {0.0f, 0.0f, -1.0f};
            float dp = std::max(0.1f, Vector_DotProduct(light_direction, normal));
            triTransformed.col = (dp > 0.1f) ? dp : 0.1f;

            // convert world space => View space

            triViewed.p[0] = Matrix_MultipyVector(matView, triTransformed.p[0]);
            triViewed.p[1] = Matrix_MultipyVector(matView, triTransformed.p[1]);
            triViewed.p[2] = Matrix_MultipyVector(matView, triTransformed.p[2]);

            // project from 3D to 2D  = > x,y,z in [-1, 1]
            triProjected.p[0] = Matrix_MultipyVector(matProj, triViewed.p[0]);
            triProjected.p[1] = Matrix_MultipyVector(matProj, triViewed.p[1]);
            triProjected.p[2] = Matrix_MultipyVector(matProj, triViewed.p[2]);
            triProjected.col = triTransformed.col;

            triProjected.p[0] = Vector_Div(triProjected.p[0], triProjected.p[0].w);
            triProjected.p[1] = Vector_Div(triProjected.p[1], triProjected.p[1].w);
            triProjected.p[2] = Vector_Div(triProjected.p[2], triProjected.p[2].w);

            // X/Y are inverted so put them back
            triProjected.p[0].x *= -1.0f;
            triProjected.p[1].x *= -1.0f;
            triProjected.p[2].x *= -1.0f;
            triProjected.p[0].y *= -1.0f;
            triProjected.p[1].y *= -1.0f;
            triProjected.p[2].y *= -1.0f;

            // offset/scale view (to screen coordinates)
            vec3 vOffsetView = {1, 1, 0};
            triProjected.p[0] = Vector_Add(triProjected.p[0], vOffsetView);
            triProjected.p[1] = Vector_Add(triProjected.p[1], vOffsetView);
            triProjected.p[2] = Vector_Add(triProjected.p[2], vOffsetView);

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

        // DrawTriangle((int32_t)triProjected.p[0].x, (int32_t)triProjected.p[0].y,
        //              (int32_t)triProjected.p[1].x, (int32_t)triProjected.p[1].y,
        //              (int32_t)triProjected.p[2].x, (int32_t)triProjected.p[2].y, olc::BLACK);
    }

    return true;
}
