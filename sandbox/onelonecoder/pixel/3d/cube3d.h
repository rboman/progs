// build & run
// cmake -A x64 .. && cmake --build . --config Release && Release\cube3d.exe

#ifndef CUBE3D_H
#define CUBE3D_H

#include "config.h"
//#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"
#include "utils.h"

class Cube3d : public olc::PixelGameEngine
{
    mesh meshCube;
    mat4x4 matProj;
    float fTheta = 0.0f;

public:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif //CUBE3D_H
