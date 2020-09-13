#ifndef CUBE3D_H
#define CUBE3D_H

#include "config.h"
#include "olcPixelGameEngine.h"
#include "utils.h"

class Cube3d : public olc::PixelGameEngine
{
    mesh meshCube;
    mat4x4 matProj;
    float fTheta = 0.0f;
    vec3 vCamera;
    
public:
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
};

#endif //CUBE3D_H
