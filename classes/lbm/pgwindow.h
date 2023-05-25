#ifndef PGWINDOW_H
#define PGWINDOW_H

#include "olcPixelGameEngine.h"
#include "lbm.h"

class PGWindow : public olc::PixelGameEngine
{
    LBM &lbm;                       ///< numerical method
    const int32_t charHeight = 9;   ///< font height
    int nt = 0;                     ///< time step number
    double vmin = 0.0;
    double vmax = 1.0;

public:
    PGWindow(LBM &_lbm);
    bool OnUserCreate() override;
    bool OnUserUpdate(float fElapsedTime) override;
protected:
    olc::Pixel getColour(double v) const;
};

#endif //PGWINDOW_H
