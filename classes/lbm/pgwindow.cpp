#include "pgwindow.h"
#include <algorithm>

PGWindow::PGWindow(LBM &_lbm) : lbm(_lbm)
{
    sAppName = "Lattice Bolzmann";
}

bool PGWindow::OnUserCreate()
{
    // Called once at the start, so create things here
    lbm.init();
    std::cout << "<SPACE> stops the simulation.\n";
    std::cout << "<R> resets the colour range.\n";
    return true;
}

/**
 * @brief get colour from a value v in the "blue-white-red" palette ('bwr')
 *        (min, max values are stored as class variables).
 */

olc::Pixel PGWindow::getColour(double v) const
{
    double rng = vmax-vmin;
    if(rng<=0.0) rng=1.0;
    double mid = (vmax+vmin)/2;
    float c = static_cast<float>(2*(v-mid)/rng);
    if(c<0)
    {
        if(c<-1.0f) c=-1.0f;
        return olc::PixelF(1 + c, 1 + c, 1); // white to blue
    }
    else
    {
        if(c>1.0f) c=1.0f;
        return olc::PixelF(1, 1 - c, 1 - c); // white to red
    }
}

bool PGWindow::OnUserUpdate(float fElapsedTime)
{
    // Erase previous frame
    Clear(olc::BLACK);

    // Handle User Input
    if (GetKey(olc::Key::SPACE).bHeld)
        return false; // quit

    nt += 1;
    //if(nt<4000)
    lbm.update();

    // select field to be displayed
    //Eigen::ArrayXXd &field = lbm.vorticity;
    Eigen::ArrayXXd &field = lbm.umag;

    // reset (vmin, vmax) if needed
    if (GetKey(olc::Key::R).bHeld || nt == 1)
    {
        vmin = field.minCoeff();
        vmax = field.maxCoeff();
        std::cout << "values in (" << vmin << ", " << vmax << ")\n";
    }

    // plot field
    for (int i = 0; i < field.rows(); ++i)
        for (int j = 0; j < field.cols(); ++j)
        {
            double v = field.coeffRef(i, j);
            olc::Pixel colour = getColour(v);
            Draw(j, i, colour);
        }

    // plot cylinder in BLACK
    for (int i = 0; i < field.rows(); ++i)
        for (int j = 0; j < field.cols(); ++j)
            if (lbm.cylinder(i, j) == 1)
                Draw(j, i, olc::PixelF(0., 0., 0.));

    // print info
    DrawString(0, 0, "min=" + std::to_string(vmin), olc::BLACK);
    DrawString(0, charHeight, "max=" + std::to_string(vmax), olc::BLACK);
    DrawString(0, 2 * charHeight, "nt=" + std::to_string(nt), olc::BLACK);
    return true;
}
