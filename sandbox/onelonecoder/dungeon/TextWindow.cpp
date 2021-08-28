#include "TextWindow.h"

/// builds the largest text zone from the whole screen

TextWindow::TextWindow(olc::PixelGameEngine &_pge) : pge(_pge)
{
    ox = 0;
    oy = 0;
    nbcols = (pge.ScreenWidth() - 2 * border) / charWidth;
    nbrows =
        (pge.ScreenHeight() - 2 * border + linesep) / (charHeight + linesep);
    row = 0;
}

/// build a new text zone from the current one
/// prescribing number of rows, columns and relative position

TextWindow
TextWindow::subwin(int32_t nrows, int32_t ncols, HJustify hjustify,
                   VJustify vjustify)
{
    TextWindow win(*this);
    win.nbrows = std::min(nbrows,nrows);
    win.nbcols = std::min(nbcols,ncols);
    win.row = 0;

    win.ox = ox;

    if (hjustify == HJustify::RIGHT)
        win.ox = ox + width() - win.width();
    if (hjustify == HJustify::CENTRE)
        win.ox = ox + (width() - win.width()) / 2;

    win.oy = oy;

    if (vjustify == VJustify::BOTTOM)
        win.oy = oy + height() - win.height();
    if (vjustify == VJustify::CENTRE)
        win.oy = oy + (height() - win.height()) / 2;

    return win;
}

void
TextWindow::print(std::string const &text, HJustify justify)
{
    int len = text.length();

    int col = 0;

    if (justify == HJustify::RIGHT)
        col = nbcols - len;
    if (justify == HJustify::CENTRE)
        col = (nbcols - len) / 2;

    // draw string as sprite (below decals)
    // pge.DrawString(col * charWidth + 1,
    //                 row * charHeight + 1,
    //                 text, colour);
    // draw string as decal
    pge.DrawStringDecal({(float)(ox + (col)*charWidth + border),
                         (float)(oy + (row) * (charHeight + linesep) + border)},
                        text, colour);
    row++;
}

void
TextWindow::clear(olc::Pixel c)
{
    pge.FillRect(
        {ox, oy},
        {nbcols * charWidth + 2 * border,
         nbrows * charHeight + (nbrows - 1) * linesep + 2 * border},
        c);

    // draw character grid (debug)
    uint8_t v = c.r > c.g ? (c.r > c.b ? c.r : c.b) : (c.g > c.b ? c.g : c.b);
    olc::Pixel gridcol = olc::Pixel(255,255,255,30);
    if(v>128)
        gridcol = olc::Pixel(0,0,0,30);

    pge.SetPixelMode(olc::Pixel::ALPHA);
    for (int i = 0; i < nbrows; ++i)
        for (int j = 0; j < nbcols; ++j)
        {
            if ((i + j) % 2)
                pge.FillRect(ox + border + j * charWidth, 
                oy + border + i * charHeight + i * linesep, 
                charWidth, charHeight, gridcol);
        }    
    pge.SetPixelMode(olc::Pixel::NORMAL);

}

/// draw a frame
void
TextWindow::frame(olc::Pixel c)
{
    pge.DrawRect(
        {ox, oy},
        {nbcols * charWidth + 2 * border - 1,
         nbrows * charHeight + (nbrows - 1) * linesep + 2 * border - 1},
        c);

    // "-1" because 
    //      pge.DrawRect({0,0}, {pge.ScreenWidth()-1, pge.ScreenHeight()-1}, olc::RED);
    // makes a border of the whole screen, but
    //      pge.FillRect({0,0}, {pge.ScreenWidth(), pge.ScreenHeight()}, olc::RED);
    // fills the screen!  
}