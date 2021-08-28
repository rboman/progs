#ifndef TEXTWINDOW_H
#define TEXTWINDOW_H

#include "dungeon.h"
#include "olcPixelGameEngine.h"

enum class HJustify
{
    LEFT,
    RIGHT,
    CENTRE
};

enum class VJustify
{
    TOP,
    BOTTOM,
    CENTRE
};


/// This class defines a text zone with borders and linespacing
///     text characters are 8x8 images
///     by default, vertical spacing is 0
///     there is also no spacing with the border of the windows

class TextWindow
{
    olc::PixelGameEngine &pge;
    int32_t charHeight = 8; // should not be changed - font dependant
    int32_t charWidth = 8;  // should not be changed - font dependant

    int32_t border = 2;  // additional border around text
    int32_t linesep = 2; // additional spacing between lines

public:
    int32_t ox;     // origin x
    int32_t oy;     // origin y
    int32_t nbrows; // number of rows
    int32_t nbcols; // number of columns
    int32_t row;    // current row

    olc::Pixel colour = olc::WHITE; // colour used to write text

    TextWindow(olc::PixelGameEngine &_pge);

    TextWindow subwin(int32_t nrows, int32_t ncols, 
                      HJustify hjustify = HJustify::CENTRE,
                      VJustify vjustify = VJustify::CENTRE);

    void print(std::string const &text, HJustify justify = HJustify::LEFT);

    int32_t width() const { return nbcols * charWidth + 2 * border; }
    int32_t height() const
    { 
        return nbrows * charHeight + (nbrows-1)*linesep + 2 * border;
    }

    void clear(olc::Pixel c = olc::DARK_RED);
    void frame(olc::Pixel c = olc::YELLOW);
};

#endif //TEXTWINDOW_H
