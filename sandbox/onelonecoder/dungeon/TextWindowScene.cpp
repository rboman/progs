
#include "TextWindowScene.h"
#include "TextWindow.h"

TextWindowScene::TextWindowScene()
{
}

TextWindowScene::~TextWindowScene()
{
}

void
TextWindowScene::update(olc::PixelGameEngine &pge, float fElapsedTime)
{
    // DRAW
    pge.Clear(olc::BLACK);

    // main window
    TextWindow win(pge);
    win.grid = true;
    win.clear(olc::BLUE);
    win.print("This is the main window", HJustify::RIGHT);
    win.print("(in blue)", HJustify::RIGHT);

    // create a new centered window from the main window
    TextWindow wcentre = win.subwin(3, 50, HJustify::CENTRE, VJustify::CENTRE);
    wcentre.grid = true;
    wcentre.clear(olc::RED);
    wcentre.frame();
    wcentre.print("centre", HJustify::CENTRE);
    wcentre.print("(red)", HJustify::CENTRE);

    // bottom right
    TextWindow winbr = win.subwin(10, 25, HJustify::RIGHT, VJustify::BOTTOM);
    winbr.grid = true;
    winbr.clear(olc::GREEN);    
    winbr.print("bottom right");    

    // sub-sub window
    TextWindow subwin = winbr.subwin(1, 10, HJustify::LEFT, VJustify::CENTRE);
    subwin.grid = true;
    subwin.clear(olc::YELLOW);    
    subwin.colour = olc::BLACK;    
    subwin.print("sub win", HJustify::RIGHT);

    // bottom
    TextWindow bottom = win.subwin(1, 12, HJustify::CENTRE, VJustify::BOTTOM);
    bottom.grid = true;
    bottom.clear(olc::CYAN);
    bottom.colour = olc::BLACK;    
    bottom.print("bottom", HJustify::CENTRE);    
    bottom.frame(olc::WHITE);

    // multi-line text written "by hand"
    std::string text = "This is some multi-line text\nwritten with DrawStringDecal().\n"
    "There is no spacing above the text,\n"
    "the text seems squeezed";
    pge.DrawStringDecal({0,0}, text, olc::WHITE);   
}
