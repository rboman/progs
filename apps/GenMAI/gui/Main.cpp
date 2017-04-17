#include "Global.h"
#include <qapplication.h>
#include "mywidgeti.h"

/**
 * @brief Stupid main : start the Qt app
 */

int main( int argc, char ** argv ) 
{
    QApplication a( argc, argv );
    MyWidgetI * mw = new MyWidgetI();
    mw->setCaption( "GenMAI - by RoBo" );
    mw->show();
    a.connect( &a, SIGNAL(lastWindowClosed()), &a, SLOT(quit()) );

    return a.exec();
}
