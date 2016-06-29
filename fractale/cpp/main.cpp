
#include <QApplication>
#include "Fract.h"


int main(int argc, char *argv[])
{
	QApplication *app = new QApplication(argc, argv);
	Fract *win = new Fract();
	win->show();
	app->exec();
	return 0;
}

