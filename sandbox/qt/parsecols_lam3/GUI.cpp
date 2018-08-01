#include <QApplication>
#include "GUI.h"


int main(int argc, char *argv[])
{
	QApplication *app = new QApplication(argc, argv);
	GUI *win = new GUI();
	win->show();
	app->exec();
	return 0;
}
