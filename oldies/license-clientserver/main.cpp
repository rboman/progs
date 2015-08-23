
#include "LicServer.h"
#include <QApplication>
#include <QPushButton>
#include <iostream>

int main(int argc, char *argv[])
{
    std::cout << "LicServer...\n";
    
    QApplication app(argc, argv);

    LicServer server;
    if(!server.listen(QHostAddress::Any, 12000))
    {
        std::cerr << "Failed to bind to port\n";
        return 1;
    }
    else
        std::cout << "LicServer started...\n";

    QPushButton quitButton(QObject::tr("&Quit"));
    quitButton.setWindowTitle(QObject::tr("Metafor License Server"));
    QObject::connect(&quitButton, SIGNAL(clicked()), &app, SLOT(quit()));
    quitButton.show();
    return app.exec();
}

