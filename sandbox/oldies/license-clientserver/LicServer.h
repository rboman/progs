#ifndef LICSERVER_H
#define LICSERVER_H

#include <QTcpServer>

class LicServer : public QTcpServer
{
    Q_OBJECT;

  public:
    LicServer(QObject *parent = 0);
};

#endif //LICSERVER_H
