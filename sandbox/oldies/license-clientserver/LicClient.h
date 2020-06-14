#ifndef LICCLIENT_H
#define LICCLIENT_H

#include <QString>
#include <QTcpSocket>

class MTLICENSE_API LicClient
{
    QString hostname;
    int port;
    QTcpSocket tcpSocket;

public:
    LicClient();
    void configure(QString const &_hostname, int _port);
};

#endif // LICCLIENT_H
