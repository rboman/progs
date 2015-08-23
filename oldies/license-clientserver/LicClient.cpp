//
// $Id$
//

#include "LicClient.h"

LicClient::LicClient() : hostname("localhost"), port(12000)
{
}

void 
LicClient::configure(QString const &_hostname, 
                     int _port)
{
    hostname=_hostname;
    port=_port;
}
