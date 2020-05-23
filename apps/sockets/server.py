#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
#   Copyright 2017 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# Le serveur
from socket import *

# Variables, encore...
host = "garfield.ltas.ulg.ac.be"
port = 2074
buf = 1024
addr = (host, port)

# On fait le socket
# et bind l'addresse
UDPSock = socket(AF_INET, SOCK_DGRAM)
UDPSock.bind(addr)

# On recoit le message
while 1:
    data, addr = UDPSock.recvfrom(buf)
    if not data:
        print("Le client a quitte")
        break
    else:
        print("\nRecu: '", data, "'")

# On ferme tout ca
UDPSock.close()

print("fini!")
input()
