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

# Le client
from socket import *

# Variables
host = "garfield.ltas.ulg.ac.be"
port = 2074
buf = 1024
addr = (host, port)

# On fait le socket
UDPSock = socket(AF_INET, SOCK_DGRAM)
def_msg = "Message?"
print("\n", def_msg)

# Envois du message
while (1):
    data = input('>> ')
    if not data:
        break
    else:
        if(UDPSock.sendto(data, addr)):
            print("Envois de: '", data, "' ... <ok>")

# On ferme tout
UDPSock.close()


print("fini!")
input()
