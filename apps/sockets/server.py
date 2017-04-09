#! /usr/bin/env python
# -*- coding: latin-1; -*-

#Le serveur

#Importe socket..
from socket import *

#Variables, encore...
host = "garfield.ltas.ulg.ac.be"
port = 2074
buf = 1024
addr = (host,port)

#On fait le socket
#et bind l'addresse
UDPSock = socket(AF_INET,SOCK_DGRAM)
UDPSock.bind(addr)

#On recois le message
while 1:
    data,addr = UDPSock.recvfrom(buf)
    if not data:
        print "Le client a quitte"
        break
    else:
        print "\nRecu: '",data,"'"

#On ferme tout ca
UDPSock.close() 

print "fini!"
raw_input()
