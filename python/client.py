#Le client
#On importe socket..
from socket import *

#Variables
host = "garfield.ltas.ulg.ac.be"
port = 2074
buf = 1024
addr = (host,port)

#On fait le socket
UDPSock = socket(AF_INET,SOCK_DGRAM)
def_msg = "Message?"
print "\n",def_msg

#Envois du message
while (1):
    data = raw_input('>> ')
    if not data:
        break
    else:
        if(UDPSock.sendto(data,addr)):
            print "Envois de: '",data,"' ... <ok>"

#On ferme tout
UDPSock.close() 


print "fini!"
raw_input()
