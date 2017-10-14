#! /usr/bin/env python
# -*- coding: latin-1 -*-

# Time client program
from socket import *
s = socket(AF_INET,SOCK_STREAM) # Create TCP socket
s.connect(("garfield.ltas.ulg.ac.be",8888)) # Connect to server
tm = s.recv(1024) # Receive up to 1024 bytes
s.close() # Close connection
print "The time is", tm

raw_input()

