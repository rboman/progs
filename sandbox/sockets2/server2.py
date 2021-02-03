#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# Time server program
from socket import *
import time

s = socket(AF_INET, SOCK_STREAM)  # Create TCP socket
s.bind(("", 8888))  # Bind to port 8888
s.listen(5)  # Start listening
while 1:
    client, addr = s.accept()  # Wait for a connection
    print("Got a connection from ", addr)
    client.send(time.ctime(time.time()))  # Send time back
    client.close()
