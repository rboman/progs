#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# Time client program

from __future__ import print_function
from builtins import input
from socket import *
s = socket(AF_INET, SOCK_STREAM)  # Create TCP socket
s.connect(("garfield.ltas.ulg.ac.be", 8888))  # Connect to server
tm = s.recv(1024)  # Receive up to 1024 bytes
s.close()  # Close connection
print("The time is", tm)

input()
