#! /usr/bin/env python
# -*- coding: latin-1; -*-


import sys

class StdoutCatcher:
    def __init__(self):
           self.data = ''
      def write(self, stuff):
           self.data = self.data + stuff
           sys.__stdout__.write(stuff)
  
sys.stdout = StdoutCatcher()
#>>> print 'foo'
#>>> print 'hello world!'
#>>> sys.stderr.write(sys.stdout.data)
