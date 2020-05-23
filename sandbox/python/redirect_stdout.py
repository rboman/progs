#! /usr/bin/env python3
# -*- coding: utf-8 -*-


import sys


class StdoutCatcher(object):
    def __init__(self):
        self.data = ''

    def write(self, stuff):
        self.data = self.data + stuff
        sys.__stdout__.write(stuff)


sys.stdout = StdoutCatcher()
# >>> print 'foo'
# >>> print 'hello world!'
# >>> sys.stderr.write(sys.stdout.data)
