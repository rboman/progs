# -*- coding: utf-8 -*-

from builtins import object
class Point(object):
    def __init__(self, num, x, y):
        self.num = num
        self.x = x
        self.y = y
    def __str__(self):
        return "point #%d (%f,%f)" % (self.num, self.x, self.y)
    def __add__(self, p2):
        return Point(0, self.x+p2.x, self.y+p2.y)


