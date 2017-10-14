#!/usr/bin/env python
# -*- coding: latin-1 -*-
# un chouette test


from genmai import *

print "Creating a pointNumber"
pt = PtNumber(54)
print "Printing the no"
print pt.getInt()

pt = Point(1,2);
print "Px =", pt.getX();
print "Py =", pt.getY();
pt.setX(8);
print "Px =", pt.getX();

print "Pno =", pt.getNo().getInt();
pt.setNo(PtNumber(99));
print "Pno =", pt.getNo().getInt();

print "-----"
arc=Arc(1,2,3);
arc.output();
line=Line(5,8);
line.output();

print arc.carteBacon()
print line.carteBacon()
