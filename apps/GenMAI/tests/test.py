#!/usr/bin/env python
# -*- coding: latin-1 -*-
#
#   Copyright 2003-2017 Romain Boman
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
