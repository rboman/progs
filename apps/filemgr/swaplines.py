#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function

fname = "test.txt"

filein = open(fname, 'r')
fileout = open(fname + ".rev", 'wt')

lines = filein.readlines()
lines.reverse()

for l in lines:
    fileout.write(l)


filein.close()
fileout.close()

print("%d lines written " % len(lines))

