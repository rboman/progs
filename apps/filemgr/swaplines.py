#!/usr/bin/env python
# -*- coding: utf-8 -*-

fname = "test.txt"

filein = open(fname, 'r')
fileout = open(fname + ".rev", 'wt')

lines = filein.readlines()
lines.reverse()

for l in lines:
    fileout.write(l)


filein.close()
fileout.close()

print "%d lines written " % len(lines)
raw_input()
