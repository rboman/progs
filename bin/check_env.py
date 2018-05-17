#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os

print "duplicate entries in env vars:"
for var in os.environ.iterkeys():
    #print var
    parts = os.environ[var].split(":")
    #print parts
    dupes = [x for n, x in enumerate(parts) if x in parts[:n]]
    if len(dupes):
        print " .", var, ": ", dupes


print "\ninvalid paths/files in env vars:"
for var in os.environ.iterkeys():
    parts = os.environ[var].split(":")
    for p in parts:
        if os.sep in p:
            if not os.path.isdir(p) and not os.path.isfile(p):
                print " .", var, ": ", p, " not found"
                
