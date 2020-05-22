#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
import os

print("duplicate entries in env vars:")
for var in list(os.environ.keys()):
    #print var
    parts = os.environ[var].split(":")
    #print parts
    dupes = [x for n, x in enumerate(parts) if x in parts[:n]]
    if len(dupes):
        print(" .", var, ": ", dupes)


print("\ninvalid paths/files in env vars:")
for var in list(os.environ.keys()):
    parts = os.environ[var].split(":")
    for p in parts:
        if os.sep in p:
            if not os.path.isdir(p) and not os.path.isfile(p):
                print(" .", var, ": ", p, " not found")
                
