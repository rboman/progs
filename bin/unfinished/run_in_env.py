#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# unifinished
#   the idea is to run a program in a controlled environnement 
#   (PYTHONPATH, PATH ... )

import sys, os

print('sys.path=')
for d in sys.path:
    print(d)

print(f"PYTHONPATH={os.environ['PYTHONPATH']}")

# newpath = sys.path
# newpath = [d for d in newpath if not 'gmsh' in d ]
# sys.path = newpath

print('sys.path=')
for d in sys.path:
    print(d)

import gmsh
print('version =',gmsh.GMSH_API_VERSION)