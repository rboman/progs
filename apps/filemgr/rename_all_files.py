#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
from builtins import input
import os
import fnmatch

lst = os.listdir(".")
# print lst

for file in lst:
    if fnmatch.fnmatch(file, '*.ascii'):
        newname = file.replace(" ", "_")
        newname = newname.replace("sens_profilage_bord_gauche", "bord_gauche")
        print(file, '->', newname)
        os.rename(file, newname)


print("fini")
input()
