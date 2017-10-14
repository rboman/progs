#! /usr/bin/env python
# -*- coding: latin-1 -*-

import os, fnmatch

lst = os.listdir(".")
#print lst

for file in lst:
    if fnmatch.fnmatch(file,'*.ascii'):
        newname = file.replace(" ","_")
        newname = newname.replace("sens_profilage_bord_gauche","bord_gauche")
        print file, '->', newname
        os.rename(file,newname)


print "fini"
raw_input()

