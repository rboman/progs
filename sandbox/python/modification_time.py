#! /usr/bin/env python
# -*- coding: latin-1; -*-
# display the modification time of the modules loaded in memory

import sys,os.path

print sys.modules.items()

for txt, m in sys.modules.items():
    try:
    #print txt, m
    #if 0:
        f = m.__file__
        if(os.path.isfile(f)):
            #if(f.find("python")==-1):
            mtime = os.path.getmtime(f)
            print txt, mtime
            mods[txt]=mtime
    except:
        pass
    
    
    

