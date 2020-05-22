#!/usr/bin/env python
# -*- coding: utf-8 -*-
# basic run script - adds bin to pythonpath, then run the script given in arg

from __future__ import print_function
from past.builtins import execfile
if __name__=="__main__": 
    import sys, os.path
    thisdir = os.path.split(__file__)[0]
    bindir = os.path.abspath(os.path.join(thisdir,'build/bin'))
    if os.path.isdir(bindir):
        sys.path.append(bindir)
        print('%s added to PATH' % bindir)
    else:
        print('%s not found' % bindir)
        sys.exit()

    if len(sys.argv)==1:
        print('missing argument.\nusage: %s file.py' % os.path.basename(sys.argv[0]))
        sys.exit()
    __file__ = os.path.abspath(sys.argv[1])
    execfile(sys.argv[1])
