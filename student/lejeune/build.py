#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from __future__ import print_function
def addroot():
    import sys, os.path
    print("__file__=", __file__)
    rootdir = os.path.abspath(os.path.join(os.path.split(__file__)[0],'..','..'))
    sys.path.append(rootdir)
    print('%s added to PATH' % rootdir)    

if __name__=="__main__":
    addroot()
    import pytools.build as b
    b.build()
    #b.test()
