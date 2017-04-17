#!/usr/bin/env python
# -*- coding: latin-1; -*-
# basic run script - adds bin to pythonpath, then run the script given in arg

if __name__=="__main__": 
    import sys, os.path
    thisdir = os.path.split(__file__)[0]
    bindir = os.path.join(thisdir,'build/bin/Release')
    if os.path.isdir(bindir):
        sys.path.append(bindir)
        print '%s added to PATH' % bindir
    else:
        print '%s not found' % bindir
        sys.exit()

    if len(sys.argv)!=2:
        print 'missing argument.\nusage: %s file.py' % os.path.basename(sys.argv[0])
        sys.exit()
    execfile(sys.argv[1])
