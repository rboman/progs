#! /usr/bin/env python
# -*- coding: latin-1 -*-

def execpyfile(fname, searchdir):
   
    if os.path.isfile(os.path.abspath(fname)):
        testname = os.path.abspath(fname)
    elif os.path.isfile(os.path.join(searchdir, fname)):
        testname = os.path.join(searchdir, fname)
    if not testname:
        raise Exception("file not found: %s" % fname)
    
    #setupwdir(testname)
    
    # split streams
    #tee = Tee('stdout.txt')
       
    # start test
    import time, platform
    print '-'*79
    print "starting test", testname
    print "time:", time.strftime("%c")
    print "hostname:", platform.node()
    print '-'*79
    
    env = globals()
    env['__file__'] = testname

    execfile(testname, env)

if __name__=="__main__":    
    import sys, os, os.path

    # adds ".." to the pythonpath
    thisdir = os.path.split(__file__)[0]
    parentdir = os.path.abspath(os.path.join(thisdir,'..'))
    print "adding '%s' to PYTHONPATH" % parentdir
    sys.path.append(parentdir)

    scriptdir = os.path.join(thisdir,'scripts')


    # reads args
    import pytools.utils as pyu
    args = pyu.parseargs()
    print args

    for testname in args.file:
        (root, ext) = os.path.splitext(testname)
        if ext.lower()=='.py':
            execpyfile(testname, scriptdir)
        else:
            print "I don't know what to do with '%s'" % testname
