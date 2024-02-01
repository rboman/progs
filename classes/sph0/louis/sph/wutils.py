# -*- coding: utf-8 -*-

# Python utilities


def setupwdir(testname):
    """
    creates a working folder for each test
    """
    import os, os.path
    # print "__file__=",__file__
    dir1 = os.path.abspath(os.path.dirname(__file__) + os.sep + "..") + os.sep
    print("dir1=", dir1)
    print("testname=", testname)
    common = os.path.commonprefix((testname, dir1))
    # print "common=", common
    resdir = testname[len(common):].replace(os.sep, "_")
    resdir = os.path.splitext(resdir)[0]  # remove ".py"
    # print "resdir=", resdir
    wdir = os.path.join('workspace', resdir)
    if not os.path.isdir(wdir):
        print("creating", wdir)
        os.makedirs(wdir)
    os.chdir(wdir)

# ------------------------------------------------------------------------------


def parseargs():
    """
    parses command line arguments
    """
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verb", help="increase output verbosity",
                        action="count", default=0)
    parser.add_argument("--nogui", help="disable any graphical output",
                        action="store_true")
    parser.add_argument("--post", help="only do post-processing",
                        action="store_true")
    parser.add_argument("-k", help="nb of threads", type=int, default=1)
    # parser.add_argument("-p", help="misc parameters")
    parser.add_argument('file', nargs='*', help='python files')
    args = parser.parse_args()
    return args
