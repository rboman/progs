# -*- coding: latin-1 -*-

def parseargs():
    """
    parses command line arguments
    """
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verb", help="increase output verbosity", action="count", default=0)       
    parser.add_argument("--nogui", help="disable any graphical output",
                        action="store_true")    
    parser.add_argument("-k", help="nb of threads", type=int, default=1)
    #parser.add_argument("-p", help="misc parameters")
    parser.add_argument('file', nargs='*', help='python file')
    args = parser.parse_args()     
    return args


def chDir(dirname):
    import os
    os.chdir(dirname)
    print "[in %s]" % os.getcwd()

def isUnix():
    import platform
    uname = platform.uname()
    return not (uname[0] == 'Windows' or uname[2] == 'Windows')

def isInstalled(name):
    """Check whether `name` is on PATH."""
    from distutils.spawn import find_executable
    return find_executable(name) is not None
