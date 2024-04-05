# -*- coding: utf-8 -*-
# Python utilities

import sys


class DupStream:
    def __init__(self, stream1, stream2):
        self.stream1 = stream1
        self.stream2 = stream2

    def write(self, data):
        self.stream1.write(data)
        self.stream2.write(data)

    def flush(self):
        self.stream1.flush()
        self.stream2.flush()


class Tee:
    def __init__(self, name):
        self.file = open(name, 'w')
        self.stdoutbak = sys.stdout
        self.stderrbak = sys.stderr
        sys.stdout = DupStream(sys.stdout, self.file)
        sys.stderr = DupStream(sys.stderr, self.file)

    def __del__(self):
        sys.stdout = self.stdoutbak
        sys.stderr = self.stderrbak
        self.file.close()


def setupwdir(testname):
    """
    creates a working folder for each test
    """
    import os, os.path
    dir1 = os.path.abspath(os.path.dirname(__file__) + os.sep + "..") + os.sep
    dir1 = os.path.normcase(dir1)  # C:\ and c:\ are the same
    testname = os.path.normcase(testname)
    common = os.path.commonprefix((testname, dir1))
    resdir = testname[len(common):].replace(os.sep, "_")
    resdir = os.path.splitext(resdir)[0]
    wdir = os.path.join('workspace', resdir)

    # add c++/fortran suffix
    args = parseargs()
    if args.cpp:
        wdir += "_cpp"
    else:
        wdir += "_f"

    # create the folder and change to it
    if not os.path.isdir(wdir):
        print("creating", wdir)
        os.makedirs(wdir)
    os.chdir(wdir)


def parseargs():
    """
    parses command line arguments of "run.py"
    """
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verb", help="increase output verbosity",
                        action="count", default=0)
    parser.add_argument("--nogui", help="disable any graphical output",
                        action="store_true")
    parser.add_argument("--nosave", help="disable saving results to disk",
                        action="store_true")
    parser.add_argument("--post", help="only do post-processing",
                        action="store_true")
    parser.add_argument("-k", help="nb of threads", type=int, default=1)
    parser.add_argument("--cpp", help="run c++ code instead of fortran code",
                        action="store_true")
    parser.add_argument('file', help='python file', type=str, nargs='?')
    args = parser.parse_args()
    return args
