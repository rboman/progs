#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys, glob, os

"""
def findfiles(patterns):
    for file_or_dir in glob.glob(patterns):
        if os.path.isdir(file_or_dir):
            for path, subdirs, files in os.walk(dir):
                print 'entering "%s"...' % path
                for file in files:
                    convert(file, path)
"""


def buildDB(basedir):
    basedir = os.path.abspath(basedir)
    for file_or_dir in glob.glob(basedir):
        if os.path.isdir(file_or_dir):
            for path, subdirs, files in os.walk(file_or_dir):
                print 'entering "%s"...' % path
                for file in files:
                    print "adding", os.path.join(path, file)  




if __name__ == "__main__":
    buildDB(".")
    #findfiles()
