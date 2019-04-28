#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import subprocess

# list of projects
progs = [
    {'path': 'apps/EHD', 'travis': True},
    {'path': 'apps/fractal/cpp', 'travis': False},
    {'path': 'apps/GenMAI', 'travis': True},
    {'path': 'apps/md5', 'travis': True},
    {'path': 'apps/minibarreTE', 'travis': True},
    {'path': 'cmake/FindGMM', 'travis': False},
    {'path': 'student/dcm1', 'travis': False},
    {'path': 'student/dcm2', 'travis': True},
    {'path': 'student/ndh', 'travis': True},
]

def getArgs():
    # parse args
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--travis", help="run only travis tests",
                        action="store_true")
    args = parser.parse_args()
    return args

def build_one(basedir, p):
    """build project 'p'
    """
    args = getArgs()
    fullpath = os.path.join(basedir, *(p['path'].split('/')))
    if(p['travis'] or not args.travis):
        print '=> running build.py in', fullpath
        os.chdir(fullpath)
        subprocess.call(['python', 'build.py'])


def build_all(basedir):
    """build everything in 'basedir'
    """
    for p in progs:
        build_one(basedir, p)


def rm_builds(basedir):
    """remove all 'build' directories in 'basedir'
    """
    import shutil
    for path, subdirs, files in os.walk(basedir):
        for name in subdirs:
            if name == 'build':
                fullname = os.path.join(path, name)
                print 'removing', fullname
                shutil.rmtree(fullname)


if __name__ == "__main__":
    basedir = os.path.abspath(os.path.dirname(__file__))
    rm_builds(basedir)
    build_all(basedir)
