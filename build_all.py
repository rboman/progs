#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import subprocess

progs = [
    {
        'path': 'apps/EHD',
        'travis': True
    },
    {
        'path': 'apps/fractal/cpp',
        'travis': False
    },
    {
        'path': 'apps/GenMAI',
        'travis': True
    },
    {
        'path': 'apps/md5',
        'travis': True
    },
    {
        'path': 'apps/minibarreTE',
        'travis': True
    },
    {
        'path': 'student/dcm1',
        'travis': False
    },
    {
        'path': 'student/dcm2',
        'travis': True
    },
    {
        'path': 'student/ndh',
        'travis': True
    }
]


def build_one(basedir, p):
    fullpath = os.path.join(basedir, *(p['path'].split('/')))
    if(p['travis']):
        print '=> running build.py in', fullpath
        os.chdir(fullpath)
        subprocess.call(['python', 'build.py'])


def build_all(basedir):
    for p in progs:
        build_one(basedir, p)


def rm_builds(basedir):
    import shutil
    for path, subdirs, files in os.walk(basedir):
        for name in subdirs:
            # print name
            if name == 'build':
                fullname = os.path.join(path, name)
                print 'removing', fullname
                shutil.rmtree(fullname)


if __name__ == "__main__":
    basedir = os.path.abspath(os.path.dirname(__file__))
    rm_builds(basedir)
    build_all(basedir)
