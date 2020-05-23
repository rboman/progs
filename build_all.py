#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import subprocess
import sys

# list of projects
progs = [
    {'path': 'apps/EHD', 'travis': True},
    {'path': 'apps/fractal/cpp', 'travis': False},
    {'path': 'apps/GenMAI', 'travis': True},
    {'path': 'apps/md5', 'travis': True},
    {'path': 'apps/minibarreTE', 'travis': True},
    {'path': 'sandbox/cpp11', 'travis': True},
    {'path': 'sandbox/exporttpl', 'travis': True},
    {'path': 'sandbox/singleton', 'travis': True},
    {'path': 'metafor/arbre', 'travis': True},
    {'path': 'metafor/drmeta', 'travis': True},
    {'path': 'metafor/mailsph', 'travis': False},
    {'path': 'sandbox/fortran', 'travis': True},
    {'path': 'sandbox/fortranc', 'travis': True},
    {'path': 'skel/cpp', 'travis': True},
    {'path': 'skel/fortran', 'travis': True},
    {'path': 'skel/staticlib', 'travis': True},
    {'path': 'skel/cmake/findGMM', 'travis': True},
    {'path': 'skel/cmake/findMKL', 'travis': False},
    {'path': 'student/dcm1', 'travis': False},
    {'path': 'student/dcm2', 'travis': True},
    {'path': 'student/lejeune', 'travis': True},
    {'path': 'student/mico', 'travis': False},
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

def build_all(basedir):
    """build everything in 'basedir'
    """
    failed = []
    for p in progs:
        args = getArgs()
        fullpath = os.path.join(basedir, *(p['path'].split('/')))
        if(p['travis'] or not args.travis):
            print('\n'+('-'*80))
            print('=> running build.py in', fullpath)
            print('-'*80)
            os.chdir(fullpath)
            iop = subprocess.call([sys.executable, 'build.py'])
            if iop != 0:
                print(f"{fullpath} FAILED!")
                failed.append(fullpath)

    if len(failed)!=0:
        print('\nLIST OF FAILED BUILDS:')
        for f in failed:
            print(f'\t- {f}')
        print()
        raise Exception(f'{len(failed)} builds failed!')


def rm_builds(basedir):
    """remove all 'build' directories in 'basedir'
    """
    import shutil
    for path, subdirs, files in os.walk(basedir):
        for name in subdirs:
            if name == 'build':
                fullname = os.path.join(path, name)
                print('removing', fullname)
                shutil.rmtree(fullname)


if __name__ == "__main__":
    print('using', sys.executable, sys.version)
    if sys.version_info.major<3:
        raise Exception('please use python 3')
    basedir = os.path.abspath(os.path.dirname(__file__))
    rm_builds(basedir)
    build_all(basedir)
