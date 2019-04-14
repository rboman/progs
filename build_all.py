#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os, subprocess

progs = [
    {
        'path': 'apps/EHD',
        'travis': True
    },
    {
        'path': 'apps/fractal/cpp',
        'travis': True
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

def runprog(basedir,p):
    path = p['path']
    travis = p['travis']

    fullpath = os.path.join(basedir,*(p['path'].split('/')))
    

    if(p['travis']):
        print 'running', fullpath
        os.chdir(fullpath)
        execfile('build.py')
        #iop = subprocess.call(['python', 'build.py'])
        #print 'iop=',iop
        #raw_input()
        #execfile('build.py')
        

def main(basedir):
    
    # print 'basedir=', basedir

    for p in progs:
        runprog(basedir, p)

    # os.chdir(os.path.join(basedir, 'apps', 'EHD'))
    # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'apps', 'fractal', 'cpp'))
    # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'apps', 'GenMAI'))
    # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'apps', 'md5'))
    # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'apps', 'minibarreTE'))  # requires gmm
    # execfile('build.py')

    # # os.chdir(os.path.join(basedir,'student','dcm1'))   # requires Qt
    # # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'student', 'dcm2'))
    # execfile('build.py')

    # os.chdir(os.path.join(basedir, 'student', 'ndh'))
    # execfile('build.py')


if __name__ == "__main__":



    basedir = os.path.abspath(os.path.dirname(__file__))
    main(basedir)
