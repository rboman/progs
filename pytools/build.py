#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import subprocess
import platform


def build():
    # create build dir
    if not os.path.isdir('build'):
        os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call(['cmake', '-A', 'x64', '..'])
    else:
        subprocess.call(['cmake', '..'])
    iop = subprocess.call(['cmake', '--build', '.', '--config', 'Release'])
    if iop!=0:
        raise Exception("cmake FAILED!")
    # go back to where we were
    os.chdir('..')

def test():
    os.chdir('build')    
    iop = subprocess.call(['ctest', '--verbose', '-C', 'Release'])
    if iop!=0:
        raise Exception("ctest FAILED!")
    os.chdir('..')

def run(progname):
    os.chdir('build')
    if 'Windows' in platform.uname(): 
        os.chdir('Release')
    print 'running', progname
    subprocess.check_output([progname])
    os.chdir('..')
    