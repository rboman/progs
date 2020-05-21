#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function
import os
import sys
import subprocess
import platform


def build(binfolder, options=''):
    cwd = os.getcwd()
    # create build dir
    if not os.path.isdir(binfolder):
        os.mkdir(binfolder)
    os.chdir(binfolder)
    # cmake
    cmd = ['cmake']
    if 'Windows' in platform.uname():
        cmd.extend(['-A', 'x64'])
    cmd.extend(options)
    cmd.append('..')
    print(cmd)
    subprocess.call(cmd)

    iop = subprocess.call(['cmake', '--build', '.', '--config', 'Release'])
    if iop != 0:
        raise Exception("cmake FAILED!")
    # go back to where we were
    os.chdir(cwd)

if __name__=="__main__":
    print('using Python', sys.version)
    build('build-py2', ['-DUSE_PY3=OFF'])
    build('build-py3', ['-DUSE_PY3=ON'])

