#! /usr/bin/env python
# -*- coding: utf-8 -*-

prognames = ['sky_app', 'gauss_app']

import os
import sys
import subprocess
import platform

try:
    # create build dir
    if not os.path.isdir('build'):
        os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call(['cmake', '-A', 'x64', '..'])
    else:
        subprocess.call(['cmake', '..'])
    subprocess.call(['cmake', '--build', '.', '--config', 'Release'])
    if 'Windows' in platform.uname():
        os.chdir('bin/Release')
    # run progs
    for p in prognames:
        subprocess.check_call(os.path.join('.', p), shell=True)
        print "<press ENTER to continue>"
        raw_input()
except Exception as e:
    print e
    print "<press ENTER to quit>"
    raw_input()
