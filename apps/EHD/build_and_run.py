#! /usr/bin/env python
# -*- coding: latin-1 -*-

prognames=[ 'skytest', 'gausstest' ]

import os, sys, subprocess, platform

try:
    # create build dir
    if not os.path.isdir('build'): os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call('cmake -G "Visual Studio 14 Win64" ..', shell=True)
    else:
        subprocess.call('cmake ..', shell=True)
    subprocess.call('cmake --build . --config Release', shell=True)
    if 'Windows' in platform.uname(): os.chdir('bin/Release')
    # run progs
    for p in prognames:
        subprocess.check_call(os.path.join('.',p), shell=True)
        print "<press ENTER to continue>"; raw_input()
except Exception as e:
    print e
    print "<press ENTER to quit>"; raw_input()
