#! /usr/bin/env python
# -*- coding: latin-1; -*-

progname='qtfract'

import os, sys, subprocess, platform

try:
    # create build dir
    if not os.path.isdir('build'): os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call('cmake -G "Visual Studio 11 Win64" ..', shell=True)
    else:
        subprocess.call('cmake ..', shell=True)
    subprocess.call('cmake --build . --config Release', shell=True)
    if 'Windows' in platform.uname(): os.chdir('Release')
    # run prog
    #if not os.path.isfile(progname): raise Exception("build failed!")
    subprocess.check_output(progname, shell=True)
except Exception as e:
    print e
    print "<press ENTER to quit>"; raw_input()
