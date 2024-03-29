#! /usr/bin/env python3
# -*- coding: utf-8 -*-

progname='HistogramXYPlot'

import os, os.path, sys, subprocess, platform

try:
    # create build dir
    if not os.path.isdir('build'): os.mkdir('build')
    os.chdir('build')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call('cmake -A x64 ..', shell=True)
    else:
        subprocess.call('cmake ..', shell=True)
    subprocess.call('cmake --build . --config Release', shell=True)
    if 'Windows' in platform.uname(): os.chdir('Release')
    # run prog
    #if not os.path.isfile(progname): raise Exception("build failed!")
    inputf = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'rainbow.jpg')
    subprocess.check_output([os.path.join('.',progname), inputf], shell=True)
except Exception as e:
    print(e)
    print("<press ENTER to quit>"); input()
