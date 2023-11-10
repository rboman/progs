#!/usr/bin/env python3
# -*- coding: utf-8 -*-




import os, sys, subprocess, platform

try:
    # create build dir
    os.chdir('..')
    if not os.path.isdir('louisB'): os.mkdir('louisB')
    os.chdir('louisB')
    # cmake
    if 'Windows' in platform.uname():
        subprocess.call(r'cmake -G "Visual Studio 11 Win64" ..\louis', shell=True)
    else:
        subprocess.call('cmake -G"Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_VERSION=4.6 -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE ../louis', shell=True)
        #subprocess.call('cmake -G"Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_GENERATE_SOURCE_PROJECT=TRUE ../louis', shell=True)
    subprocess.call('cmake --build . --config Release', shell=True)
    os.chdir('../louis')
except Exception as e:
    print(e)
    print("<press ENTER to quit>"); input()
