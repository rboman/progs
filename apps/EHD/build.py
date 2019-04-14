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
    subprocess.call(['cmake', '--build', '.', '--config', 'Release'])
    if 'Windows' in platform.uname():
        os.chdir('bin/Release')


if __name__ == "__main__":
    build()
